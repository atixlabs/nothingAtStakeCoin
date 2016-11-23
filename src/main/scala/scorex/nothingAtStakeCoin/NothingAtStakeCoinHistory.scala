package scorex.nothingAtStakeCoin.history

import java.nio.ByteBuffer

import scorex.core.NodeViewComponentCompanion
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.consensus.History
import scorex.core.consensus.History.{BlockId, RollbackTo}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.nothingAtStakeCoin.consensus.NothingAtStakeCoinSyncInfo
import scorex.nothingAtStakeCoin.history.NothingAtStakeCoinHistory.{BlockIndexLength, sonsSize}
import scorex.nothingAtStakeCoin.transaction.{NothingAtStakeCoinBlock, NothingAtStakeCoinBlockCompanion, NothingAtStakeCoinTransaction}

import scala.util.{Failure, Success, Try}
import scorex.core.utils.ScorexLogging
import scorex.nothingAtStakeCoin.transaction.NothingAtStakeCoinBlock.CoinAgeLength
import scorex.nothingAtStakeCoin.transaction.account.PublicKey25519NoncedBox

case class BlockInfo(sons: sonsSize, totalCoinAge: NothingAtStakeCoinBlock.CoinAgeLength)

case class NothingAtStakeCoinHistory(blocks: Map[ByteBuffer, NothingAtStakeCoinBlock] = Map(),
                                     blocksInfo: Map[ByteBuffer, BlockInfo] = Map(),
                                     bestNChains: List[ByteBuffer] = List(),
                                     txVerifiedInBlock : Map[ByteBuffer, (ByteBuffer, BlockIndexLength)] = Map()
                                    )
  extends History[PublicKey25519Proposition,
                  NothingAtStakeCoinTransaction,
                  NothingAtStakeCoinBlock,
                  NothingAtStakeCoinSyncInfo,
                  NothingAtStakeCoinHistory] with ScorexLogging {

  override def isEmpty: Boolean = blocks.isEmpty

  override def blockById(blockId: BlockId): Option[NothingAtStakeCoinBlock] = blocks.get(ByteBuffer.wrap(blockId))

  override def append(block: NothingAtStakeCoinBlock): Try[(NothingAtStakeCoinHistory, Option[RollbackTo[NothingAtStakeCoinBlock]])] = {
    log.debug("Appending block to history")
    val uniqueTxs: Boolean = block.txs.toSet.size == block.txs.length //Check for duplicate txs
    val blockSignatureValid: Boolean =  block.generator.verify( //Check block generator matches signature
                                        block.companion.messageToSign(block),
                                        block.generationSignature)
    val blockReachedTarget: Boolean = checkStakeKernelHash(block) //Check if block reached target

    if (  uniqueTxs &&
          blockSignatureValid &&
          blockReachedTarget
    ) {
      log.debug("Append conditions met")
      /* Add tx-Block relation to txVerifiedBlock */
      val txWithBlockIndex = block.txs.zipWithIndex
      val boxesWithBlockIndex = txWithBlockIndex.flatMap(pairTxIndex => pairTxIndex._1.newBoxes.map((_, pairTxIndex._2)))
      val newTxVerifiedInBlock = boxesWithBlockIndex
        .foldLeft[Map[ByteBuffer, (ByteBuffer, BlockIndexLength)]](txVerifiedInBlock){
          case (prevTxVerifiedInBlock, (box : PublicKey25519NoncedBox, txBlockIndex : BlockIndexLength)) =>
            prevTxVerifiedInBlock + (ByteBuffer.wrap(box.id) -> (ByteBuffer.wrap(block.id), txBlockIndex))
      }

      /* Add block */
      val parentInfo = blocksInfo.get(ByteBuffer.wrap(block.parentId))
      val newBlocks = blocks + (ByteBuffer.wrap(block.id) -> block)
      val newBlockTotalCoinAge = parentInfo.getOrElse(BlockInfo(0, 0)).totalCoinAge + block.coinAge
      val (newBestN, blockIdToRemove) = updateBestN(block, newBlockTotalCoinAge)
      val newBlocksInfo = changeSons(ByteBuffer.wrap(block.parentId), 1).map(_._1).getOrElse(blocksInfo) +
                            (ByteBuffer.wrap(block.id) -> BlockInfo(0, newBlockTotalCoinAge)) //Add new block to info
      NothingAtStakeCoinHistory(newBlocks, newBlocksInfo, newBestN, newTxVerifiedInBlock) //Obtain newHistory with newInfo
        .removeBlock(blockIdToRemove) //Remove blockToRemove
    } else {
      Failure(new Exception("Block does not verify requirements"))
    }
  }

  def removeBlock(blockToRemoveId: Option[ByteBuffer]): Try[(NothingAtStakeCoinHistory, Option[RollbackTo[NothingAtStakeCoinBlock]])] = blockToRemoveId match {
    case Some(blockId) =>
      if (blocks.get(blockId).isDefined && blocks.get(ByteBuffer.wrap(blocks(blockId).parentId)).isDefined) {
        //Remove txs from txVerifiedInBlock
        val newTxVerifiedInBlock = blocks(blockToRemoveId.get).txs.zipWithIndex.flatMap(pairTxIndex => pairTxIndex._1.newBoxes.map((_, pairTxIndex._2)))
          .foldLeft[Map[ByteBuffer, (ByteBuffer, BlockIndexLength)]](txVerifiedInBlock){
            case (prevTxVerifiedInBlock, (box : PublicKey25519NoncedBox, txBlockIndex : BlockIndexLength)) =>
              prevTxVerifiedInBlock + (ByteBuffer.wrap(box.id) -> (blockToRemoveId.get, txBlockIndex))
        }

        //Remove blockToRemoveId
        val parentId = ByteBuffer.wrap(blocks(blockId).parentId)
        val historyWithoutBlock = changeSons(blockId, -1).map(_._1)
          .map[NothingAtStakeCoinHistory](newBlocksInfo => NothingAtStakeCoinHistory(blocks - blockId, newBlocksInfo, bestNChains, newTxVerifiedInBlock))
        //Remove parent if necessary
        historyWithoutBlock.map(_.changeSons(parentId, -1).get) match {
          case Success((info, parentNumSons)) if parentNumSons > 0 => Success(historyWithoutBlock.get.copy(blocksInfo=info), None)
          case Success((info, parentNumSons)) if parentNumSons == 0 => historyWithoutBlock.get.copy(blocksInfo=info).removeBlock(Some(parentId))
          case Failure(e) => Failure(e)
        }
      }
      else {
        Failure(new Exception("remove: Block to remove or parent not found"))
      }
    case None => Success((this, None))
  }

  override def applicable(block: NothingAtStakeCoinBlock): Boolean = isEmpty || blocks.get(ByteBuffer.wrap(block.parentId)).isDefined

  override def openSurfaceIds(): Seq[BlockId] = bestNChains.map(_.array())

  override def continuation(from: Seq[(ModifierTypeId, ModifierId)], size: Int): Option[Seq[NothingAtStakeCoinBlock]] = ???

  override def continuationIds(from: Seq[(ModifierTypeId, ModifierId)], size: Int): Option[Seq[(ModifierTypeId, ModifierId)]] = ???

  override def syncInfo(answer: Boolean): NothingAtStakeCoinSyncInfo =
    NothingAtStakeCoinSyncInfo(answer, bestNChains.map(_.array()))

  override def compare(other: NothingAtStakeCoinSyncInfo): _root_.scorex.core.consensus.History.HistoryComparisonResult.Value = ???

  override type NVCT = this.type

  override def companion: NodeViewComponentCompanion = null

  /* Auxiliary functions */
  private def changeSons(blockId: ByteBuffer, amountToAdd: sonsSize): Try[(Map[ByteBuffer, BlockInfo], sonsSize)] = {
    blocksInfo.get(blockId) match {
      case Some(info) =>
        if (info.sons + amountToAdd > 0) {
          val newSonsNum = info.sons + amountToAdd
          Success(blocksInfo - blockId + (blockId -> BlockInfo(newSonsNum, info.totalCoinAge)), newSonsNum)
        } else
          Success((blocksInfo - blockId, 0: sonsSize))
      case None => Failure(new Exception(s"changeSons: Block ${blockId} not found on history"))
    }
  }

  private def updateBestN(newBlock: NothingAtStakeCoinBlock, newBlockTotalCoinAge: NothingAtStakeCoinBlock.CoinAgeLength): (List[ByteBuffer], Option[ByteBuffer]) = {
    val prevBestN: List[ByteBuffer] = bestNChains
    val newBlockId = ByteBuffer.wrap(newBlock.id)
    val newParentId = blocks.get(ByteBuffer.wrap(newBlock.parentId)).map(_.id)
    if(newParentId.isDefined && bestNChains.contains(ByteBuffer.wrap(newParentId.get))){
      (newBlockId +: (prevBestN diff List(ByteBuffer.wrap(newParentId.get))), None)
    }else{
      if (prevBestN.size < NothingAtStakeCoinHistory.N){
        (newBlockId +: prevBestN, None)
      } else {
        val obtainTotalCoinAge: (ByteBuffer => NothingAtStakeCoinBlock.CoinAgeLength) =
          block => blocksInfo.get(block).map(_.totalCoinAge).get
        val worstBlockId = prevBestN.minBy[NothingAtStakeCoinBlock.CoinAgeLength](block => obtainTotalCoinAge(block))
        val newBestN = if (obtainTotalCoinAge(worstBlockId) < newBlockTotalCoinAge) newBlockId +: (prevBestN diff List(worstBlockId)) else prevBestN
        (newBestN, Some(worstBlockId))
      }
    }
  }

  def getCoinAge(box : PublicKey25519NoncedBox) : Try[CoinAgeLength] = {
    txVerifiedInBlock.get(ByteBuffer.wrap(box.id)) match{
      case Some((txBlockId, txBlockIndex)) =>
        val block = blocks(txBlockId)
        val tx = block.txs(txBlockIndex)
        tx.from.foldLeft[Try[CoinAgeLength]](Success(0: CoinAgeLength)) { case (prevCalculation, txFromInput) =>
          prevCalculation match {
            case Success(prevCoinAge) =>
              txVerifiedInBlock.get(ByteBuffer.wrap(txFromInput.id))
                .flatMap[NothingAtStakeCoinTransaction](pairBlockIdIndex => blocks.get(pairBlockIdIndex._1)
                .flatMap[NothingAtStakeCoinTransaction](block => Some(block.txs(pairBlockIdIndex._2)))) match {
                  case Some(txIn) =>
                    if (tx.timestamp < txIn.timestamp) Failure(new Exception("getCoinAge: tx output is used in a tx before it according to timestamps"))
                    else {
                      if (block.timestamp + NothingAtStakeCoinHistory.nStakeMinAge > tx.timestamp) Success(prevCoinAge)
                      else Success(prevCoinAge + tx.timestamp - txIn.timestamp)
                    }
                  case None => Failure(new Exception("getCoinAge: tx from tx.from was not found in history.txVerifiedInBlock"))
              }
            case Failure(e) => prevCalculation
          }
        }
      case None => Failure(new Exception("getCoinAge: tx nonce not found on history.txVerifiedInBlock"))
    }
  }

  def getCoinAge(tx : NothingAtStakeCoinTransaction) : Try[CoinAgeLength] = {
    tx.newBoxes.foldLeft[Try[CoinAgeLength]](Success(0)){ (tryPrevTotalCoinAge, box) =>
      tryPrevTotalCoinAge match {
        case Success(prevTotalCoinAge) => getCoinAge(box).map(boxCoinAge => boxCoinAge + prevTotalCoinAge)
        case Failure(e) => Failure(e)
      }
    }
  }

  def getCoinAge(block : NothingAtStakeCoinBlock) : Try[CoinAgeLength] = {
    block.txs.foldLeft[Try[CoinAgeLength]](Success(0)) { (tryPrevTotalCoinAge, tx) =>
      tryPrevTotalCoinAge match {
        case Success(prevTotalCoinAge) => getCoinAge(tx).map(txCoinAge => txCoinAge + prevTotalCoinAge)
        case Failure(e) => Failure(e)
      }
    }
  }

  def getStakeReward(box : PublicKey25519NoncedBox) : Try[Long] = getCoinAge(box).map(_ * 33 / (365 * 33 + 8) * 10000)

  private def checkStakeKernelHash(block: NothingAtStakeCoinBlock): Boolean = true
}

object NothingAtStakeCoinHistory {
  type sonsSize = Long
  type BlockIndexLength = Int

  //TODO: Temporary N value that should be obtained from config file
  val N: Int = 10

  val nStakeMinAge : Int = 1
}