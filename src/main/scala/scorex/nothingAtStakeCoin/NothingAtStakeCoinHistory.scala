package scorex.nothingAtStakeCoin.history

import java.nio.ByteBuffer

import scorex.core.NodeViewComponentCompanion
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.consensus.History
import scorex.core.consensus.History.{BlockId, RollbackTo}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.nothingAtStakeCoin.consensus.NothingAtStakeCoinSyncInfo
import scorex.nothingAtStakeCoin.history.NothingAtStakeCoinHistory.sonsSize
import scorex.nothingAtStakeCoin.state.NothingAtStakeCoinTransaction
import scorex.nothingAtStakeCoin.transaction.{NothingAtStakeCoinBlock, NothingAtStakeCoinBlockCompanion}

import scala.util.{Failure, Success, Try}
import scorex.core.utils.ScorexLogging

case class BlockInfo(sons: sonsSize, totalCoinAge: NothingAtStakeCoinBlock.CoinAgeLength)

case class NothingAtStakeCoinHistory(blocks: Map[ByteBuffer, NothingAtStakeCoinBlock] = Map(),
                                     blocksInfo: Map[ByteBuffer, BlockInfo] = Map(),
                                     bestNChains: List[ByteBuffer] = List()
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
    /* Verify the block:
     *    txs in UTXO: In charge of nodeViewHolder (MemPool)
     *    check block coin age: In charge of nodeViewHolder (MemPool)
     *    timestamp valid?
     *    check coinbase and coinstake transactions
     *    check only 2nd transaction can be coinstake
     */
    val parentIdValid: Boolean = isEmpty || blocks.get(ByteBuffer.wrap(block.parentId)).isDefined //Check if parentId is valid
    val uniqueTxs: Boolean = block.txs.toSet.size == block.txs.length //Check for duplicate txs
    val blockSignatureValid: Boolean =  block.generator.verify( //Check block generator matches signature
                                        block.companion.messageToSign(block),
                                        block.generationSignature)
    val blockReachedTarget: Boolean = checkStakeKernelHash(block) //Check if block reached target

    if (  parentIdValid &&
          uniqueTxs &&
          blockSignatureValid &&
          blockReachedTarget
    ) {
      log.debug("Append conditions met")
      /* Add block */
      val parentInfo = blocksInfo.get(ByteBuffer.wrap(block.parentId))
      val newBlocks = blocks + (ByteBuffer.wrap(block.id) -> block)
      val newBlockTotalCoinAge = parentInfo.getOrElse(BlockInfo(0, 0)).totalCoinAge + block.coinAge
      val (newBestN, blockIdToRemove) = updateBestN(block, newBlockTotalCoinAge)
      val newBlocksInfo = changeSons(ByteBuffer.wrap(block.parentId), 1).map(_._1).getOrElse(blocksInfo) +
                            (ByteBuffer.wrap(block.id) -> BlockInfo(0, newBlockTotalCoinAge)) //Add new block to info
      NothingAtStakeCoinHistory(newBlocks, newBlocksInfo, newBestN) //Obtain newHistory with newInfo
        .remove(blockIdToRemove) //Remove blockToRemove
    } else {
      Failure(new Exception("Block does not verify requirements"))
    }
  }

  def remove(blockToRemoveId: Option[ByteBuffer]): Try[(NothingAtStakeCoinHistory, Option[RollbackTo[NothingAtStakeCoinBlock]])] = blockToRemoveId match {
    case Some(blockId) =>
      if (blocks.get(blockId).isDefined && blocks.get(ByteBuffer.wrap(blocks(blockId).parentId)).isDefined) {
        //Remove blockToRemoveId
        val parentId = ByteBuffer.wrap(blocks(blockId).parentId)
        val historyWithoutBlock = changeSons(blockId, -1).map(_._1)
          .map[NothingAtStakeCoinHistory](newBlocksInfo => NothingAtStakeCoinHistory(blocks - blockId, newBlocksInfo, bestNChains))
        //Remove parent if necessary
        historyWithoutBlock.map(_.changeSons(parentId, -1).get) match {
          case Success((info, numSons)) if numSons == 0 => Success(historyWithoutBlock.get, None)
          case Success((info, numSons)) if numSons > 0 => historyWithoutBlock.get.remove(Some(parentId))
          case Failure(e) => Failure(e)
        }
      }
      else {
        Failure(new Exception("remove: Block to remove or parent not found"))
      }
    case None => Success((this, None))
  }

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
        if (info.sons > 0) {
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
      (newBlockId +: prevBestN diff List(newParentId.get), None)
    }else{
      if (prevBestN.size < NothingAtStakeCoinHistory.N) {
        (newBlockId +: prevBestN, None)
      } else {
        val obtainTotalCoinAge: (ByteBuffer => NothingAtStakeCoinBlock.CoinAgeLength) =
          block => blocksInfo.get(block).map(_.totalCoinAge).get
        val worstBlock = prevBestN.minBy[NothingAtStakeCoinBlock.CoinAgeLength](block => obtainTotalCoinAge(block))
        val newBestN = if (obtainTotalCoinAge(worstBlock) < newBlockTotalCoinAge) newBlockId +: (prevBestN diff List(worstBlock)) else prevBestN
        (newBestN, Some(worstBlock))
      }
    }
  }

  private def checkStakeKernelHash(block: NothingAtStakeCoinBlock): Boolean = true
}

object NothingAtStakeCoinHistory {
  type sonsSize = Long

  //TODO: Temporary N value that should be obtained from config file
  val N: Int = 10
}