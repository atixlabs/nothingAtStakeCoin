package scorex.nothingAtStakeCoin.consensus

import java.nio.ByteBuffer

import scorex.core.NodeViewComponentCompanion
import scorex.core.NodeViewModifier._
import scorex.core.consensus.History
import scorex.core.consensus.History.{RollbackTo, _}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.ScorexLogging
import scorex.nothingAtStakeCoin.block.NothingAtStakeCoinBlock._
import scorex.nothingAtStakeCoin.block.{NothingAtStakeCoinBlock, NothingAtStakeCoinSyncInfo}
import scorex.nothingAtStakeCoin.consensus.NothingAtStakeCoinHistory.{BlockIndexLength, TxOutputIndexLength, sonsSize}
import scorex.nothingAtStakeCoin.transaction.{NothingAtStakeCoinOutput, NothingAtStakeCoinTransaction}
import scorex.nothingAtStakeCoin.transaction.NothingAtStakeCoinTransaction.Value
import scorex.nothingAtStakeCoin.transaction.account.PublicKey25519NoncedBox

import scala.util.{Failure, Success, Try}

case class BlockInfo(sons: sonsSize, totalCoinAge: NothingAtStakeCoinBlock.CoinAgeLength)

case class OutputBlockLocation(blockId: ByteBuffer, blockIndex: BlockIndexLength, txOutputIndex: TxOutputIndexLength)

case class NothingAtStakeCoinHistory(numberOfBestChains: Int = 10,
                                     blocks: Map[ByteBuffer, NothingAtStakeCoinBlock] = Map(),
                                     blocksInfo: Map[ByteBuffer, BlockInfo] = Map(),
                                     bestNChains: List[ByteBuffer] = List(),
                                     outputBlockLocations: Map[ByteBuffer, OutputBlockLocation] = Map()
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
    lazy val uniqueTxs: Boolean = block.txs.toSet.size == block.txs.length //Check for duplicate txs
    lazy val blockSignatureValid: Boolean = block.generator.verify(//Check block generator matches signature
      block.companion.messageToSign(block),
      block.generationSignature)
    lazy val stakeTxValid: Boolean = block.txs.nonEmpty && checkStakeTx(block.txs.head, block.generator)
    lazy val blockTimestampValid: Boolean = block.txs.forall(_.timestamp < block.timestamp)

    if (uniqueTxs &&
      blockSignatureValid &&
      stakeTxValid
    ) {
      log.debug("Append conditions met")

      /* Add block */
      val parentInfo = blocksInfo.get(ByteBuffer.wrap(block.parentId))
      val newBlocks = blocks + (ByteBuffer.wrap(block.id) -> block)
      val newBlockTotalCoinAge = parentInfo.getOrElse(BlockInfo(0, 0)).totalCoinAge + block.coinAge
      val (newBestN, blockIdToRemove) = updateBestN(block, newBlockTotalCoinAge)
      val newBlocksInfo = changeSons(ByteBuffer.wrap(block.parentId), 1).map(_._1).getOrElse(blocksInfo) +
        (ByteBuffer.wrap(block.id) -> BlockInfo(0, newBlockTotalCoinAge)) //Add new block to info
      val newTxVerifiedInBlock = outputBlockLocationSeq(block)
      NothingAtStakeCoinHistory(numberOfBestChains, newBlocks, newBlocksInfo, newBestN, newTxVerifiedInBlock) //Obtain newHistory with newInfo
        .removeBlock(blockIdToRemove) //Remove blockToRemove
    } else {
      Failure(new Exception("Block does not verify requirements"))
    }
  }

  def removeBlock(blockToRemoveId: Option[ByteBuffer]): Try[(NothingAtStakeCoinHistory, Option[RollbackTo[NothingAtStakeCoinBlock]])] = blockToRemoveId match {
    case Some(blockId) =>
      if (blocks.get(blockId).isDefined && blocks.get(ByteBuffer.wrap(blocks(blockId).parentId)).isDefined) {
        //Remove txs from txVerifiedInBlock
        val blockToRemove = blocks(blockId)
        val blockToRemoveOutputs = blockToRemove.txs.flatMap(tx => tx.newBoxes.map(box => ByteBuffer.wrap(box.id)))
        val newTxVerifiedInBlock = outputBlockLocations -- blockToRemoveOutputs

        //Remove blockToRemoveId
        val parentId = ByteBuffer.wrap(blockToRemove.parentId)
        val historyWithoutBlock = changeSons(blockId, -1).map(_._1)
          .map[NothingAtStakeCoinHistory](newBlocksInfo => NothingAtStakeCoinHistory(numberOfBestChains, blocks - blockId, newBlocksInfo, bestNChains, newTxVerifiedInBlock))
        //Remove parent if necessary
        historyWithoutBlock.map(_.changeSons(parentId, -1).get) match {
          case Success((info, parentNumSons)) if parentNumSons > 0 => Success(historyWithoutBlock.get.copy(blocksInfo = info), None)
          case Success((info, parentNumSons)) if parentNumSons == 0 => historyWithoutBlock.get.copy(blocksInfo = info).removeBlock(Some(parentId))
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

  override def compare(other: NothingAtStakeCoinSyncInfo): HistoryComparisonResult.Value = ???

  override type NVCT = this.type

  override def companion: NodeViewComponentCompanion = null

  def getCoinAge(tx: NothingAtStakeCoinTransaction): Try[CoinAgeLength] = {
    val maybeCoinAge = tx.from.foldLeft[Try[CoinAgeLength]](Success(0: CoinAgeLength)) { case (prevCalculation, txFromInput) =>
      prevCalculation match {
        case Success(prevCoinAge) =>
          val maybeBlockLocation = outputBlockLocations.get(ByteBuffer.wrap(txFromInput.id))
          val maybeBlock: Option[NothingAtStakeCoinBlock] = maybeBlockLocation.flatMap(blockLocation => blocks.get(blockLocation.blockId))
          val maybeTx: Option[NothingAtStakeCoinTransaction] = maybeBlock.flatMap(block => Try {
            block.txs(maybeBlockLocation.get.blockIndex)
          }.toOption)
          val maybeOutput: Option[NothingAtStakeCoinOutput] = maybeTx.flatMap(tx => Try {
            tx.to(maybeBlockLocation.get.blockIndex)
          }.toOption)
          maybeOutput match {
            case Some(output) if tx.timestamp < maybeTx.get.timestamp =>
              Failure(new Exception("getCoinAge: tx output is used in a tx before it according to timestamps"))
            case Some(output) if tx.timestamp >= maybeTx.get.timestamp =>
              if (maybeBlock.get.timestamp + NothingAtStakeCoinHistory.STAKE_MIN_AGE > tx.timestamp) Success(prevCoinAge)
              else {
                //Saturate the timestampDiff to STAKE_MAX_AGE days
                val timestampDiff = Seq(tx.timestamp - maybeTx.get.timestamp, NothingAtStakeCoinHistory.STAKE_MAX_AGE).min
                Success(prevCoinAge + maybeOutput.get.value * timestampDiff / NothingAtStakeCoinHistory.CENT)
              }
            case None => Failure(new Exception("getCoinAge: tx from tx.from was not found in history.txVerifiedInBlock"))
          }
        case Failure(e) => prevCalculation
      }
    }
    maybeCoinAge.map(_ * NothingAtStakeCoinHistory.CENT / NothingAtStakeCoinHistory.COIN / (24 * 60 * 60))
  }

  def getCoinAge(block: NothingAtStakeCoinBlock): Try[CoinAgeLength] = {
    block.txs.foldLeft[Try[CoinAgeLength]](Success(0)) { (tryPrevTotalCoinAge, tx) =>
      tryPrevTotalCoinAge match {
        case Success(prevTotalCoinAge) => getCoinAge(tx).map(txCoinAge => txCoinAge + prevTotalCoinAge)
        case Failure(e) => Failure(e)
      }
    }
  }

  def getStakeReward(tx: NothingAtStakeCoinTransaction): Try[Value] =
    getCoinAge(tx).map(_ * 33 / (365 * 33 + 8) * NothingAtStakeCoinHistory.CENT)

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
    blocks.get(ByteBuffer.wrap(newBlock.parentId)) match {
      case Some(newBlockParent: NothingAtStakeCoinBlock) if bestNChains.contains(ByteBuffer.wrap(newBlockParent.id)) => (newBlockId +: (prevBestN diff List(ByteBuffer.wrap(newBlockParent.id))), None)
      case _ => {
        if (prevBestN.size < numberOfBestChains) {
          (newBlockId +: prevBestN, None)
        } else {
          val obtainTotalCoinAge: (ByteBuffer => NothingAtStakeCoinBlock.CoinAgeLength) = block => blocksInfo.get(block).map(_.totalCoinAge).get
          val worstBlockId = prevBestN.minBy[NothingAtStakeCoinBlock.CoinAgeLength](block => obtainTotalCoinAge(block))
          val newBestN =
            if (obtainTotalCoinAge(worstBlockId) <= newBlockTotalCoinAge) newBlockId +: (prevBestN diff List(worstBlockId))
            else prevBestN
          (newBestN, Some(worstBlockId))
        }
      }
    }
  }

  private def outputBlockLocationSeq(block: NothingAtStakeCoinBlock): Map[ByteBuffer, OutputBlockLocation] = {
    val txWithBlockIndex: Seq[(NothingAtStakeCoinTransaction, Int)] = block.txs.zipWithIndex
    val boxesWithBlockLocation: Seq[(ByteBuffer, OutputBlockLocation)] =
      txWithBlockIndex.flatMap { case (tx: NothingAtStakeCoinTransaction, blockIndex: Int) =>
        val boxesWithTxOutputIndex: Seq[(PublicKey25519NoncedBox, TxOutputIndexLength)] = tx.newBoxes.toIndexedSeq.zipWithIndex
        boxesWithTxOutputIndex.map(pairBoxTxOutputIndex =>
          (ByteBuffer.wrap(pairBoxTxOutputIndex._1.id), OutputBlockLocation(ByteBuffer.wrap(block.id), blockIndex, pairBoxTxOutputIndex._2)))
      }
    boxesWithBlockLocation.foldLeft[Map[ByteBuffer, OutputBlockLocation]](Map()) {
      case (prevTxVerifiedInBlock, (boxId, outputLocation)) =>
        prevTxVerifiedInBlock +
          (boxId -> OutputBlockLocation(ByteBuffer.wrap(block.id), outputLocation.blockIndex, outputLocation.txOutputIndex))
    }
  }

  def checkStakeTx(stakeTx: NothingAtStakeCoinTransaction, minter: PublicKey25519Proposition): Boolean = {
    //Verify that the input and output has the same address and that it is the one from the block
    val inputFromMinter = stakeTx.from.forall(_.proposition.address == minter.address)
    val outputToMinter = stakeTx.to.forall(_.proposition.address == minter.address)

    //Input+reward==Output
    val totalInput = stakeTx.from.foldLeft[Try[Value]](Success(0)) { (maybePartialSumInput, input) =>
      maybePartialSumInput match {
        case Success(partialSumInput) =>
          outputBlockLocations.get(ByteBuffer.wrap(input.id)) match {
            case Some(OutputBlockLocation(blockId, blockIndex, txOutputIndex)) =>
              Success(partialSumInput + blocks(blockId).txs(blockIndex).to(txOutputIndex).value)
            case None => Failure(new Exception("checkStakeTx: stakeTx input not found on history"))
          }
        case Failure(e) => Failure(e)
      }
    }
    val totalOutput = stakeTx.to.map(_.value).sum
    val reward: Try[Value] = getStakeReward(stakeTx)
    val correctInputOutputQuantities = totalInput.isSuccess && reward.isSuccess && totalInput.get + reward.get == totalOutput
    inputFromMinter && outputToMinter //&& correctInputOutputQuantities
  }
}

object NothingAtStakeCoinHistory {
  type sonsSize = Long
  type BlockIndexLength = Int
  type TxOutputIndexLength = Int

  val CENT = 10000
  val COIN = 1000000

  val secondsToDays = 60 * 60 * 24
  val STAKE_MIN_AGE: Long = secondsToDays * 30
  val STAKE_MAX_AGE: Long = secondsToDays * 90
}