package scorex.nothingAtStakeCoin.consensus

import java.nio.ByteBuffer

import scorex.core.NodeViewComponentCompanion
import scorex.core.NodeViewModifier._
import scorex.core.block.Block.Timestamp
import scorex.core.consensus.History
import scorex.core.consensus.History.{HistoryComparisonResult, RollbackTo, _}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.ScorexLogging
import scorex.nothingAtStakeCoin.block.NothingAtStakeCoinBlock._
import scorex.nothingAtStakeCoin.block.{NothingAtStakeCoinBlock, NothingAtStakeCoinSyncInfo}
import scorex.nothingAtStakeCoin.consensus.NothingAtStakeCoinHistory.{BlockIndexLength, TxOutputIndexLength}
import scorex.nothingAtStakeCoin.transaction.{NothingAtStakeCoinInput, NothingAtStakeCoinOutput, NothingAtStakeCoinTransaction}
import scorex.nothingAtStakeCoin.transaction.NothingAtStakeCoinTransaction.Value
import scorex.nothingAtStakeCoin.transaction.account.PublicKey25519NoncedBox

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

case class OutputBlockLocation(blockId: ByteBuffer, blockIndex: BlockIndexLength, txOutputIndex: TxOutputIndexLength)

case class BlockNodeInfo(sons: List[ByteBuffer] = List(), levelsFromRoot: Int)

case class NothingAtStakeCoinHistory(numberOfBestChains: Int = 10,
                                     blocks: Map[ByteBuffer, NothingAtStakeCoinBlock] = Map(),
                                     blocksNodeInfo: Map[ByteBuffer, BlockNodeInfo] = Map(),
                                     bestNChains: List[ByteBuffer] = List(),
                                     outputBlockLocations: Map[ByteBuffer, OutputBlockLocation] = Map()
                                    )
  extends History[PublicKey25519Proposition,
    NothingAtStakeCoinTransaction,
    NothingAtStakeCoinBlock,
    NothingAtStakeCoinSyncInfo,
    NothingAtStakeCoinHistory] with ScorexLogging {

  override def isEmpty: Boolean = blocks.isEmpty

  def blockById(id: ByteBuffer) = blocks.get(id)

  override def blockById(blockId: BlockId): Option[NothingAtStakeCoinBlock] = blockById(wrapId(blockId))

  override def append(block: NothingAtStakeCoinBlock): Try[(NothingAtStakeCoinHistory, Option[RollbackTo[NothingAtStakeCoinBlock]])] = {
    log.debug("Appending block to history")
    this match {
      case _ if this.isEmpty =>
        val newHistory = NothingAtStakeCoinHistory(numberOfBestChains,
          Map(wrapId(block.id) -> block),
          Map(wrapId(block.id) -> BlockNodeInfo(sons = List(), 0)),
          List(wrapId(block.id)),
          outputBlockLocationSeq(block))
        Success((newHistory, None))
      case _ =>
        val uniqueTxs: Boolean = block.txs.toSet.size == block.txs.length //Check for duplicate txs
      val blockSignatureValid: Boolean = block.generator.verify(//Check block generator matches signature
        block.companion.messageToSign(block),
        block.generationSignature)
        val stakeTxValid: Boolean = block.txs.nonEmpty && checkStakeTx(block.txs.head, block.generator)
        val blockTimestampValid: Boolean = block.txs.forall(_.timestamp < block.timestamp)

        //FIXME: Check before appending a block
        val validCoinAge: Boolean = getCoinAge(block.txs).map(_ == block.coinAge).isSuccess
        val numberOfTxPerBlockValid: Boolean = block.txs.length == NothingAtStakeCoinHistory.numberOfTxsPerBlock + 1

        if (uniqueTxs &&
          blockSignatureValid &&
          stakeTxValid &&
          blockTimestampValid
        ) {
          log.debug("Append conditions met")

          /* Add block */
          val newBlocks = blocks + (wrapId(block.id) -> block)
          val (newBestN, blockIdToRemove) = updateBestN(block)
          val newBlocksSons = changeSons(wrapId(block.parentId), wrapId(block.id), isAdd = true)
            .getOrElse(blocksNodeInfo + (wrapId(block.id) -> BlockNodeInfo(levelsFromRoot = 0))) // It's genesis block
          val newOutputBlockLocations = outputBlockLocations ++ outputBlockLocationSeq(block)
          NothingAtStakeCoinHistory(numberOfBestChains, newBlocks, newBlocksSons, newBestN, newOutputBlockLocations) //Obtain newHistory with newInfo
            .removeBlock(blockIdToRemove) //Remove blockToRemove
        } else {
          Failure(new Exception("Block does not verify requirements"))
        }
    }
  }

  @tailrec
  private def removeBlock(blockToRemoveId: Option[ByteBuffer]): Try[(NothingAtStakeCoinHistory, Option[RollbackTo[NothingAtStakeCoinBlock]])] = blockToRemoveId match {
    case Some(blockId) =>
      if (blocks.get(blockId).isDefined && blocks.get(wrapId(blocks(blockId).parentId)).isDefined) {
        //Remove txs from outputBlockLocations
        val blockToRemove = blocks(blockId)
        val blockToRemoveOutputs = blockToRemove.txs.flatMap(tx => tx.newBoxes.map(box => wrapId(box.id)))
        val newOutputBlockLocations = outputBlockLocations -- blockToRemoveOutputs

        //Remove blockToRemoveId
        val parentId = wrapId(blockToRemove.parentId)
        val historyWithoutBlock = changeSons(parentId, blockId, isAdd = false)
          .map[NothingAtStakeCoinHistory](newSons => NothingAtStakeCoinHistory(
          numberOfBestChains = numberOfBestChains,
          blocks = blocks - blockId,
          blocksNodeInfo = newSons,
          bestNChains = bestNChains.filterNot(id => id == blockId),
          newOutputBlockLocations))

        //Remove parent if necessary
        historyWithoutBlock match {
          case Success(h: NothingAtStakeCoinHistory) if h.blocksNodeInfo(parentId).sons.nonEmpty => Success(h, None)
          case Success(h: NothingAtStakeCoinHistory) if h.blocksNodeInfo(parentId).sons.isEmpty => h.removeBlock(Some(parentId))
          case Failure(e) => Failure(e)
        }
      }
      else {
        Failure(new Exception("remove: Block to remove or parent not found"))
      }
    case None => Success((this, None))
  }

  override def applicable(block: NothingAtStakeCoinBlock): Boolean = {
    val blockFound = blocks.get(wrapId(block.id)).isDefined
    val parentFound = isEmpty || blocks.get(wrapId(block.parentId)).isDefined
    val blockWillBeInserted = belongsToBestN(block.coinAge)
    !blockFound && parentFound && blockWillBeInserted
  }

  override def openSurfaceIds(): Seq[BlockId] = bestNChains.map(_.array())

  override def continuation(from: Seq[(ModifierTypeId, ModifierId)], size: Int): Option[Seq[NothingAtStakeCoinBlock]] =
    continuationIds(from, size) match {
      case Some(ids) => Some(ids.map(id => blockById(id._2).get))
      case None => None
    }

  override def continuationIds(from: Seq[(ModifierTypeId, ModifierId)], size: Int): Option[Seq[(ModifierTypeId, ModifierId)]] = {
    require(from.size == 1)
    require(from.head._1 == NothingAtStakeCoinBlock.ModifierTypeId)

    @tailrec
    def continuationRecursive(acum: Seq[(ModifierTypeId, ModifierId)], sons: List[ByteBuffer], size: Int): Seq[(ModifierTypeId, ModifierId)] = {
      if (sons.isEmpty || acum.size == size) acum
      else {
        blocksNodeInfo.get(sons.head) match {
          case None => acum
          case Some(headSonBlockInfo) if headSonBlockInfo.sons.nonEmpty => continuationRecursive(acum :+ (NothingAtStakeCoinBlock.ModifierTypeId -> sons.head.array()), headSonBlockInfo.sons ++ sons.tail, size)
          case Some(headSonBlockInfo) if headSonBlockInfo.sons.isEmpty => continuationRecursive(acum :+ (NothingAtStakeCoinBlock.ModifierTypeId -> sons.head.array()), sons.tail, size)
        }
      }
    }

    val found = from.flatMap(item => {
      blocksNodeInfo.get(wrapId(item._2)) match {
        case Some(blockNodeInfo) => continuationRecursive(Seq(), blockNodeInfo.sons, size)
        case None => Seq()
      }
    })
    if (found.isEmpty) None else Some(found.take(size))
  }

  override def syncInfo(answer: Boolean): NothingAtStakeCoinSyncInfo =
    NothingAtStakeCoinSyncInfo(answer, bestNChains.flatMap(id => blockById(id)).map(block => block.id -> block.coinAge))

  // TODO We are not dealing with trolling nodes that might be sending wrong coinage to us as syncInfo
  override def compare(other: NothingAtStakeCoinSyncInfo): HistoryComparisonResult.Value = compareRecursive(HistoryComparisonResult.Equal, other.bestNChains)

  override type NVCT = this.type

  override def companion: NodeViewComponentCompanion = null

  def getCoinAge(txFrom: IndexedSeq[NothingAtStakeCoinInput], txTimestamp: Timestamp) = {
    val maybeCoinAge = txFrom.foldLeft[Try[CoinAgeLength]](Success(0: CoinAgeLength)) { case (prevCalculation, txFromInput) =>
      prevCalculation match {
        case Success(prevCoinAge) =>
          val maybeBlockLocation = outputBlockLocations.get(wrapId(txFromInput.id))
          val maybeBlock: Option[NothingAtStakeCoinBlock] = maybeBlockLocation.flatMap(blockLocation => blocks.get(blockLocation.blockId))
          val maybeTx: Option[NothingAtStakeCoinTransaction] = maybeBlock.flatMap(block => Try {
            block.txs(maybeBlockLocation.get.blockIndex)
          }.toOption)
          val maybeOutput: Option[NothingAtStakeCoinOutput] = maybeTx.flatMap(tx => Try {
            tx.to(maybeBlockLocation.get.txOutputIndex)
          }.toOption)
          maybeOutput match {
            case Some(output) if txTimestamp < maybeTx.get.timestamp =>
              Failure(new Exception("getCoinAge: tx output is used in a tx before it according to timestamps"))
            case Some(output) if txTimestamp >= maybeTx.get.timestamp =>
              val timestampDiff = txTimestamp - maybeTx.get.timestamp match {
                case tDiff if tDiff < NothingAtStakeCoinHistory.STAKE_MIN_AGE => 0
                case tDiff if tDiff > NothingAtStakeCoinHistory.STAKE_MAX_AGE => NothingAtStakeCoinHistory.STAKE_MAX_AGE
                case tDiff => tDiff
              }
              Success(prevCoinAge + maybeOutput.get.value * timestampDiff / NothingAtStakeCoinHistory.CENT)
            case None => Failure(new Exception("getCoinAge: tx from tx.from was not found in history.outputBlockLocations"))
          }
        case Failure(e) => prevCalculation
      }
    }
    maybeCoinAge.map(_ * NothingAtStakeCoinHistory.CENT / NothingAtStakeCoinHistory.COIN / (24 * 60 * 60))
  }

  def getCoinAge(tx: NothingAtStakeCoinTransaction): Try[CoinAgeLength] = getCoinAge(tx.from, tx.timestamp)

  def getCoinAge(txs: Seq[NothingAtStakeCoinTransaction]): Try[CoinAgeLength] = {
    txs.foldLeft[Try[CoinAgeLength]](Success(0)) { (tryPrevTotalCoinAge, tx) =>
      tryPrevTotalCoinAge match {
        case Success(prevTotalCoinAge) => getCoinAge(tx).map(txCoinAge => txCoinAge + prevTotalCoinAge)
        case Failure(e) => Failure(e)
      }
    }
  }

  def getStakeReward(txFrom: IndexedSeq[NothingAtStakeCoinInput], txTimestamp: Timestamp): Try[Value] =
    getCoinAge(txFrom, txTimestamp).map(_ * 33 / (365 * 33 + 8) * NothingAtStakeCoinHistory.CENT)

  /* Auxiliary functions */
  private def changeSons(parentId: ByteBuffer, sonId: ByteBuffer, isAdd: Boolean): Try[Map[ByteBuffer, BlockNodeInfo]] = {
    blocksNodeInfo.get(parentId) match {
      case Some(blockNodeInfo: BlockNodeInfo) =>
        if(isAdd) {
          Success(blocksNodeInfo + (parentId -> blockNodeInfo.copy(sons = blockNodeInfo.sons :+ sonId)) + (sonId -> BlockNodeInfo(levelsFromRoot = blockNodeInfo.levelsFromRoot + 1)))
        } else {
          val newSons = blockNodeInfo.sons.filter(son => son != sonId)
          val newBlocksSons = blocksNodeInfo - sonId // if its a removal, clear removed sons info
          Success(newBlocksSons + (parentId -> blockNodeInfo.copy(sons = newSons)))
        }
      case None => Failure(new Exception(s"changeSons: Block $parentId not found on history"))
    }
  }

  @tailrec
  private def compareRecursive(comparisonResult: HistoryComparisonResult.Value, bestNChains: List[(BlockId, CoinAgeLength)]): History.HistoryComparisonResult.Value = {
    // This consensus algorithm tries to find if the other node has at least a younger branch (not included in best chains)
    // in order to let the peers receive them
    if (bestNChains.isEmpty) return comparisonResult

    comparisonResult match {
      case HistoryComparisonResult.Younger => comparisonResult
      case (HistoryComparisonResult.Equal | HistoryComparisonResult.Older) if bestNChains.nonEmpty =>
        val itemToCompare = bestNChains.head
        val maybeOurBlockNumberOfSons = blocksNodeInfo.get(wrapId(itemToCompare._1))
        (maybeOurBlockNumberOfSons, itemToCompare) match {
          case (Some(blockNodeInfo), _) if blockNodeInfo.sons.nonEmpty => HistoryComparisonResult.Younger
          case (Some(blockNodeInfo), _) if blockNodeInfo.sons.isEmpty => compareRecursive(comparisonResult, bestNChains.tail)
          case (None, (_, coinAge: CoinAgeLength)) => if (belongsToBestN(coinAge)) compareRecursive(HistoryComparisonResult.Older, bestNChains.tail) else compareRecursive(comparisonResult, bestNChains)
        }
    }
  }

  /**
    * This method returns the block with least con age amongst the ones with highest coinAge
    */
  private def leastCoinAgeFromBestChains: NothingAtStakeCoinBlock = bestNChains.flatMap(id => blockById(id)).minBy[NothingAtStakeCoinBlock.CoinAgeLength](b => b.coinAge)

  private def belongsToBestN(coinAge: CoinAgeLength): Boolean = bestNChains.size < numberOfBestChains || leastCoinAgeFromBestChains.coinAge < coinAge

  private def updateBestN(newBlock: NothingAtStakeCoinBlock): (List[ByteBuffer], Option[ByteBuffer]) = {
    val newBlockId = wrapId(newBlock.id)
    blocks.get(wrapId(newBlock.parentId)) match {
      case Some(newBlockParent: NothingAtStakeCoinBlock) if bestNChains.contains(wrapId(newBlockParent.id)) => (newBlockId +: (bestNChains diff List(wrapId(newBlockParent.id))), None)
      case _ =>
        if (bestNChains.size < numberOfBestChains) {
          (newBlockId +: bestNChains, None)
        } else {
          val worstBlock = leastCoinAgeFromBestChains
          if (worstBlock.coinAge <= newBlock.coinAge) (newBlockId +: (bestNChains diff List(worstBlock.id)), Some(wrapId(worstBlock.id)))
          else (bestNChains, None)
        }
    }
  }

  private def outputBlockLocationSeq(block: NothingAtStakeCoinBlock): Map[ByteBuffer, OutputBlockLocation] = {
    val txWithBlockIndex: Seq[(NothingAtStakeCoinTransaction, Int)] = block.txs.zipWithIndex
    val boxesWithBlockLocation: Seq[(ByteBuffer, OutputBlockLocation)] =
      txWithBlockIndex.flatMap { case (tx: NothingAtStakeCoinTransaction, blockIndex: Int) =>
        val boxesWithTxOutputIndex: Seq[(PublicKey25519NoncedBox, TxOutputIndexLength)] = tx.newBoxes.toIndexedSeq.zipWithIndex
        boxesWithTxOutputIndex.map(pairBoxTxOutputIndex =>
          (wrapId(pairBoxTxOutputIndex._1.id), OutputBlockLocation(wrapId(block.id), blockIndex, pairBoxTxOutputIndex._2)))
      }
    boxesWithBlockLocation.foldLeft[Map[ByteBuffer, OutputBlockLocation]](Map()) {
      case (prevOutputBlockLocations, (boxId, outputLocation)) =>
        prevOutputBlockLocations + (boxId -> outputLocation)
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
          outputBlockLocations.get(wrapId(input.id)) match {
            case Some(OutputBlockLocation(blockId, blockIndex, txOutputIndex)) =>
              Success(partialSumInput + blocks(blockId).txs(blockIndex).to(txOutputIndex).value)
            case None => Failure(new Exception("checkStakeTx: stakeTx input not found on history"))
          }
        case Failure(e) => Failure(e)
      }
    }
    val totalOutput = stakeTx.to.map(_.value).sum
    val reward: Try[Value] = getStakeReward(stakeTx.from, stakeTx.timestamp)
    val correctInputOutputQuantities = totalInput.isSuccess && reward.isSuccess && totalInput.get + reward.get == totalOutput
    inputFromMinter && outputToMinter && correctInputOutputQuantities
  }

  private def wrapId(bytes: Array[Byte]): ByteBuffer = ByteBuffer.wrap(bytes)
}

object NothingAtStakeCoinHistory {
  type SonsSize = Long
  type BlockIndexLength = Int
  type TxOutputIndexLength = Int

  val CENT = 10000
  val COIN = 1000000

  val secondsToDays = 60 * 60 * 24
  val STAKE_MIN_AGE: Long = secondsToDays * 30
  val STAKE_MAX_AGE: Long = secondsToDays * 90

  //FIXME: Obtain values from settings
  val numberOfTxsPerBlock = 10
}