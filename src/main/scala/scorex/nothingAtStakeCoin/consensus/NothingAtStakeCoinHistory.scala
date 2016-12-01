package scorex.nothingAtStakeCoin.consensus

import java.nio.ByteBuffer
import java.util.concurrent.atomic.AtomicLong

import scorex.core.NodeViewComponentCompanion
import scorex.core.NodeViewModifier._
import scorex.core.block.Block.{BlockId, Timestamp}
import scorex.core.consensus.History
import scorex.core.consensus.History.{HistoryComparisonResult, RollbackTo}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58
import scorex.nothingAtStakeCoin.block.NothingAtStakeCoinBlock._
import scorex.nothingAtStakeCoin.block.{NothingAtStakeCoinBlock, NothingAtStakeCoinSyncInfo}
import scorex.nothingAtStakeCoin.consensus.NothingAtStakeCoinHistory.{BlockIndexLength, TxOutputIndexLength}
import scorex.nothingAtStakeCoin.transaction.NothingAtStakeCoinTransaction.Value
import scorex.nothingAtStakeCoin.transaction.account.PublicKey25519NoncedBox
import scorex.nothingAtStakeCoin.transaction.{NothingAtStakeCoinInput, NothingAtStakeCoinOutput, NothingAtStakeCoinTransaction}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

case class HistorySettings(numberOfBestChains: Int = 10,
                           transactionsPerBlock: Int = 10,
                           stakeMinAgeInMs: Long = 2592000000L, // 30 days
                           stakeMaxAgeInMs: Long = 7776000000L) // 90 days

case class OutputBlockLocation(blockId: ByteBuffer, blockIndex: BlockIndexLength, txOutputIndex: TxOutputIndexLength)

case class BlockNodeInfo(sons: List[ByteBuffer] = List(), levelsFromRoot: Int, insertionOrder: Long)

case class NothingAtStakeCoinHistory(historySettings: HistorySettings = HistorySettings(),
                                     genesisBlockId: Option[ByteBuffer] = None,
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

  override def blockById(blockId: BlockId): Option[NothingAtStakeCoinBlock] = blockById(wrapId(blockId))

  override def append(block: NothingAtStakeCoinBlock): Try[(NothingAtStakeCoinHistory, Option[RollbackTo[NothingAtStakeCoinBlock]])] = {
    log.debug(s"Appending block ${block.idAsString()} to history")
    if (this.isEmpty) {
      val newHistory = this.copy(
        genesisBlockId = Some(wrapId(block.id)),
        blocks = Map(wrapId(block.id) -> block),
        blocksNodeInfo = Map(wrapId(block.id) -> BlockNodeInfo(
          sons = List(),
          levelsFromRoot = 0,
          insertionOrder = NothingAtStakeCoinHistory.insertionOrder.getAndIncrement())),
        bestNChains = List(wrapId(block.id)),
        outputBlockLocations = outputBlockLocationSeq(block))
      Success((newHistory, None))
    } else {
      val uniqueTxs: Boolean = block.txs.toSet.size == block.txs.length //Check for duplicate txs
      val blockSignatureValid: Boolean = block.generator.verify(//Check block generator matches signature
        block.companion.messageToSign(block),
        block.generationSignature)
      val stakeTxValid: Boolean = block.txs.nonEmpty && checkStakeTx(block.txs.head, block.generator)
      val blockTimestampValid: Boolean = block.txs.forall(_.timestamp <= block.timestamp)
      val validCoinAge: Boolean = getCoinAge(block.txs).map(_ == block.coinAge).isSuccess
      val numberOfTxPerBlockValid: Boolean = block.txs.length == historySettings.transactionsPerBlock + 1 // stake tx

      if (uniqueTxs && blockSignatureValid && stakeTxValid && blockTimestampValid && validCoinAge && numberOfTxPerBlockValid) {
        log.debug(s"Appending conditions met for block ${block.idAsString()}")

        /* Add block */
        val (newBestN, blockIdToRemove) = updateBestN(block)
        val newBlocksNodeInfo = updateBlocksNodeInfo(wrapId(block.parentId), wrapId(block.id), isAdd = true).get
        this.copy(
          blocks = blocks + (wrapId(block.id) -> block),
          blocksNodeInfo = newBlocksNodeInfo,
          bestNChains = newBestN,
          outputBlockLocations = outputBlockLocations ++ outputBlockLocationSeq(block)) //Obtain newHistory with newInfo
          .removeBlockFromHistory(blockIdToRemove) //Remove blockToRemove
      } else {
        Failure(new Exception("Block does not verify requirements"))
      }
    }
  }

  override def applicable(block: NothingAtStakeCoinBlock): Boolean = {
    val blockFound = blocks.get(wrapId(block.id)).isDefined
    val parentFound = isEmpty || blocks.get(wrapId(block.parentId)).isDefined
    val blockWillBeInserted = belongsToBestN(block.coinAge)
    !blockFound && parentFound && blockWillBeInserted
  }

  override def openSurfaceIds(): Seq[BlockId] = {
    if (bestNChains.isEmpty) Seq(NothingAtStakeCoinBlock.EmptyChainId)
    else bestNChains.map(_.array())
  }

  override def continuation(from: Seq[(ModifierTypeId, ModifierId)], size: Int): Option[Seq[NothingAtStakeCoinBlock]] =
    continuationIds(from, size) match {
      case Some(ids) => Some(ids.map(id => blockById(id._2).get))
      case None => None
    }

  override def continuationIds(from: Seq[(ModifierTypeId, ModifierId)], size: Int): Option[Seq[(ModifierTypeId, ModifierId)]] = {
    require(from.size == 1)
    require(from.head._1 == NothingAtStakeCoinBlock.ModifierTypeId)

    val found = from.flatMap(item => {
      blocksNodeInfo.get(wrapId(item._2)) match {
        case Some(blockNodeInfo) => continuationRecursive(Seq(), blockNodeInfo.sons).reverse
        case None => Seq()
      }
    })
    if (found.nonEmpty) Some(found.take(size))
    else {
      if (from.size == 1 && (from.head._2 sameElements NothingAtStakeCoinBlock.EmptyChainId))
        Some(Seq(NothingAtStakeCoinBlock.ModifierTypeId -> genesisBlockId.get.array()))
      else None
    }
  }

  override def syncInfo(answer: Boolean): NothingAtStakeCoinSyncInfo =
    NothingAtStakeCoinSyncInfo(answer, bestNChains.flatMap(id => blockById(id)).map(block => block.id -> block.coinAge))

  // TODO We are not dealing with trolling nodes that might be sending wrong coinage to us as syncInfo
  override def compare(other: NothingAtStakeCoinSyncInfo): HistoryComparisonResult.Value = {
    if (other.bestNChains.size < bestNChains.size) HistoryComparisonResult.Younger
    else compareRecursive(HistoryComparisonResult.Equal, other.bestNChains)
  }

  override type NVCT = this.type

  override def companion: NodeViewComponentCompanion = ???

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

  private def removeBlockFromHistory(blockIdToRemove: Option[ByteBuffer]): Try[(NothingAtStakeCoinHistory, Option[RollbackTo[NothingAtStakeCoinBlock]])] = {
    blockIdToRemove match {
      // In order to be able to rollback to a specific version, we need to find the lower common ancestor of the new
      // best N chains and the old bestNChain branch to be removed. Then, we will re add all of them and remove all
      // blocks that can be removed from the removed node branch
      case Some(blockId) => {
        log.debug(s"About to remove ${Base58.encode(blockId.array())} from history")
        val (commonParent, allBlockFromParent): (ByteBuffer, Seq[ByteBuffer]) = findCommonBlocksUntilParent((blockId +: bestNChains).toSet)
        // We will only remove blocks from leaf to parent that do not have any other sons, which means, they can be removed
        val blocksToRemove = nodesToRemove(Seq(), blockId, commonParent).reverse.map(toRemove => blockById(toRemove).get)
        val blocksToAdd = allBlockFromParent.foldLeft(Seq[NothingAtStakeCoinBlock]()) { (acum, blockFromParent) =>
          if (blocksToRemove.exists(tr => tr.id sameElements blockFromParent.array())) acum
          else {
            log.debug(s"Block to re add ${Base58.encode(blockFromParent.array())} to history")
            blockById(blockFromParent).get +: acum
          }
        } // we need to sort the blocks by insertion order in order to append the unspents in the same order
          .sortBy((b: NothingAtStakeCoinBlock) => blocksNodeInfo(wrapId(b.id)).insertionOrder)

        val rollbackTo = RollbackTo(to = commonParent.array(), thrown = blocksToRemove, applied = blocksToAdd)
        log.debug(s"RollbackTo ${Base58.encode(commonParent.array())} from history")
        val newHistory = blocksToRemove.foldLeft[NothingAtStakeCoinHistory](this) {
          (currHistory: NothingAtStakeCoinHistory, blockToRemove: NothingAtStakeCoinBlock) =>
            log.debug(s"Removing block ${blockToRemove.idAsString()} from history")
            //Remove txs from outputBlockLocations
            val blockToRemoveOutputs = blockToRemove.txs.flatMap(tx => tx.newBoxes.map(box => wrapId(box.id)))
            val newOutputBlockLocations = currHistory.outputBlockLocations -- blockToRemoveOutputs
            //Remove blockToRemoveId
            val toRemoveId = wrapId(blockToRemove.id)
            val newSons = currHistory.updateBlocksNodeInfo(wrapId(blockToRemove.parentId), toRemoveId, isAdd = false).get
            currHistory.copy(
              blocks = currHistory.blocks - toRemoveId,
              blocksNodeInfo = newSons,
              bestNChains = currHistory.bestNChains,
              outputBlockLocations = newOutputBlockLocations)
        }
        Success(newHistory -> Some(rollbackTo))
      }
      case None => Success(this -> None)
    }
  }

  /**
    * This method takes a number x in an interval [lower, upper] and scales it so that xScaled is proportional to x and
    * is in interval [newLower, newUpper]
    */
  private def scaleNumber(x: Long, lower: Long, upper: Long, newLower: Long, newUpper: Long): Long = {
    require(lower <= x && x <= upper)
    val proportion: Double = (x - lower).toDouble / (upper - lower)
    val xScaled: Double = proportion * (newUpper - newLower) + newLower
    xScaled.toLong
  }

  private def getCoinAge(txFrom: IndexedSeq[NothingAtStakeCoinInput], txTimestamp: Timestamp): Try[CoinAgeLength] = {
    val maybePartialCoinAge = txFrom.foldLeft[Try[BigInt]](Success(0: BigInt)) { case (prevCalculation, txFromInput) =>
      prevCalculation.flatMap{ prevCoinAge =>
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
                case tDiff if tDiff < historySettings.stakeMinAgeInMs => 0
                case tDiff if tDiff > historySettings.stakeMaxAgeInMs => NothingAtStakeCoinHistory.daysToMs * 90
                case tDiff => {
                  val scaledTimestampDiff: Long = scaleNumber(
                    x = tDiff,
                    lower = historySettings.stakeMinAgeInMs,
                    upper = historySettings.stakeMaxAgeInMs,
                    newLower = NothingAtStakeCoinHistory.daysToMs * 30,
                    newUpper = NothingAtStakeCoinHistory.daysToMs * 90)
                  scaledTimestampDiff
                }
              }
              Success(prevCoinAge + (maybeOutput.get.value: BigInt) * timestampDiff / NothingAtStakeCoinHistory.CENT)
            case None => Failure(new Exception("getCoinAge: tx from tx.from was not found in history.outputBlockLocations"))
          }
      }
    }
    maybePartialCoinAge.map(partialCoinAge => {
      val coinAge: BigInt = partialCoinAge * NothingAtStakeCoinHistory.CENT / NothingAtStakeCoinHistory.COIN / NothingAtStakeCoinHistory.daysToMs
      if (coinAge > Long.MaxValue) Long.MaxValue else coinAge.toLong
    })

  }

  private def updateBlocksNodeInfo(parentId: ByteBuffer, sonId: ByteBuffer, isAdd: Boolean): Try[Map[ByteBuffer, BlockNodeInfo]] = {
    blocksNodeInfo.get(parentId) match {
      case Some(blockNodeInfo: BlockNodeInfo) =>
        if (isAdd) {
          Success(
            blocksNodeInfo +
              (parentId -> blockNodeInfo.copy(sons = sonId +: blockNodeInfo.sons)) +
              (sonId -> BlockNodeInfo(
                levelsFromRoot = blockNodeInfo.levelsFromRoot + 1,
                insertionOrder = NothingAtStakeCoinHistory.insertionOrder.getAndIncrement()
              ))
          )
        } else {
          val newSons = blockNodeInfo.sons.filter(son => son != sonId)
          val newBlocksSons = blocksNodeInfo - sonId // if its a removal, clear removed sons info
          Success(newBlocksSons + (parentId -> blockNodeInfo.copy(sons = newSons)))
        }
      case None => Failure(new Exception(s"updateBlocksNodeInfo: Block ${Base58.encode(parentId.array())} not found on history"))
    }
  }

  @tailrec
  private def compareRecursive(comparisonResult: HistoryComparisonResult.Value, otherBestNChains: List[(BlockId, CoinAgeLength)]):
  History.HistoryComparisonResult.Value = {
    // This consensus algorithm tries to find if the other node has at least a younger branch (not included in best chains)
    // in order to let the peers receive them
    (otherBestNChains.isEmpty, comparisonResult) match {
      case (true, _) => comparisonResult
      case (false, HistoryComparisonResult.Younger) => comparisonResult
      case _ => {
        val itemToCompare = otherBestNChains.head
        val maybeOurBlockNumberOfSons = blocksNodeInfo.get(wrapId(itemToCompare._1)).map(_.sons.size)
        (maybeOurBlockNumberOfSons, itemToCompare) match {
          case (Some(0), _) => compareRecursive(comparisonResult, otherBestNChains.tail)
          case (Some(_), _) => HistoryComparisonResult.Younger
          case (None, (_, coinAge: CoinAgeLength)) =>
            if (belongsToBestN(coinAge)) compareRecursive(HistoryComparisonResult.Older, otherBestNChains.tail)
            else HistoryComparisonResult.Younger
        }
      }
    }
  }

  /**
    * This method returns the block with least con age amongst the ones with highest coinAge
    */
  private def leastCoinAgeFromBestChains: NothingAtStakeCoinBlock = bestNChains.
    flatMap(id => blockById(id)).minBy[NothingAtStakeCoinBlock.CoinAgeLength](b => b.coinAge)

  private def belongsToBestN(coinAge: CoinAgeLength): Boolean =
    bestNChains.size < historySettings.numberOfBestChains || leastCoinAgeFromBestChains.coinAge < coinAge

  private def updateBestN(newBlock: NothingAtStakeCoinBlock): (List[ByteBuffer], Option[ByteBuffer]) = {
    val newBlockId = wrapId(newBlock.id)
    val newBlockParentId = wrapId(newBlock.parentId)
    blocks.get(newBlockParentId) match {
      case Some(newBlockParent: NothingAtStakeCoinBlock) if bestNChains.contains(wrapId(newBlockParent.id)) =>
        (newBlockId +: bestNChains.filterNot(b => b == newBlockParentId), None)
      case _ =>
        if (bestNChains.size < historySettings.numberOfBestChains) {
          (newBlockId +: bestNChains, None)
        } else {
          val worstBlock = leastCoinAgeFromBestChains
          val worstBlockId = wrapId(worstBlock.id)
          if (worstBlock.coinAge <= newBlock.coinAge) (newBlockId +: bestNChains.filterNot(b => b == worstBlockId), Some(wrapId(worstBlock.id)))
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
    boxesWithBlockLocation.toMap
  }

  private def checkStakeTx(stakeTx: NothingAtStakeCoinTransaction, minter: PublicKey25519Proposition): Boolean = {
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


  /**
    * This method finds all blocks until the least common ancestor and returns parentId and all the child nodes
    */
  @tailrec
  private def findCommonBlocksUntilParent(ids: Set[ByteBuffer], commonBlocks: Seq[ByteBuffer] = Seq()): (ByteBuffer, Seq[ByteBuffer]) = {
    val parents = ids.flatMap(id => blockById(id)).map(b => wrapId(b.parentId))
    if (parents.size == 1) parents.head -> (commonBlocks ++ ids) // If all items have the same parent, we have found it!
    else {
      val maxDistanceToGenesisOne: (ByteBuffer, BlockIndexLength) = ids.map(id => id -> blocksNodeInfo(id).levelsFromRoot).maxBy(_._2)
      val maxDistanceBlock = blockById(maxDistanceToGenesisOne._1).get
      val maxDistanceBlockId = wrapId(maxDistanceBlock.id)
      // Add parent, remove son and iterate again
      val nextIterationIds = ids.filter(id => id != maxDistanceBlockId) + wrapId(maxDistanceBlock.parentId)
      findCommonBlocksUntilParent(nextIterationIds, maxDistanceBlockId +: commonBlocks)
    }
  }

  /**
    * Returns all the tuple id -> blockNodeInfo between a child to a parent. It assumes both child and parent do exist
    * in the history
    */
  @tailrec
  private def nodesToRemove(prevNodesToRemove: Seq[ByteBuffer], from: ByteBuffer, to: ByteBuffer): Seq[ByteBuffer] = {
    if (from == to || blocksNodeInfo(from).sons.size > 1) prevNodesToRemove
    else nodesToRemove(from +: prevNodesToRemove, wrapId(blockById(from).get.parentId), to)
  }

  @tailrec
  private def continuationRecursive(acum: Seq[(ModifierTypeId, ModifierId)], sons: List[ByteBuffer]): Seq[(ModifierTypeId, ModifierId)] = {
    if (sons.isEmpty) acum
    else {
      blocksNodeInfo.get(sons.head) match {
        case None => acum
        case Some(headSonBlockInfo) if headSonBlockInfo.sons.nonEmpty =>
          continuationRecursive((NothingAtStakeCoinBlock.ModifierTypeId -> sons.head.array()) +: acum, headSonBlockInfo.sons ++ sons.tail)
        case Some(headSonBlockInfo) if headSonBlockInfo.sons.isEmpty =>
          continuationRecursive((NothingAtStakeCoinBlock.ModifierTypeId -> sons.head.array()) +: acum, sons.tail)
      }
    }
  }

  private def blockById(id: ByteBuffer) = blocks.get(id)

  private def wrapId(bytes: Array[Byte]): ByteBuffer = ByteBuffer.wrap(bytes)
}

object NothingAtStakeCoinHistory {
  type SonsSize = Long
  type BlockIndexLength = Int
  type TxOutputIndexLength = Int

  val CENT = 10000
  val COIN = 1000000

  val daysToMs: Long = 60 * 60 * 24 * 1000

  val insertionOrder: AtomicLong = new AtomicLong(0)
}
