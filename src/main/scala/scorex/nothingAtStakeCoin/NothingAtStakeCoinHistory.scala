package scorex.nothingAtStakeCoin.history

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

case class BlockInfo(sons: sonsSize, totalCoinAge: NothingAtStakeCoinBlock.CoinAgeLength)

case class NothingAtStakeCoinHistory(blocks: Map[BlockId, NothingAtStakeCoinBlock] = Map(),
                                     blocksInfo: Map[BlockId, BlockInfo] = Map(),
                                     bestNChains: List[BlockId] = List()
                                    )
  extends History[PublicKey25519Proposition,
    NothingAtStakeCoinTransaction,
    NothingAtStakeCoinBlock,
    NothingAtStakeCoinSyncInfo,
    NothingAtStakeCoinHistory] {

  override def isEmpty: Boolean = blocks.isEmpty

  override def blockById(blockId: BlockId): Option[NothingAtStakeCoinBlock] = blocks.get(blockId)

  override def append(block: NothingAtStakeCoinBlock): Try[(NothingAtStakeCoinHistory, Option[RollbackTo[NothingAtStakeCoinBlock]])] = {
    /* Verify the block:
     *    txs in UTXO: In charge of nodeViewHolder (MemPool)
     *    check block coin age: In charge of nodeViewHolder (MemPool)
     *    timestamp valid?
     *    check coinbase and coinstake transactions
     *    check only 2nd transaction can be coinstake
     */
    if (blocks.get(block.parentId).isDefined && //Check if parentId is valid
      block.txs.toSet.size == block.txs.length && //Check for duplicate txs
      block.generator.verify(//Check block generator matches signature
        NothingAtStakeCoinBlockCompanion.messageToSign(block),
        block.generationSignature) &&
      checkStakeKernelHash(block) //Check if block reached target
    ) {
      /* Add block */
      val info = blocksInfo(block.parentId) //.get(block.parentId).get
      val newBlocks = blocks + (block.id -> block)
      val newBlockTotalCoinAge = info.totalCoinAge + block.coinAge
      val (newBestN, blockIdToRemove) = updateBestN(block.id, newBlockTotalCoinAge)
      changeSons(block.parentId, 1).map(_._1) //Change parents sons in blocksInfo
        .map(_ + (block.id -> BlockInfo(0, newBlockTotalCoinAge))) //Add new block to info
        .map(NothingAtStakeCoinHistory(newBlocks, _, newBestN)) //Obtain newHistory with newInfo
        .flatMap(_.remove(blockIdToRemove)) //Remove blockToRemove
    } else {
      Failure(new Exception("Block does not verify requirements"))
    }
  }

  def remove(blockToRemoveId: Option[BlockId]): Try[(NothingAtStakeCoinHistory, Option[RollbackTo[NothingAtStakeCoinBlock]])] = blockToRemoveId match {
    case Some(blockId) =>
      if (blocks.get(blockId).isDefined && blocks.get(blocks(blockId).parentId).isDefined) {
        //Remove blockToRemoveId
        val parentId = blocks(blockId).parentId
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

  override def openSurfaceIds(): Seq[BlockId] = bestNChains

  override def continuation(from: Seq[(ModifierTypeId, ModifierId)], size: Int): Option[Seq[NothingAtStakeCoinBlock]] = ???

  override def continuationIds(from: Seq[(ModifierTypeId, ModifierId)], size: Int): Option[Seq[(ModifierTypeId, ModifierId)]] = ???

  override def syncInfo(answer: Boolean): NothingAtStakeCoinSyncInfo =
    NothingAtStakeCoinSyncInfo(answer, bestNChains)

  override def compare(other: NothingAtStakeCoinSyncInfo): _root_.scorex.core.consensus.History.HistoryComparisonResult.Value = ???

  override type NVCT = this.type

  override def companion: NodeViewComponentCompanion = ???

  /* Auxiliary functions */
  private def changeSons(blockId: BlockId, amountToAdd: sonsSize): Try[(Map[BlockId, BlockInfo], sonsSize)] = {
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

  private def updateBestN(newBlockId: BlockId, newBlockTotalCoinAge: NothingAtStakeCoinBlock.CoinAgeLength): (List[BlockId], Option[BlockId]) = {
    val prevBestN: List[BlockId] = bestNChains
    if (prevBestN.size < NothingAtStakeCoinHistory.N) {
      (newBlockId +: prevBestN, None)
    } else {
      val obtainTotalCoinAge: (BlockId => Option[NothingAtStakeCoinBlock.CoinAgeLength]) =
        block => blocksInfo.get(block).map(_.totalCoinAge)
      val worstBlock = prevBestN.minBy[NothingAtStakeCoinBlock.CoinAgeLength](block => obtainTotalCoinAge(block).get)
      val newBestN = if (obtainTotalCoinAge(worstBlock).get < newBlockTotalCoinAge) newBlockId +: (prevBestN diff List(worstBlock)) else prevBestN
      (newBestN, Some(worstBlock))
    }
  }

  private def checkStakeKernelHash(block: NothingAtStakeCoinBlock): Boolean = ???
}

object NothingAtStakeCoinHistory {
  type sonsSize = Long

  //TODO: Temporary N value that should be obtained from config file
  val N: Int = 10
}