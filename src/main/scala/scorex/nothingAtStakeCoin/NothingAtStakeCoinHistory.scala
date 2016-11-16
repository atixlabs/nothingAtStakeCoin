package scorex.nothingAtStakeCoin.history

import scorex.core.NodeViewComponentCompanion
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.consensus.History
import scorex.core.consensus.History.{BlockId, RollbackTo}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.nothingAtStakeCoin.consensus.NothingAtStakeCoinSyncInfo
import scorex.nothingAtStakeCoin.state.NothingAtStakeCoinTransaction
import scorex.nothingAtStakeCoin.transaction.NothingAtStakeCoinBlock

import scala.util.{Success, Try, Failure}

case class BlockInfo(sons:Long, totalCoinAge:Long)

case class NothingAtStakeCoinHistory( blocks:Map[BlockId, NothingAtStakeCoinBlock]=Map(),
                                      blocksInfo:Map[BlockId, BlockInfo]=Map()
                                    )
  extends History[PublicKey25519Proposition,
                  NothingAtStakeCoinTransaction,
                  NothingAtStakeCoinBlock,
                  NothingAtStakeCoinSyncInfo,
                  NothingAtStakeCoinHistory] {
  override def isEmpty: Boolean = ???

  override def blockById(blockId: BlockId): Option[NothingAtStakeCoinBlock] = ???

  override def append(block: NothingAtStakeCoinBlock): Try[(NothingAtStakeCoinHistory, Option[RollbackTo[NothingAtStakeCoinBlock]])] = {
    /* Verifiy the block:
     *    txs in UTXO
     *    coinAge valid
     *    timestamp valid?
     *    parentId valid (Already verified below)
     *    block generator matches signature?
     */
    /* Add block */
    blocksInfo.get(block.parentId) match{
      case Some(info:BlockInfo) =>
        {
          val newBlocks = blocks + (block.id->block)
          val newBlocksInfo = blocksInfo - block.parentId + (block.parentId->BlockInfo(info.sons + 1, info.totalCoinAge))
          Success(NothingAtStakeCoinHistory(newBlocks, newBlocksInfo), None)
        }
      case None => Failure(new Exception("Parent block not found"))
    }
  }

  override def openSurfaceIds(): Seq[BlockId] = ???

  override def continuation(from: Seq[(ModifierTypeId, ModifierId)], size: Int): Option[Seq[NothingAtStakeCoinBlock]] = ???

  override def continuationIds(from: Seq[(ModifierTypeId, ModifierId)], size: Int): Option[Seq[(ModifierTypeId, ModifierId)]] = ???

  override def syncInfo(answer: Boolean): NothingAtStakeCoinSyncInfo = ???

  override def compare(other: NothingAtStakeCoinSyncInfo): _root_.scorex.core.consensus.History.HistoryComparisonResult.Value = ???

  override type NVCT = this.type

  override def companion: NodeViewComponentCompanion = ???
}