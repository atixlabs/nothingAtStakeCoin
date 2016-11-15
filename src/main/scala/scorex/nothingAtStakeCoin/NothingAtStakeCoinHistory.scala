package scorex.nothingAtStakeCoin.history

import scorex.core.NodeViewComponentCompanion
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.consensus.History
import scorex.core.consensus.History.{BlockId, RollbackTo}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.nothingAtStakeCoin.consensus.NothingAtStakeCoinSyncInfo
import scorex.nothingAtStakeCoin.state.NothingAtStakeCoinTransaction
import scorex.nothingAtStakeCoin.transaction.NothingAtStakeCoinBlock

import scala.util.Try

case class NothingAtStakeCoinHistory() extends History[
  PublicKey25519Proposition,
  NothingAtStakeCoinTransaction,
  NothingAtStakeCoinBlock,
  NothingAtStakeCoinSyncInfo,
  NothingAtStakeCoinHistory] {
  override def isEmpty: Boolean = ???

  override def blockById(blockId: BlockId): Option[NothingAtStakeCoinBlock] = ???

  override def append(block: NothingAtStakeCoinBlock): Try[(NothingAtStakeCoinHistory, Option[RollbackTo[NothingAtStakeCoinBlock]])] = ???

  override def openSurfaceIds(): Seq[BlockId] = ???

  override def continuation(from: Seq[(ModifierTypeId, ModifierId)], size: Int): Option[Seq[NothingAtStakeCoinBlock]] = ???

  override def continuationIds(from: Seq[(ModifierTypeId, ModifierId)], size: Int): Option[Seq[(ModifierTypeId, ModifierId)]] = ???

  override def syncInfo(answer: Boolean): NothingAtStakeCoinSyncInfo = ???

  override def compare(other: NothingAtStakeCoinSyncInfo): _root_.scorex.core.consensus.History.HistoryComparisonResult.Value = ???

  override type NVCT = this.type

  override def companion: NodeViewComponentCompanion = ???
}
