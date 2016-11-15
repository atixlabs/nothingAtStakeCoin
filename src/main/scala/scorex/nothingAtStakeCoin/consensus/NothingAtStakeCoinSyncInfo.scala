package scorex.nothingAtStakeCoin.consensus

import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.consensus.SyncInfo

case class NothingAtStakeCoinSyncInfo() extends SyncInfo {
  override def answer: Boolean = ???

  override def startingPoints: Seq[(ModifierTypeId, ModifierId)] = ???

  override def bytes: Array[Byte] = ???
}
