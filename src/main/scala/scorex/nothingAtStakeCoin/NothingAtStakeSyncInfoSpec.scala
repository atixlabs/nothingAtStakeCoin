package scorex.nothingAtStakeCoin.consensus

import scorex.core.NodeViewModifier
import scorex.core.consensus.{BlockChain, SyncInfo}

class NothingAtStakeSyncInfoSpec (answer: Boolean, lastBlockID: NodeViewModifier.ModifierId, score: BlockChain.Score) extends SyncInfo {
  override def startingPoints: Seq[(NodeViewModifier.ModifierTypeId, NodeViewModifier.ModifierId)] = ???

  override def bytes: Array[Byte] = ???

  override def answer: Boolean = ???
}
