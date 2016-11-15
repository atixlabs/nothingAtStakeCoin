package scorex.nothingAtStakeCoin

import scorex.core.{NodeViewHolder, NodeViewModifier, NodeViewModifierCompanion}
import scorex.core.NodeViewModifier.ModifierTypeId
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.mid.wallet.DefaultWallet25519
import scorex.nothingAtStakeCoin.consensus.NothingAtStakeCoinSyncInfo
import scorex.nothingAtStakeCoin.history.NothingAtStakeCoinHistory
import scorex.nothingAtStakeCoin.state.NothingAtStakeCoinTransaction
import scorex.nothingAtStakeCoin.transaction.{NothingAtStakeCoinBlock, NothingAtStakeCoinMemoryPool}

class NothingAtStakeCoinNodeViewHolder extends NodeViewHolder[PublicKey25519Proposition, NothingAtStakeCoinTransaction, NothingAtStakeCoinBlock] {
  override type SI = NothingAtStakeCoinSyncInfo
  override type HIS = NothingAtStakeCoinHistory
  override type MS = NothingAtStakeCoinMinimalState
  override type VL = DefaultWallet25519[NothingAtStakeCoinTransaction, NothingAtStakeCoinBlock]
  override type MP = NothingAtStakeCoinMemoryPool
  override val modifierCompanions: Map[ModifierTypeId, NodeViewModifierCompanion[_ <: NodeViewModifier]] = ???

  override protected def genesisState: (NothingAtStakeCoinHistory, NothingAtStakeCoinMinimalState, DefaultWallet25519[NothingAtStakeCoinTransaction, NothingAtStakeCoinBlock], NothingAtStakeCoinMemoryPool) = ???

  override def restoreState(): Option[(NothingAtStakeCoinHistory, NothingAtStakeCoinMinimalState, DefaultWallet25519[NothingAtStakeCoinTransaction, NothingAtStakeCoinBlock], NothingAtStakeCoinMemoryPool)] = ???
}
