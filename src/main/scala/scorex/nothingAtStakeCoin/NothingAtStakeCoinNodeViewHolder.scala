package scorex.nothingAtStakeCoin

import scorex.core.NodeViewModifier.ModifierTypeId
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.{NodeViewHolder, NodeViewModifier, NodeViewModifierCompanion}
import scorex.nothingAtStakeCoin.block.{NothingAtStakeCoinBlock, NothingAtStakeCoinBlockCompanion, NothingAtStakeCoinSyncInfo}
import scorex.nothingAtStakeCoin.consensus.NothingAtStakeCoinHistory
import scorex.nothingAtStakeCoin.settings.NothingAtStakeCoinSettings
import scorex.nothingAtStakeCoin.transaction.state.NothingAtStakeCoinMinimalState
import scorex.nothingAtStakeCoin.transaction.wallet.NothingAtStakeCoinWallet
import scorex.nothingAtStakeCoin.transaction.{NothingAtStakeCoinMemoryPool, NothingAtStakeCoinTransaction}

import scala.util.Random

class NothingAtStakeCoinNodeViewHolder(settings: NothingAtStakeCoinSettings)
  extends NodeViewHolder[PublicKey25519Proposition, NothingAtStakeCoinTransaction, NothingAtStakeCoinBlock] {
  override type SI = NothingAtStakeCoinSyncInfo
  override type HIS = NothingAtStakeCoinHistory
  override type MS = NothingAtStakeCoinMinimalState
  override type VL = NothingAtStakeCoinWallet
  override type MP = NothingAtStakeCoinMemoryPool
  override val modifierCompanions: Map[ModifierTypeId, NodeViewModifierCompanion[_ <: NodeViewModifier]] =
    Map(NothingAtStakeCoinBlock.ModifierTypeId -> NothingAtStakeCoinBlockCompanion)

  override protected def genesisState: (HIS, MS, VL, MP) = {

    log.debug("Generating genesis state")
    val wallet: NothingAtStakeCoinWallet = NothingAtStakeCoinWallet(settings)

    if (settings.createGenesisBlock) {
      val genesisTxs = (1 to settings.genesisTransactions).flatMap { _ =>
        wallet.publicKeys.map { pub =>
          val priv = wallet.secretByPublicImage(pub).get
          NothingAtStakeCoinTransaction(
            priv,
            IndexedSeq(Random.nextLong()),
            IndexedSeq((pub, settings.genesisTransactionAmount)),
            0L,
            0L)
        }.toSeq
      }.take(settings.genesisTransactions)

      val pubKeyGenesisBlock = wallet.publicKeys.head

      val genesisBlock = NothingAtStakeCoinBlock(
        parentId = NothingAtStakeCoinBlock.GenesisBlockId,
        timestamp = 0,
        generatorKeys = wallet.secretByPublicImage(pubKeyGenesisBlock).get,
        coinAge = 0,
        txs = genesisTxs
      )

      val minimalState = NothingAtStakeCoinMinimalState.genesisState().applyModifier(genesisBlock).get

      val history = NothingAtStakeCoinHistory(numberOfBestChains = settings.numberOfBestChains).append(genesisBlock)

      (history.get._1, minimalState, wallet, NothingAtStakeCoinMemoryPool.emptyPool)
    } else
      (NothingAtStakeCoinHistory(numberOfBestChains = settings.numberOfBestChains),
        NothingAtStakeCoinMinimalState.genesisState(),
        wallet,
        NothingAtStakeCoinMemoryPool.emptyPool)

  }

  override def restoreState(): Option[(
    NothingAtStakeCoinHistory,
      NothingAtStakeCoinMinimalState,
      NothingAtStakeCoinWallet,
      NothingAtStakeCoinMemoryPool
    )] = None
}
