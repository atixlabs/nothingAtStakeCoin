package scorex.nothingAtStakeCoin

import scorex.core.NodeViewModifier.ModifierTypeId
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.core.{NodeViewHolder, NodeViewModifier, NodeViewModifierCompanion}
import scorex.crypto.signatures.Curve25519
import scorex.nothingAtStakeCoin.consensus.NothingAtStakeCoinSyncInfo
import scorex.nothingAtStakeCoin.history.NothingAtStakeCoinHistory
import scorex.nothingAtStakeCoin.state.NothingAtStakeCoinTransaction
import scorex.nothingAtStakeCoin.transaction.{NothingAtStakeCoinBlock, NothingAtStakeCoinBlockCompanion, NothingAtStakeCoinMemoryPool}

import scala.util.Random

class NothingAtStakeCoinNodeViewHolder(settings: Settings) extends NodeViewHolder[PublicKey25519Proposition, NothingAtStakeCoinTransaction, NothingAtStakeCoinBlock] {
  override type SI = NothingAtStakeCoinSyncInfo
  override type HIS = NothingAtStakeCoinHistory
  override type MS = NothingAtStakeCoinMinimalState
  override type VL = NothingAtStakeCoinWallet
  override type MP = NothingAtStakeCoinMemoryPool
  override val modifierCompanions: Map[ModifierTypeId, NodeViewModifierCompanion[_ <: NodeViewModifier]] =
    Map(NothingAtStakeCoinBlock.ModifierTypeId -> NothingAtStakeCoinBlockCompanion)

  override protected def genesisState: (HIS, MS, VL, MP) = {
    log.debug("Generating genesis block")

    val wallet: NothingAtStakeCoinWallet = NothingAtStakeCoinWallet(settings).generateNewSecret()

    val genesisTx = NothingAtStakeCoinTransaction(
      IndexedSeq((wallet.secrets.head, Random.nextLong())),
      IndexedSeq((wallet.publicKeys.head, 100000L)),
      0L,
      0L)

    val pubKeyGenesisBlock = wallet.publicKeys.head

    val unsignedGenesisBlock = NothingAtStakeCoinBlock(
      NothingAtStakeCoinBlock.GenesisBlockId,
      timestamp = 0,
      generationSignature = Array.fill(NothingAtStakeCoinBlock.SignatureLength)(1: Byte),
      generator = pubKeyGenesisBlock,
      Long.MaxValue,
      txs = Seq(genesisTx)
    )

    val genesisBlockSignature = PrivateKey25519Companion.sign(wallet.secretByPublicImage(pubKeyGenesisBlock).get,
                                                              unsignedGenesisBlock.companion.messageToSign(unsignedGenesisBlock))

    val genesisBlock = unsignedGenesisBlock.copy(generationSignature = genesisBlockSignature.signature, generator = pubKeyGenesisBlock)

    val minimalState = NothingAtStakeCoinMinimalState.genesisState().applyModifier(genesisBlock).get

    val history = (new NothingAtStakeCoinHistory).append(genesisBlock)

    (history.get._1, minimalState, wallet, NothingAtStakeCoinMemoryPool.emptyPool)
  }

  override def restoreState(): Option[(NothingAtStakeCoinHistory, NothingAtStakeCoinMinimalState, NothingAtStakeCoinWallet, NothingAtStakeCoinMemoryPool)] = None
}
