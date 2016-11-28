package scorex.nothingAtStakeCoin.transaction.wallet

import com.google.common.primitives.Bytes
import scorex.core.NodeViewComponentCompanion
import scorex.core.crypto.hash.DoubleCryptographicHash
import scorex.core.settings.Settings
import scorex.core.transaction.account.PublicKeyNoncedBox
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.core.transaction.wallet.{Wallet, WalletBox, WalletTransaction}
import scorex.crypto.encode.Base64
import scorex.nothingAtStakeCoin.block.NothingAtStakeCoinBlock
import scorex.nothingAtStakeCoin.transaction.NothingAtStakeCoinTransaction

import scala.util.{Success, Try}

case class NothingAtStakeCoinWallet(settings: Settings)
  extends Wallet[PublicKey25519Proposition, NothingAtStakeCoinTransaction, NothingAtStakeCoinBlock, NothingAtStakeCoinWallet] {

  type TX = NothingAtStakeCoinTransaction
  type PMOD = NothingAtStakeCoinBlock

  override type S = PrivateKey25519
  override type PI = PublicKey25519Proposition

  val password: String = settings.walletPassword
  val seed: Array[Byte] = settings.walletSeed

  private lazy val dbSecret: (PrivateKey25519, PublicKey25519Proposition) = PrivateKey25519Companion.generateKeys(DoubleCryptographicHash(Bytes.concat(Base64.decode(password), seed)))

  override def generateNewSecret(): NothingAtStakeCoinWallet = {
    NothingAtStakeCoinWallet(settings)
  }

  override def scanOffchain(tx: TX): NothingAtStakeCoinWallet = this

  override def scanOffchain(txs: Seq[TX]): NothingAtStakeCoinWallet = this

  override def historyTransactions: Seq[WalletTransaction[PublicKey25519Proposition, TX]] = ???

  override def boxes: Seq[WalletBox[PublicKey25519Proposition, _ <: PublicKeyNoncedBox[PublicKey25519Proposition]]] = ???

  override def publicKeys: Set[PublicKey25519Proposition] = Set(dbSecret._2)

  override def secrets: Set[PrivateKey25519] = Set(dbSecret._1)

  override def secretByPublicImage(publicImage: PublicKey25519Proposition): Option[PrivateKey25519] =
    if (dbSecret._2.address == publicImage.address) Some(dbSecret._1) else None

  override type NVCT = NothingAtStakeCoinWallet

  override def companion: NodeViewComponentCompanion = ??? //todo: fix

  override def scanPersistent(modifier: PMOD): NothingAtStakeCoinWallet = this

  override def rollback(to: VersionTag): Try[NothingAtStakeCoinWallet] = Success(this)
}

