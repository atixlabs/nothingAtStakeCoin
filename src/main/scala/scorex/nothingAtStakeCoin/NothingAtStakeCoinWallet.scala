package scorex.nothingAtStakeCoin

import java.io.File

import com.google.common.primitives.{Bytes, Ints}
import org.mapdb.serializer.SerializerByteArray
import org.mapdb.{DBMaker, HTreeMap}
import scorex.core.NodeViewComponentCompanion
import scorex.core.crypto.hash.DoubleCryptographicHash
import scorex.core.settings.Settings
import scorex.core.transaction.account.PublicKeyNoncedBox
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.core.transaction.wallet.{Wallet, WalletBox, WalletTransaction}
import scorex.nothingAtStakeCoin.state.NothingAtStakeCoinTransaction
import scorex.nothingAtStakeCoin.transaction.NothingAtStakeCoinBlock

import scala.collection.JavaConversions._


case class NothingAtStakeCoinWallet(settings: Settings)
  extends Wallet[PublicKey25519Proposition, NothingAtStakeCoinTransaction, NothingAtStakeCoinBlock, NothingAtStakeCoinWallet] {

  type TX = NothingAtStakeCoinTransaction
  type PMOD = NothingAtStakeCoinBlock

  override type S = PrivateKey25519
  override type PI = PublicKey25519Proposition

  val walletFileOpt: Option[File] = settings.walletDirOpt.map(walletDir => new java.io.File(walletDir, "wallet.dat"))
  val password: String = settings.walletPassword
  val seed: Array[Byte] = settings.walletSeed

  val db = DBMaker
    .fileDB("wallet.dat")
    .make()

  private val dbSeed = db.atomicString("seed").createOrOpen()

  private def lastNonce = db.atomicInteger("nonce").createOrOpen()

  private lazy val dbSecrets: HTreeMap[Array[Byte], Array[Byte]] =
    db.hashMap("secrets", new SerializerByteArray, new SerializerByteArray).createOrOpen()

  override def generateNewSecret(): NothingAtStakeCoinWallet = {
    val nonce = lastNonce.incrementAndGet()
    val randomSeed = DoubleCryptographicHash(Bytes.concat(Ints.toByteArray(nonce), seed))
    val (priv, pub) = PrivateKey25519Companion.generateKeys(randomSeed)

    dbSecrets.put(pub.pubKeyBytes, priv.privKeyBytes)
    db.commit()
    db.close()
    NothingAtStakeCoinWallet(settings)
  }

  override def scanOffchain(tx: TX): NothingAtStakeCoinWallet = ???

  override def scanOffchain(txs: Seq[TX]): NothingAtStakeCoinWallet = ???

  override def historyTransactions: Seq[WalletTransaction[PublicKey25519Proposition, TX]] = ???

  override def boxes: Seq[WalletBox[PublicKey25519Proposition, _ <: PublicKeyNoncedBox[PublicKey25519Proposition]]] = ???

  override def publicKeys: Set[PublicKey25519Proposition] =
    dbSecrets.getKeys.map(PublicKey25519Proposition.apply).toSet

  //todo: protection?
  override def secrets: Set[PrivateKey25519] =
  dbSecrets.getEntries.map(e => PrivateKey25519(e.getValue, e.getKey)).toSet

  override def secretByPublicImage(publicImage: PublicKey25519Proposition): Option[PrivateKey25519] =
    Option(dbSecrets.get(publicImage.bytes))
      .map(privBytes => PrivateKey25519(privBytes, publicImage.pubKeyBytes))

  override type NVCT = NothingAtStakeCoinWallet

  override def companion: NodeViewComponentCompanion = ??? //todo: fix

  override def scanPersistent(modifier: PMOD): NothingAtStakeCoinWallet = ???

  def rollback(to: VersionTag) = ??? //todo: fix
}

