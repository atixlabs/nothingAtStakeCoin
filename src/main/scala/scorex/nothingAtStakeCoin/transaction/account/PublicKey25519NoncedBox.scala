package scorex.nothingAtStakeCoin.transaction.account

import com.google.common.primitives.Longs
import scorex.core.serialization.BytesParseable
import scorex.core.transaction.account.PublicKeyNoncedBox
import scorex.core.transaction.box.proposition.{Constants25519, PublicKey25519Proposition}

import scala.util.Try

case class PublicKey25519NoncedBox(
                                    override val proposition: PublicKey25519Proposition,
                                    override val nonce: Long,
                                    override val value: Long
                                  ) extends PublicKeyNoncedBox[PublicKey25519Proposition] {
  override lazy val bytes: Array[Byte] =
    proposition.pubKeyBytes ++ Longs.toByteArray(nonce) ++ Longs.toByteArray(value)
}

object PublicKey25519NoncedBox extends BytesParseable[PublicKey25519NoncedBox] {
  override def parseBytes(bytes: Array[Byte]): Try[PublicKey25519NoncedBox] = Try {
    val pk = PublicKey25519Proposition(bytes.take(Constants25519.PubKeyLength))
    val nonce = Longs.fromByteArray(bytes.slice(Constants25519.PubKeyLength, Constants25519.PubKeyLength + Longs.BYTES))
    val value = Longs.fromByteArray(bytes.slice(Constants25519.PubKeyLength + Longs.BYTES, Constants25519.PubKeyLength + Longs.BYTES + Longs.BYTES))
    PublicKey25519NoncedBox(pk, nonce, value)
  }
}
