package scorex.nothingAtStakeCoin.state

import com.google.common.primitives.{Bytes, Ints, Longs}
import io.circe.Json
import io.circe.syntax._
import scorex.core.NodeViewModifierCompanion
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.BoxTransaction
import scorex.core.transaction.account.PublicKeyNoncedBox
import scorex.core.transaction.box.BoxUnlocker
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.{Proof, Signature25519}
import scorex.crypto.encode.{Base58, Base64}
import scorex.crypto.signatures.Curve25519
import scorex.nothingAtStakeCoin.state.NothingAtStakeCoinTransaction._
import scorex.nothingAtStakeCoin.transaction.PublicKey25519NoncedBox

import scala.util.Try

case class NothingAtStakeCoinInput(proposition: PublicKey25519Proposition, nonce: Nonce)

case class NothingAtStakeCoinOutput(proposition: PublicKey25519Proposition, value: Value)

case class NothingAtStakeCoinTransaction(from: IndexedSeq[NothingAtStakeCoinInput],
                                         to: IndexedSeq[NothingAtStakeCoinOutput],
                                         signatures: IndexedSeq[Signature25519],
                                         override val fee: Long,
                                         override val timestamp: Long
                                        ) extends BoxTransaction[PublicKey25519Proposition, PublicKeyNoncedBox[PublicKey25519Proposition]] {

  override lazy val unlockers: Traversable[BoxUnlocker[PublicKey25519Proposition]] = from.zip(signatures).map {
    case (input, signature) => new BoxUnlocker[PublicKey25519Proposition] {
      override val closedBoxId: Array[Byte] = PublicKeyNoncedBox.idFromBox(input.proposition, input.nonce)
      override val boxKey: Proof[PublicKey25519Proposition] = signature
    }
  }

  override val newBoxes: Traversable[PublicKeyNoncedBox[PublicKey25519Proposition]] = to.zipWithIndex.map {
    case (output, index) =>
      val nonce = generateNonce(output, from, timestamp, index)
      PublicKey25519NoncedBox(output.proposition, nonce, output.value)
  }

  override type M = NothingAtStakeCoinTransaction

  override lazy val companion = NothingAtStakeCoinNodeNodeViewModifierCompanion

  override lazy val json: Json = Map(
    "from" -> from.map(input => Base58.encode(input.proposition.pubKeyBytes)).asJson,
    "to" -> from.map(output => Base58.encode(output.proposition.pubKeyBytes)).asJson,
    "signatures" -> signatures.map(signature => Base64.encode(signature.bytes)).asJson,
    "fee" -> fee.asJson,
    "timestamp" -> timestamp.asJson
  ).asJson
}

object NothingAtStakeCoinNodeNodeViewModifierCompanion extends NodeViewModifierCompanion[NothingAtStakeCoinTransaction] {
  override def bytes(m: NothingAtStakeCoinTransaction): Array[Byte] = {
    Bytes.concat(Longs.toByteArray(m.fee),
      Longs.toByteArray(m.timestamp),
      Ints.toByteArray(m.signatures.length),
      Ints.toByteArray(m.from.length),
      Ints.toByteArray(m.to.length),
      m.signatures.foldLeft(Array[Byte]())((a, b) => a ++ b.bytes),
      m.from.foldLeft(Array[Byte]())((a, i) => a ++ i.proposition.bytes ++ Longs.toByteArray(i.nonce)),
      m.to.foldLeft(Array[Byte]())((a, o) => a ++ o.proposition.bytes ++ Longs.toByteArray(o.value))
    )
  }

  override def parse(bytes: Array[Byte]): Try[NothingAtStakeCoinTransaction] = Try {
    val fee = Longs.fromByteArray(bytes.slice(0, 8))
    val timestamp = Longs.fromByteArray(bytes.slice(8, 16))
    val sigLength = Ints.fromByteArray(bytes.slice(16, 20))
    val fromLength = Ints.fromByteArray(bytes.slice(20, 24))
    val toLength = Ints.fromByteArray(bytes.slice(24, 28))
    val signatures = (0 until sigLength) map { i =>
      Signature25519(bytes.slice(28 + i * Curve25519.SignatureLength, 28 + (i + 1) * Curve25519.SignatureLength))
    }
    val s = 28 + sigLength * Curve25519.SignatureLength
    val elementLength = 8 + Curve25519.KeyLength
    val from = (0 until fromLength) map { i =>
      val pk = bytes.slice(s + i * elementLength, s + (i + 1) * elementLength - 8)
      val v = Longs.fromByteArray(bytes.slice(s + (i + 1) * elementLength - 8, s + (i + 1) * elementLength))
      NothingAtStakeCoinInput(PublicKey25519Proposition(pk), v)
    }
    val s2 = s + fromLength * elementLength
    val to = (0 until toLength) map { i =>
      val pk = bytes.slice(s2 + i * elementLength, s2 + (i + 1) * elementLength - 8)
      val v = Longs.fromByteArray(bytes.slice(s2 + (i + 1) * elementLength - 8, s2 + (i + 1) * elementLength))
      NothingAtStakeCoinOutput(PublicKey25519Proposition(pk), v)
    }
    NothingAtStakeCoinTransaction(from, to, signatures, fee, timestamp)
  }
}

object NothingAtStakeCoinTransaction {
  type Value = Long
  type Nonce = Long

  def generateNonce(output: NothingAtStakeCoinOutput, inputs: Seq[NothingAtStakeCoinInput], timestamp: Long, index: Int): Long =
    Longs.fromByteArray(FastCryptographicHash(
      output.proposition.pubKeyBytes ++
        inputs.map(input => Longs.toByteArray(input.nonce)).fold(Array.emptyByteArray)(_ ++ _) ++
        Longs.toByteArray(timestamp) ++
        Ints.toByteArray(index)
    ).take(Longs.BYTES))

}
