package scorex.nothingAtStakeCoin.transaction

import com.google.common.primitives.{Bytes, Ints, Longs}
import io.circe.Json
import io.circe.syntax._
import scorex.core.NodeViewModifierCompanion
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.BoxTransaction
import scorex.core.transaction.account.PublicKeyNoncedBox
import scorex.core.transaction.box.BoxUnlocker
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.crypto.encode.Base64
import scorex.crypto.signatures.Curve25519
import scorex.nothingAtStakeCoin.transaction.NothingAtStakeCoinTransaction.{Nonce, Value}
import scorex.nothingAtStakeCoin.transaction.account.PublicKey25519NoncedBox
import scorex.nothingAtStakeCoin.transaction.state.NothingAtStakeCoinMinimalState

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

case class NothingAtStakeCoinInput(proposition: PublicKey25519Proposition, nonce: Nonce) {
  lazy val id: Array[Byte] = PublicKeyNoncedBox.idFromBox(proposition, nonce)

  lazy val json: Json = Map(
    "address" -> proposition.address.asJson
  ).asJson
}

case class NothingAtStakeCoinOutput(proposition: PublicKey25519Proposition, value: Value) {
  lazy val json: Json = Map(
    "address" -> proposition.address.asJson,
    "value" -> value.asJson
  ).asJson
}

case class NothingAtStakeCoinTransaction(
                                          from: IndexedSeq[NothingAtStakeCoinInput],
                                          to: IndexedSeq[NothingAtStakeCoinOutput],
                                          signatures: IndexedSeq[Signature25519],
                                          override val fee: Long,
                                          override val timestamp: Long
                                        )
  extends BoxTransaction[PublicKey25519Proposition, PublicKey25519NoncedBox] {

  override lazy val unlockers: Traversable[BoxUnlocker[PublicKey25519Proposition]] = from.zip(signatures).map {
    case (input, signature) => new BoxUnlocker[PublicKey25519Proposition] {
      override val closedBoxId = input.id
      override val boxKey = signature
    }
  }

  override val newBoxes: Traversable[PublicKey25519NoncedBox] = to.zipWithIndex.map {
    case (output, index) =>
      val nonce = NothingAtStakeCoinTransaction.generateNonce(output, from, timestamp, index)
      PublicKey25519NoncedBox(output.proposition, nonce, output.value)
  }

  override type M = NothingAtStakeCoinTransaction

  override lazy val companion = NothingAtStakeCoinNodeNodeViewModifierCompanion

  override lazy val json: Json = Map(
    "from" -> from.map(_.json).toList.asJson,
    "to" -> to.map(_.json).toList.asJson,
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

  private def transactionSize(sigLength: Int, fromLength: Int, toLength: Int): Int = {
    val sigSize = Curve25519.SignatureLength
    val elementLength = 8 + Curve25519.KeyLength
    28 + sigLength * sigSize + fromLength * elementLength + toLength * elementLength
  }

  def parseTransactionsArray(cntTxs: Int, bytes: Array[Byte]): Try[Seq[NothingAtStakeCoinTransaction]] = Try {
    val offsetSigLength: Int = 16
    val offsetFromLength: Int = 20
    val offsetToLength: Int = 24
    val reverseTxs: Seq[NothingAtStakeCoinTransaction] =
      (0 until cntTxs).foldLeft[(Seq[NothingAtStakeCoinTransaction], Int)](Tuple2(Seq(), 0)) {
        (rec, i) => (Seq(), 0)
          val (prevTxs, partialSumSize) = rec
          val sigLength = Ints.fromByteArray(bytes.slice(partialSumSize + offsetSigLength, partialSumSize + offsetSigLength + 4))
          val fromLength = Ints.fromByteArray(bytes.slice(partialSumSize + offsetFromLength, partialSumSize + offsetFromLength + 4))
          val toLength = Ints.fromByteArray(bytes.slice(partialSumSize + offsetToLength, partialSumSize + offsetToLength + 4))
          val txSize = transactionSize(sigLength, fromLength, toLength)
          val txBytes = bytes.slice(partialSumSize, partialSumSize + txSize)
          parse(txBytes) match {
            case Success(tx) => (tx +: prevTxs, partialSumSize + txSize)
            case Failure(e) => throw e
          }
      }._1
    reverseTxs.reverse
  }

  def createTransaction(state: NothingAtStakeCoinMinimalState,
                        fromPk: PrivateKey25519,
                        from: String,
                        to: IndexedSeq[String],
                        amount: IndexedSeq[Long],
                        fee: Long,
                        timestamp: Long): Try[NothingAtStakeCoinTransaction] = Try {

    val totalAmountToSend = amount.sum + fee

    val sender: PublicKey25519Proposition = PublicKey25519Proposition.validPubKey(from) match {
      case Success(pk: PublicKey25519Proposition) => pk
      case Failure(err) => throw err
    }

    val toPropositions: IndexedSeq[(PublicKey25519Proposition, Value)] = to.zip(amount).map {
      case (address: String, amount: Long) => (PublicKey25519Proposition.validPubKey(address).get, amount)
    }

    val unspentsToUse: (Long, Seq[PublicKey25519NoncedBox]) = findUnspentsToPay(totalAmountToSend, state.boxesOf(sender), 0, Seq())

    val propositions = unspentsToUse match {
      case (unspentsAmount, boxes) if unspentsAmount > totalAmountToSend =>
        (PublicKey25519Proposition.validPubKey(from).get, unspentsToUse._1 - totalAmountToSend) +: toPropositions
      case (unspentsAmount, boxes) if unspentsAmount == totalAmountToSend => toPropositions
      case (unspentsAmount, boxes) if unspentsAmount < totalAmountToSend => throw new Exception("Not Enough funds to spend")
    }

    NothingAtStakeCoinTransaction(
      fromPk,
      unspentsToUse._2.map(_.nonce).toIndexedSeq,
      propositions.to,
      fee,
      timestamp
    )
  }

  @tailrec
  private def findUnspentsToPay(totalAmountToSend: Value, boxes: Seq[PublicKey25519NoncedBox], acum: Long, boxesToUse: Seq[PublicKey25519NoncedBox]):
  (Long, Seq[PublicKey25519NoncedBox]) = {
    acum match {
      case _ if acum >= totalAmountToSend || boxes.isEmpty => (acum, boxesToUse)
      case _ if acum < totalAmountToSend => findUnspentsToPay(totalAmountToSend, boxes.tail, acum + boxes.head.value, boxes.head +: boxesToUse)
    }
  }
}

object NothingAtStakeCoinTransaction {
  type Value = Long
  type Nonce = Long

  def apply(fromPk: PrivateKey25519,
            from: IndexedSeq[Nonce],
            to: IndexedSeq[(PublicKey25519Proposition, Value)],
            fee: Long,
            timestamp: Long): NothingAtStakeCoinTransaction = {

    val withoutSignature: NothingAtStakeCoinTransaction = new NothingAtStakeCoinTransaction(
      from = from.map { case (n: Nonce) => NothingAtStakeCoinInput(fromPk.publicImage, n) },
      to = to.map { case (p: PublicKey25519Proposition, n: Nonce) => NothingAtStakeCoinOutput(p, n) },
      signatures = from.map { _ => PrivateKey25519Companion.sign(fromPk, Array.emptyByteArray) },
      fee = fee,
      timestamp = timestamp
    )

    val signatures = from.map { _ => PrivateKey25519Companion.sign(fromPk, withoutSignature.messageToSign) }

    NothingAtStakeCoinTransaction(
      withoutSignature.from,
      withoutSignature.to,
      signatures,
      fee,
      timestamp
    )
  }

  def generateNonce(output: NothingAtStakeCoinOutput, inputs: Seq[NothingAtStakeCoinInput], timestamp: Long, index: Int): Long =
    Longs.fromByteArray(FastCryptographicHash(
      output.proposition.pubKeyBytes ++
        inputs.map(input => Longs.toByteArray(input.nonce)).fold(Array.emptyByteArray)(_ ++ _) ++
        Longs.toByteArray(timestamp) ++
        Ints.toByteArray(index)
    ).take(Longs.BYTES))

}
