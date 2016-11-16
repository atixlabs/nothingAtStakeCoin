package nothingAtStakeCoin

import org.scalacheck.{Arbitrary, Gen}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.crypto.signatures.Curve25519
import scorex.nothingAtStakeCoin.state.{NothingAtStakeCoinInput, NothingAtStakeCoinOutput, NothingAtStakeCoinTransaction}

trait ObjectGenerators {

  def genBoundedBytes(minSize: Int, maxSize: Int): Gen[Array[Byte]] = {
    Gen.choose(minSize, maxSize) flatMap { sz => Gen.listOfN(sz, Arbitrary.arbitrary[Byte]).map(_.toArray) }
  }

  def genBytesList(size: Int): Gen[Array[Byte]] = genBoundedBytes(size, size)

  lazy val keyGenerator: Gen[(PrivateKey25519, PublicKey25519Proposition)] = genBytesList(Curve25519.KeyLength)
    .map(s => PrivateKey25519Companion.generateKeys(s))

  lazy val inputsGenerator: Gen[IndexedSeq[NothingAtStakeCoinInput]] =
    Gen.listOf(keyGenerator.map(keyPair => NothingAtStakeCoinInput(keyPair._2, 0L)))
      .map(_.toIndexedSeq)

  lazy val outputsGenerator: Gen[IndexedSeq[NothingAtStakeCoinOutput]] =
    Gen.listOf(keyGenerator.map(keyPair => NothingAtStakeCoinOutput(keyPair._2, Arbitrary.arbitrary[Long].sample.get)))
      .map(_.toIndexedSeq)

  lazy val signaturesGenerator: Gen[IndexedSeq[Signature25519]] =
    Gen.listOf(genBytesList(Signature25519.SignatureSize)
      .map(b => Signature25519(b)))
      .map(_.toIndexedSeq)

  lazy val nothingAtSakeCoinTransactionGenerator: Gen[NothingAtStakeCoinTransaction] = for {
    outputs <- outputsGenerator
    inputs <- inputsGenerator
    signatures <- signaturesGenerator
    timestamp: Long <- Arbitrary.arbitrary[Long]
    fee: Long <- Arbitrary.arbitrary[Long]
  } yield new NothingAtStakeCoinTransaction(
    from = inputs,
    to = outputs,
    signatures = signatures,
    fee = fee,
    timestamp = timestamp
  )

  lazy val nothingAtStakeCoinTransactionArrayGenerator: Gen[Array[NothingAtStakeCoinTransaction]] =
    Gen.listOf(nothingAtSakeCoinTransactionGenerator).map(_.toArray)
}
