package scorex.nothingAtStakeCoin

import io.circe
import org.scalacheck.{Arbitrary, Gen}
import scorex.core.NodeViewModifier.{ModifierId, ModifierIdSize}
import scorex.core.block.Block.Timestamp
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.crypto.signatures.Curve25519
import scorex.nothingAtStakeCoin.block.NothingAtStakeCoinBlock
import scorex.nothingAtStakeCoin.block.NothingAtStakeCoinBlock.CoinAgeLength
import scorex.nothingAtStakeCoin.settings.NothingAtStakeCoinSettings
import scorex.nothingAtStakeCoin.transaction.{NothingAtStakeCoinInput, NothingAtStakeCoinOutput, NothingAtStakeCoinTransaction}

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

  lazy val signatureGenerator: Gen[Signature25519] = genBytesList(Signature25519.SignatureSize).map(b => Signature25519(b))

  lazy val signaturesGenerator: Gen[IndexedSeq[Signature25519]] =
    Gen.listOf(signatureGenerator).map(_.toIndexedSeq)

  lazy val nothingAtSakeCoinTransactionGenerator: Gen[NothingAtStakeCoinTransaction] = for {
    outputs <- outputsGenerator
    inputs <- inputsGenerator
    signatures <- signaturesGenerator
    timestamp: Long <- Gen.choose(0: Long, Long.MaxValue)
    fee: Long <- Arbitrary.arbitrary[Long]
  } yield new NothingAtStakeCoinTransaction(
    from = inputs,
    to = outputs,
    signatures = signatures,
    fee = fee,
    timestamp = timestamp
  )

  lazy val settings: NothingAtStakeCoinSettings = new NothingAtStakeCoinSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile("test-settings.json")
  }

  lazy val nothingAtStakeCoinTransactionSeqGenerator: Gen[Seq[NothingAtStakeCoinTransaction]] =
    Gen.listOfN(settings.transactionsPerBlock, nothingAtSakeCoinTransactionGenerator).map(_.toSeq)

  lazy val emptyTx : NothingAtStakeCoinTransaction = NothingAtStakeCoinTransaction(IndexedSeq(), IndexedSeq(), IndexedSeq(), 0, 0)

  lazy val nothingAtSakeCoinBlockGenerator: Gen[NothingAtStakeCoinBlock] = for {
    parentId: ModifierId <- genBytesList(ModifierIdSize)
    coinAge: CoinAgeLength <- Gen.choose(0: Long, Long.MaxValue-1)
    key <- keyGenerator
    possibleTxs <- nothingAtStakeCoinTransactionSeqGenerator
    txs = possibleTxs.map(tx => if(tx.timestamp==Long.MaxValue) tx.copy(timestamp = Long.MaxValue-1) else tx)
    coinStakeTx = emptyTx
    timestamp : Timestamp = (coinStakeTx +: txs).map(_.timestamp).max + 1
  } yield NothingAtStakeCoinBlock(
    parentId = parentId,
    timestamp = timestamp,
    generatorKeys = key._1,
    coinAge = coinAge,
    txs = coinStakeTx +: txs.distinct
  )

  def genNothingAtStakeCoinBlockSeqGeneratorSeqOfN(size: Int): Gen[Seq[NothingAtStakeCoinBlock]] = {
    Gen.listOfN(size, nothingAtSakeCoinBlockGenerator).map(_.toSeq)
  }

  lazy val nothingAtStakeCoinBlockSeqGenerator: Gen[Seq[NothingAtStakeCoinBlock]] =
    Gen.listOf(nothingAtSakeCoinBlockGenerator).map(_.toSeq)
}
