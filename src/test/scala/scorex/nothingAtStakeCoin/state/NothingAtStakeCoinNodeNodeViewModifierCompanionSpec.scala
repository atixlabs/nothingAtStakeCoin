package scorex.nothingAtStakeCoin.state

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.nothingAtStakeCoin.ObjectGenerators
import scorex.nothingAtStakeCoin.transaction.{NothingAtStakeCoinNodeNodeViewModifierCompanion, NothingAtStakeCoinTransaction}

class NothingAtStakeCoinNodeNodeViewModifierCompanionSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ObjectGenerators {

  property("Payment serialization") {
    forAll(nothingAtSakeCoinTransactionGenerator) { tx: NothingAtStakeCoinTransaction =>
      val asBytes = NothingAtStakeCoinNodeNodeViewModifierCompanion.bytes(tx)
      val asObject = NothingAtStakeCoinNodeNodeViewModifierCompanion.parse(asBytes).get
      val asBytesAgain = NothingAtStakeCoinNodeNodeViewModifierCompanion.bytes(asObject)
      asBytes shouldEqual asBytesAgain
    }
  }

  property("TransactionArray serialization") {
    forAll(nothingAtStakeCoinTransactionSeqGenerator) { txSeq: Seq[NothingAtStakeCoinTransaction] =>
      val asBytes = txSeq.foldLeft[Array[Byte]](Array[Byte]()) {
        (prevTxs, tx) => prevTxs ++ NothingAtStakeCoinNodeNodeViewModifierCompanion.bytes(tx)
      }
      val asObject = NothingAtStakeCoinNodeNodeViewModifierCompanion.parseTransactionsArray(txSeq.length, asBytes).get
      val asBytesAgain = asObject.foldLeft[Array[Byte]](Array[Byte]()) {
        (prevTxs, tx) => prevTxs ++ NothingAtStakeCoinNodeNodeViewModifierCompanion.bytes(tx)
      }
      asBytes shouldEqual asBytesAgain
    }
  }
}
