package scorex.nothingAtStakeCoin.state

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.nothingAtStakeCoin.ObjectGenerators

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
}
