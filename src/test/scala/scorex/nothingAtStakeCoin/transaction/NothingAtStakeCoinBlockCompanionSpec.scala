package scorex.nothingAtStakeCoin.transaction

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.nothingAtStakeCoin.ObjectGenerators

class NothingAtStakeCoinBlockCompanionSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ObjectGenerators {

  property("Block serialization") {
    forAll(nothingAtSakeCoinBlockGenerator) { block: NothingAtStakeCoinBlock =>
      val asBytes = NothingAtStakeCoinBlockCompanion.bytes(block)
      val asObject = NothingAtStakeCoinBlockCompanion.parse(asBytes).get
      val asBytesAgain = NothingAtStakeCoinBlockCompanion.bytes(asObject)
      asBytes shouldEqual asBytesAgain
    }
  }
}