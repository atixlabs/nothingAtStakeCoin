package scorex.nothingAtStakeCoin.consensus

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.nothingAtStakeCoin.ObjectGenerators
import scorex.nothingAtStakeCoin.block.NothingAtStakeCoinSyncInfo

class NothingAtStakeCoinSyncInfoSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ObjectGenerators {

  property("Sync Info serialization") {
    forAll(syncInfoGenerator) { syncInfo: NothingAtStakeCoinSyncInfo =>
      val asBytes = syncInfo.bytes
      val asObject = NothingAtStakeCoinSyncInfo.parse(asBytes).get
      val asBytesAgain = asObject.bytes
      asBytes shouldEqual asBytesAgain
    }
  }
}
