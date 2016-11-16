import org.scalatest.Suites
import scorex.nothingAtStakeCoin.state._

class NothingAtStakeCoinTestSuite extends Suites(
  new NothingAtStakeCoinNodeNodeViewModifierCompanionSpec
)
