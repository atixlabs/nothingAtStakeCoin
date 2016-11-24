import org.scalatest.Suites
import scorex.nothingAtStakeCoin.history.NothingAtStakeCoinNodeNodeHistorySpec
import scorex.nothingAtStakeCoin.peercoin.MinterSpec
import scorex.nothingAtStakeCoin.state._
import scorex.nothingAtStakeCoin.transaction.NothingAtStakeCoinBlockCompanionSpec

class NothingAtStakeCoinTestSuite extends Suites(
  new NothingAtStakeCoinNodeNodeViewModifierCompanionSpec,
  new NothingAtStakeCoinBlockCompanionSpec,
  new NothingAtStakeCoinNodeNodeHistorySpec,
  new MinterSpec
)
