package scorex.nothingAtStakeCoin.transaction

import scorex.core.block.Block
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.nothingAtStakeCoin.state.NothingAtStakeCoinTransaction

case class NothingAtStakeCoinBlock extends Block[PublicKey25519Proposition, NothingAtStakeCoinTransaction] {

}
