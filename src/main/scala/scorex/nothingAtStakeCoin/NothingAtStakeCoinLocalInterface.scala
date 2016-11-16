package scorex.nothingAtStakeCoin

import akka.actor.ActorRef
import scorex.core.LocalInterface
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.nothingAtStakeCoin.state.NothingAtStakeCoinTransaction
import scorex.nothingAtStakeCoin.transaction.NothingAtStakeCoinBlock

class NothingAtStakeCoinLocalInterface(override val viewHolderRef: ActorRef)
  extends LocalInterface[PublicKey25519Proposition, NothingAtStakeCoinTransaction, NothingAtStakeCoinBlock] {

  override protected def onStartingPersistentModifierApplication(pmod: NothingAtStakeCoinBlock): Unit = {
    // TODO
  }

  override protected def onFailedTransaction(tx: NothingAtStakeCoinTransaction): Unit = {
    // TODO
  }

  override protected def onFailedModification(mod: NothingAtStakeCoinBlock): Unit = {
    // TODO
  }

  override protected def onSuccessfulTransaction(tx: NothingAtStakeCoinTransaction): Unit = {
    // TODO
  }

  override protected def onSuccessfulModification(mod: NothingAtStakeCoinBlock): Unit = {
    // TODO
  }

  override protected def onNoBetterNeighbour(): Unit = {
    // TODO
  }

  override protected def onBetterNeighbourAppeared(): Unit = {
    // TODO
  }
}
