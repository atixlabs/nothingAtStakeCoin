package scorex.nothingAtStakeCoin

import akka.actor.ActorRef
import scorex.core.LocalInterface
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.nothingAtStakeCoin.state.NothingAtStakeCoinTransaction
import scorex.nothingAtStakeCoin.transaction.NothingAtStakeCoinBlock

class NothingAtStakeCoinLocalInterface extends LocalInterface[PublicKey25519Proposition, NothingAtStakeCoinTransaction, NothingAtStakeCoinBlock]{
  override val viewHolderRef: ActorRef = ???

  override protected def onStartingPersistentModifierApplication(pmod: NothingAtStakeCoinBlock): Unit = ???

  override protected def onFailedTransaction(tx: NothingAtStakeCoinTransaction): Unit = ???

  override protected def onFailedModification(mod: NothingAtStakeCoinBlock): Unit = ???

  override protected def onSuccessfulTransaction(tx: NothingAtStakeCoinTransaction): Unit = ???

  override protected def onSuccessfulModification(mod: NothingAtStakeCoinBlock): Unit = ???

  override protected def onNoBetterNeighbour(): Unit = ???

  override protected def onBetterNeighbourAppeared(): Unit = ???
}
