package scorex.nothingAtStakeCoin

import akka.actor.ActorRef
import scorex.core.LocalInterface
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.nothingAtStakeCoin.block.NothingAtStakeCoinBlock
import scorex.nothingAtStakeCoin.transaction.NothingAtStakeCoinTransaction

class NothingAtStakeCoinLocalInterface(override val viewHolderRef: ActorRef)
  extends LocalInterface[PublicKey25519Proposition, NothingAtStakeCoinTransaction, NothingAtStakeCoinBlock] {

  override protected def onStartingPersistentModifierApplication(pmod: NothingAtStakeCoinBlock): Unit = Unit

  override protected def onFailedTransaction(tx: NothingAtStakeCoinTransaction): Unit = Unit

  override protected def onFailedModification(mod: NothingAtStakeCoinBlock): Unit = Unit

  override protected def onSuccessfulTransaction(tx: NothingAtStakeCoinTransaction): Unit = Unit

  override protected def onSuccessfulModification(mod: NothingAtStakeCoinBlock): Unit = Unit

  override protected def onNoBetterNeighbour(): Unit = Unit

  override protected def onBetterNeighbourAppeared(): Unit = Unit
}
