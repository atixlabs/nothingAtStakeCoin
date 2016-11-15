package scorex.nothingAtStakeCoin

import scorex.core.NodeViewComponentCompanion
import scorex.core.block.StateChanges
import scorex.core.transaction.account.PublicKeyNoncedBox
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.MinimalState
import scorex.core.transaction.state.MinimalState.VersionTag
import scorex.nothingAtStakeCoin.state.NothingAtStakeCoinTransaction
import scorex.nothingAtStakeCoin.transaction.NothingAtStakeCoinBlock

import scala.util.Try

case class NothingAtStakeCoinMinimalState() extends MinimalState[
  PublicKey25519Proposition,
  PublicKeyNoncedBox[PublicKey25519Proposition],
  NothingAtStakeCoinTransaction,
  NothingAtStakeCoinBlock,
  NothingAtStakeCoinMinimalState
  ] {
  override def version: VersionTag = ???

  override def closedBox(boxId: Array[Byte]): Option[PublicKeyNoncedBox[PublicKey25519Proposition]] = ???

  override def validate(transaction: NothingAtStakeCoinTransaction): Try[Unit] = ???

  override def boxesOf(proposition: PublicKey25519Proposition): Seq[PublicKeyNoncedBox[PublicKey25519Proposition]] = ???

  override def changes(mod: NothingAtStakeCoinBlock): Try[StateChanges[PublicKey25519Proposition, PublicKeyNoncedBox[PublicKey25519Proposition]]] = ???

  override def applyChanges(changes: StateChanges[PublicKey25519Proposition, PublicKeyNoncedBox[PublicKey25519Proposition]], newVersion: VersionTag): Try[NothingAtStakeCoinMinimalState] = ???

  override def rollbackTo(version: VersionTag): Try[NothingAtStakeCoinMinimalState] = ???

  override type NVCT = this.type

  override def companion: NodeViewComponentCompanion = ???
}
