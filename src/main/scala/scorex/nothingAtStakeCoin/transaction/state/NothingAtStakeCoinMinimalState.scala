package scorex.nothingAtStakeCoin.transaction.state

import java.nio.ByteBuffer

import scorex.core.NodeViewComponentCompanion
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.MinimalState.VersionTag
import scorex.core.transaction.state.StateChanges
import scorex.core.transaction.state.authenticated.BoxMinimalState
import scorex.nothingAtStakeCoin.block.NothingAtStakeCoinBlock
import scorex.nothingAtStakeCoin.transaction.NothingAtStakeCoinTransaction
import scorex.nothingAtStakeCoin.transaction.account.PublicKey25519NoncedBox

import scala.util.{Failure, Success, Try}

case class NothingAtStakeCoinMinimalState(
                                           override val version: VersionTag,
                                           history: Map[VersionTag, NothingAtStakeCoinMinimalState] = Map(),
                                           boxes: Map[ByteBuffer, PublicKey25519NoncedBox]
                                         )
  extends BoxMinimalState[PublicKey25519Proposition,
    PublicKey25519NoncedBox,
    NothingAtStakeCoinTransaction,
    NothingAtStakeCoinBlock,
    NothingAtStakeCoinMinimalState] {

  override def semanticValidity(tx: NothingAtStakeCoinTransaction): Try[Unit] = Success(Unit)

  override def closedBox(boxId: Array[Byte]): Option[PublicKey25519NoncedBox] = boxes.get(ByteBuffer.wrap(boxId))

  override def boxesOf(proposition: PublicKey25519Proposition): Seq[PublicKey25519NoncedBox] =
    boxes.values.filter(p => p.proposition.address == proposition.address).toSeq

  override def changes(mod: NothingAtStakeCoinBlock): Try[StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]] = {
    Try {
      val initial = (Set(): Set[Array[Byte]], Set(): Set[PublicKey25519NoncedBox])
      val toChange = mod.txs.foldLeft(initial) {
        case (changes, tx) => (
          changes._1 ++ tx.from.map(_.id),
          changes._2 ++ tx.newBoxes.toSet
          )
      }

      StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox](toChange._1, toChange._2)
    }
  }

  override def applyChanges(changes: StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox], newVersion: VersionTag):
  Try[NothingAtStakeCoinMinimalState] = {
    val afterRemoval = changes.boxIdsToRemove.foldLeft(boxes) { case (newBoxes, idToRemove) => newBoxes - ByteBuffer.wrap(idToRemove) }
    val afterAppending = changes.toAppend.foldLeft(afterRemoval) { case (newBoxes, toAdd) => newBoxes + (ByteBuffer.wrap(toAdd.id) -> toAdd) }
    Success(NothingAtStakeCoinMinimalState(newVersion, history + (this.version -> this), afterAppending))
  }

  override def rollbackTo(version: VersionTag): Try[NothingAtStakeCoinMinimalState] = {
    history.get(version) match {
      case Some(previousVersion) => Success(previousVersion)
      case None => Failure(new RuntimeException(s"Unable to rollback Minimal State, $version not found"))
    }
  }

  override type NVCT = this.type

  override def companion: NodeViewComponentCompanion = ???

}

object NothingAtStakeCoinMinimalState {
  def genesisState(): NothingAtStakeCoinMinimalState = NothingAtStakeCoinMinimalState(Array.emptyByteArray, Map.empty, Map.empty)
}
