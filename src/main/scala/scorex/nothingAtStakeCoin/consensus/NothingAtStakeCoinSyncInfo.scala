package scorex.nothingAtStakeCoin.consensus

import scorex.core.NodeViewModifier
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.consensus.{BlockChain, SyncInfo}
import scorex.core.network.message.SyncInfoSpec

import scala.util.{Success, Try}

case class NothingAtStakeCoinSyncInfo(answer: Boolean, lastBlockID: NodeViewModifier.ModifierId, score: BlockChain.Score) extends SyncInfo {
  override def startingPoints: Seq[(ModifierTypeId, ModifierId)] = Seq.empty

  override def bytes: Array[Byte] = Array.emptyByteArray
}

object NothingAtStakeCoinSyncInfo {
  def parse(data: Array[Byte]): Try[NothingAtStakeCoinSyncInfo] = {
    Success(NothingAtStakeCoinSyncInfo(answer = true, Array.emptyByteArray, 0L))
  }
}

object NothingAtStakeCoinSyncInfoSpec extends SyncInfoSpec[NothingAtStakeCoinSyncInfo](NothingAtStakeCoinSyncInfo.parse)