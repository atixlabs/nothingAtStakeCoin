package scorex.nothingAtStakeCoin.block

import scorex.core.NodeViewModifier
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.consensus.History._
import scorex.core.consensus.SyncInfo
import scorex.core.network.message.SyncInfoSpec

import scala.util.Try

case class NothingAtStakeCoinSyncInfo(answer: Boolean, bestNChains: List[BlockId]) extends SyncInfo {
  override def startingPoints: Seq[(ModifierTypeId, ModifierId)] = bestNChains.map(blockId => (NothingAtStakeCoinBlock.ModifierTypeId, blockId))

  override def bytes: Array[Byte] = (if (answer) Array(1: Byte) else Array(0: Byte)) ++ bestNChains.foldLeft(Array[Byte]())((arr, blockId) => arr ++ blockId)
}

object NothingAtStakeCoinSyncInfo {
  def parse(data: Array[Byte]): Try[NothingAtStakeCoinSyncInfo] = Try {
    val answer: Boolean = if (data.head == 1.toByte) true else false
    val bestNChains = data.slice(1, data.length).grouped(NodeViewModifier.ModifierIdSize).toList
    NothingAtStakeCoinSyncInfo(answer, bestNChains)
  }
}

object NothingAtStakeCoinSyncInfoSpec extends SyncInfoSpec[NothingAtStakeCoinSyncInfo](NothingAtStakeCoinSyncInfo.parse)