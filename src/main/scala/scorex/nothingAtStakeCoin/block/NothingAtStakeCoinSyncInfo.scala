package scorex.nothingAtStakeCoin.block

import java.nio.ByteBuffer

import scorex.core.NodeViewModifier
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.block.Block.BlockId
import scorex.core.consensus.SyncInfo
import scorex.core.network.message.SyncInfoSpec
import scorex.nothingAtStakeCoin.block.NothingAtStakeCoinBlock.CoinAgeLength

import scala.util.Try

case class NothingAtStakeCoinSyncInfo(answer: Boolean, bestNChains: List[(BlockId, CoinAgeLength)]) extends SyncInfo {

  override def startingPoints: Seq[(ModifierTypeId, ModifierId)] = {
    val toSend = bestNChains.map { case (blockId: BlockId, _) => (NothingAtStakeCoinBlock.ModifierTypeId, blockId) }
    if(toSend.isEmpty) Seq(NothingAtStakeCoinBlock.ModifierTypeId -> NothingAtStakeCoinBlock.GenesisBlockId) else toSend
  }

  override def bytes: Array[Byte] = (if (answer) Array(1: Byte) else Array(0: Byte)) ++ bestNChains.foldLeft(Array[Byte]()) {
    (array, item) => {
      array ++ (item match {
        case (blockId: BlockId, coinAge: CoinAgeLength) =>
          blockId ++ ByteBuffer.allocate(NothingAtStakeCoinBlock.CoinAgeLengthSize).putLong(coinAge).array()
      })
    }
  }
}

object NothingAtStakeCoinSyncInfo {
  def parse(data: Array[Byte]): Try[NothingAtStakeCoinSyncInfo] = Try {
    val answer: Boolean = if (data.head == 1.toByte) true else false
    val bestNChains: List[(BlockId, CoinAgeLength)] = data.slice(1, data.length)
      .grouped(NodeViewModifier.ModifierIdSize + NothingAtStakeCoinBlock.CoinAgeLengthSize)
      .map(bytes => ByteBuffer.wrap(bytes.slice(0, NodeViewModifier.ModifierIdSize)).array() ->
        ByteBuffer.wrap(bytes.slice(NodeViewModifier.ModifierIdSize, bytes.length)).getLong)
      .toList
    NothingAtStakeCoinSyncInfo(answer, bestNChains)
  }
}

object NothingAtStakeCoinSyncInfoSpec extends SyncInfoSpec[NothingAtStakeCoinSyncInfo](NothingAtStakeCoinSyncInfo.parse)
