package scorex.nothingAtStakeCoin.history

import scorex.core.NodeViewComponentCompanion
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.consensus.History
import scorex.core.consensus.History.{BlockId, RollbackTo}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.nothingAtStakeCoin.consensus.NothingAtStakeCoinSyncInfo
import scorex.nothingAtStakeCoin.state.NothingAtStakeCoinTransaction
import scorex.nothingAtStakeCoin.transaction.{NothingAtStakeCoinBlock, NothingAtStakeCoinBlockCompanion}
import scorex.nothingAtStakeCoin.transaction.NothingAtStakeCoinBlock.CoinAgeLength

import scala.util.{Failure, Success, Try}

case class BlockInfo(sons:Long, totalCoinAge: NothingAtStakeCoinBlock.CoinAgeLength)

case class NothingAtStakeCoinHistory( blocks:Map[BlockId, NothingAtStakeCoinBlock]=Map(),
                                      blocksInfo:Map[BlockId, BlockInfo]=Map(),
                                      bestNBlocks:List[BlockId]
                                    )
  extends History[PublicKey25519Proposition,
                  NothingAtStakeCoinTransaction,
                  NothingAtStakeCoinBlock,
                  NothingAtStakeCoinSyncInfo,
                  NothingAtStakeCoinHistory] {

  override def isEmpty: Boolean = blocks.isEmpty

  override def blockById(blockId: BlockId): Option[NothingAtStakeCoinBlock] = blocks.get(blockId)

  override def append(block: NothingAtStakeCoinBlock): Try[(NothingAtStakeCoinHistory, Option[RollbackTo[NothingAtStakeCoinBlock]])] = {
    /* Verifiy the block:
     *    txs in UTXO: In charge of nodeViewHolder
     *    timestamp valid?
     *    block generator matches signature?
     *      block.generator.verify(???, block.generationSignature)
     *
     */
    if( blocks.get(block.parentId).isDefined &&
        block.coinAge == getCoinAge(block.id)){
      /* Add block */
      val info = blocksInfo.get(block.parentId).get
      val newBlocks = blocks + (block.id->block)
      val newBlockTotalCoinAge = info.totalCoinAge + block.coinAge
      val newBlocksInfo = blocksInfo -  block.parentId +
                                        (block.parentId->BlockInfo(info.sons + 1, info.totalCoinAge)) +
                                        (block.id->BlockInfo(0, newBlockTotalCoinAge))
      val newBestN = obtainBestN(block.id, newBlockTotalCoinAge)
      Success(NothingAtStakeCoinHistory(newBlocks, newBlocksInfo, newBestN), None)
    }
  }

  override def openSurfaceIds(): Seq[BlockId] = ???

  override def continuation(from: Seq[(ModifierTypeId, ModifierId)], size: Int): Option[Seq[NothingAtStakeCoinBlock]] = ???

  override def continuationIds(from: Seq[(ModifierTypeId, ModifierId)], size: Int): Option[Seq[(ModifierTypeId, ModifierId)]] = ???

  override def syncInfo(answer: Boolean): NothingAtStakeCoinSyncInfo = ???

  override def compare(other: NothingAtStakeCoinSyncInfo): _root_.scorex.core.consensus.History.HistoryComparisonResult.Value = ???

  override type NVCT = this.type

  override def companion: NodeViewComponentCompanion = ???

  def getCoinAge(block: BlockId): Option[NothingAtStakeCoinBlock.CoinAgeLength] = ???

  /* Auxiliary functions */
  def obtainBestN(newBlock: BlockId, newBlockTotalCoinAge: NothingAtStakeCoinBlock.CoinAgeLength): List[BlockId] = {
    val prevBestN: List[BlockId] = bestNBlocks
    if(prevBestN.size<NothingAtStakeCoinHistory.N){
      newBlock +: prevBestN
    }else{
      val obtainTotalCoinAge : (BlockId=>Option[NothingAtStakeCoinBlock.CoinAgeLength]) =
        block => blocksInfo.get(block).map(_.totalCoinAge)
      val worstBlock = prevBestN.minBy[NothingAtStakeCoinBlock.CoinAgeLength](block => obtainTotalCoinAge(block).get)
      if(obtainTotalCoinAge(worstBlock).get < newBlockTotalCoinAge){
        newBlock +: (prevBestN diff List(worstBlock))
      }else{
        prevBestN
      }
    }
  }
}

object NothingAtStakeCoinHistory{
  //TODO: Temporary N value that should be obtained from config file
  val N: Int = 10
}