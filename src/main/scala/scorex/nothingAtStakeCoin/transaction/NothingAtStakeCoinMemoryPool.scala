package scorex.nothingAtStakeCoin.transaction

import scorex.core.{NodeViewComponentCompanion, NodeViewModifier}
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.transaction.MemoryPool
import scorex.nothingAtStakeCoin.state.NothingAtStakeCoinTransaction

import scala.util.Try

case class NothingAtStakeCoinMemoryPool(unconfirmed: Map[NodeViewModifier.ModifierId, NothingAtStakeCoinTransaction]) extends MemoryPool[NothingAtStakeCoinTransaction, NothingAtStakeCoinMemoryPool] {
  override def getById(id: ModifierId): Option[NothingAtStakeCoinTransaction] = ???

  override def notIn(ids: Seq[ModifierId]): Seq[ModifierId] = ???

  override def getAll(ids: Seq[ModifierId]): Seq[NothingAtStakeCoinTransaction] = ???

  override def put(tx: NothingAtStakeCoinTransaction): Try[NothingAtStakeCoinMemoryPool] = ???

  override def put(txs: Iterable[NothingAtStakeCoinTransaction]): Try[NothingAtStakeCoinMemoryPool] = ???

  override def putWithoutCheck(txs: Iterable[NothingAtStakeCoinTransaction]): NothingAtStakeCoinMemoryPool = ???

  override def remove(tx: NothingAtStakeCoinTransaction): NothingAtStakeCoinMemoryPool = ???

  override def take(limit: Int): Iterable[NothingAtStakeCoinTransaction] = ???

  override def filter(id: Array[Byte]): NothingAtStakeCoinMemoryPool = ???

  override def filter(tx: NothingAtStakeCoinTransaction): NothingAtStakeCoinMemoryPool = ???

  override def filter(txs: Seq[NothingAtStakeCoinTransaction]): NothingAtStakeCoinMemoryPool = ???

  override type NVCT = this.type

  override def companion: NodeViewComponentCompanion = ???
}
