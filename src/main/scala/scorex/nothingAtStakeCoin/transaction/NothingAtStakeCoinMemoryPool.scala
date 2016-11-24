package scorex.nothingAtStakeCoin.transaction

import java.nio.ByteBuffer

import scorex.core.NodeViewComponentCompanion
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.transaction.MemoryPool

import scala.util.{Success, Try}

case class NothingAtStakeCoinMemoryPool(unconfirmed: Map[ByteBuffer, NothingAtStakeCoinTransaction])
  extends MemoryPool[NothingAtStakeCoinTransaction, NothingAtStakeCoinMemoryPool] {

  private def toStoreableId(id: ModifierId) = ByteBuffer.wrap(id)

  override def getById(id: ModifierId): Option[NothingAtStakeCoinTransaction] = unconfirmed.get(toStoreableId(id))

  override def notIn(ids: Seq[ModifierId]): Seq[ModifierId] = ???

  override def getAll(ids: Seq[ModifierId]): Seq[NothingAtStakeCoinTransaction] = ids.flatMap(getById)

  override def put(tx: NothingAtStakeCoinTransaction): Try[NothingAtStakeCoinMemoryPool] =
    Success(NothingAtStakeCoinMemoryPool(unconfirmed + (toStoreableId(tx.id) -> tx)))

  override def put(txs: Iterable[NothingAtStakeCoinTransaction]): Try[NothingAtStakeCoinMemoryPool] =
    Success(NothingAtStakeCoinMemoryPool(unconfirmed ++ txs.map(tx => toStoreableId(tx.id) -> tx)))

  override def putWithoutCheck(txs: Iterable[NothingAtStakeCoinTransaction]): NothingAtStakeCoinMemoryPool =
    NothingAtStakeCoinMemoryPool(unconfirmed ++ txs.map(tx => toStoreableId(tx.id) -> tx))

  override def remove(tx: NothingAtStakeCoinTransaction): NothingAtStakeCoinMemoryPool = {
    NothingAtStakeCoinMemoryPool(unconfirmed - toStoreableId(tx.id))
  }

  override def take(limit: Int): Iterable[NothingAtStakeCoinTransaction] =
    unconfirmed.values.toSeq.sortBy(-_.fee).take(limit)

  override def filter(id: Array[Byte]): NothingAtStakeCoinMemoryPool = NothingAtStakeCoinMemoryPool(unconfirmed - ByteBuffer.wrap(id))

  override def filter(tx: NothingAtStakeCoinTransaction): NothingAtStakeCoinMemoryPool = remove(tx)

  override def filter(txs: Seq[NothingAtStakeCoinTransaction]): NothingAtStakeCoinMemoryPool = txs.foldLeft(this)((mp, tx) => mp.filter(tx))

  override type NVCT = this.type

  override def companion: NodeViewComponentCompanion = ???
}

object NothingAtStakeCoinMemoryPool {
  lazy val emptyPool: NothingAtStakeCoinMemoryPool = NothingAtStakeCoinMemoryPool(Map())
}
