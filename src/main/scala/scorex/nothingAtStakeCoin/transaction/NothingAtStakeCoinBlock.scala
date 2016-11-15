package scorex.nothingAtStakeCoin.transaction

import io.circe.Json
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.NodeViewModifierCompanion
import scorex.core.block.Block
import scorex.core.block.Block.{BlockId, Timestamp, Version}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.nothingAtStakeCoin.state.NothingAtStakeCoinTransaction
import shapeless.{::, HNil}

case class NothingAtStakeCoinBlock() extends Block[PublicKey25519Proposition, NothingAtStakeCoinTransaction] {
  override type BlockFields = BlockId :: Timestamp :: Version :: HNil

  override def version: Version = ???

  override def parentId: ModifierId = ???

  override def json: Json = ???

  override def timestamp: Timestamp = ???

  override def blockFields: BlockFields = ???

  override def transactions: Option[Seq[NothingAtStakeCoinTransaction]] = ???

  override type M = this.type
  override val modifierTypeId: ModifierTypeId = ???

  override def id: ModifierId = ???

  override def companion: NodeViewModifierCompanion[NothingAtStakeCoinBlock.this.type] = ???
}
