package scorex.nothingAtStakeCoin.transaction

import io.circe.Json
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.NodeViewModifierCompanion
import scorex.core.block.Block
import scorex.core.block.Block.{BlockId, Timestamp, Version}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.nothingAtStakeCoin.state.{NothingAtStakeCoinNodeNodeViewModifierCompanion, NothingAtStakeCoinTransaction}
import shapeless.{::, HNil}
import com.google.common.primitives.{Ints, Longs}
import scorex.crypto.encode.Base58
import scorex.nothingAtStakeCoin.transaction.NothingAtStakeCoinBlock.{CoinAgeLength, GenerationSignature}
import io.circe.syntax._

import scala.util.{Failure, Success, Try}

case class NothingAtStakeCoinBlock( override val parentId:ModifierId,
                                    override val timestamp: Timestamp,
                                    generationSignature: GenerationSignature,
                                    generator: PublicKey25519Proposition,
                                    coinAge : CoinAgeLength,
                                    txs: Seq[NothingAtStakeCoinTransaction]
                                  )
  extends Block[PublicKey25519Proposition, NothingAtStakeCoinTransaction] {

  override type BlockFields = ModifierId :: Timestamp :: GenerationSignature :: PublicKey25519Proposition ::
                              Version :: NothingAtStakeCoinBlock.CoinAgeLength ::
                              Seq[NothingAtStakeCoinTransaction] ::HNil

  override def version: Version = ???

  override def json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "parentId" -> Base58.encode(parentId).asJson,
    "timestamp" -> timestamp.asJson,
    "generationSignature" -> Base58.encode(generationSignature).asJson,
    "generator" -> Base58.encode(generator.pubKeyBytes).asJson,
    "coinAge" -> coinAge.asJson,
    "txs" -> txs.map(_.json).asJson
  ).asJson

  override def blockFields: BlockFields = parentId :: timestamp :: generationSignature :: generator :: version :: coinAge :: txs :: HNil

  override def transactions: Option[Seq[NothingAtStakeCoinTransaction]] = Some(txs)

  override type M = this.type
  override val modifierTypeId: ModifierTypeId = ???

  override def id: ModifierId = ???

  override def companion: NodeViewModifierCompanion[NothingAtStakeCoinBlock.this.type] = ???
}

object NothingAtStakeCoinBlock{
  type CoinAgeLength = Long

  type GenerationSignature = Array[Byte]

  val SignatureLength = 64
}

object NothingAtStakeCoinBlockCompanion extends NodeViewModifierCompanion[NothingAtStakeCoinBlock] {
  def messageToSign(block: NothingAtStakeCoinBlock): Array[Byte] = bytes(block)

  override def bytes(block: NothingAtStakeCoinBlock): Array[Byte] = {
    block.parentId ++
    Longs.toByteArray(block.timestamp) ++
    block.generationSignature ++
    block.generator.pubKeyBytes ++
    Array(block.version) ++
    Longs.toByteArray(block.coinAge) ++ {
      val cntTxs = Ints.toByteArray(block.txs.size)
      block.txs.foldLeft(cntTxs) { case (bytes, tx) => bytes ++ NothingAtStakeCoinNodeNodeViewModifierCompanion.bytes(tx) }
    }
  }

  override def parse(bytes: Array[ModifierTypeId]): Try[NothingAtStakeCoinBlock] = Try {
    val parentId = bytes.slice(0, Block.BlockIdLength)
    val timestamp = Longs.fromByteArray(bytes.slice(Block.BlockIdLength, Block.BlockIdLength + 8))
    val version = bytes.slice(Block.BlockIdLength + 8, Block.BlockIdLength + 9).head
    val coinAge = Longs.fromByteArray(bytes.slice(Block.BlockIdLength + 9, Block.BlockIdLength + 17))
    val s0 = Block.BlockIdLength + 17
    val generationSignature = bytes.slice(s0, s0 + NothingAtStakeCoinBlock.SignatureLength)
    val generator = PublicKey25519Proposition(bytes.slice(s0 + NothingAtStakeCoinBlock.SignatureLength,
                                                          s0 + NothingAtStakeCoinBlock.SignatureLength + 32))
    val s1 = s0 + NothingAtStakeCoinBlock.SignatureLength + 32
    val cntTxs = Ints.fromByteArray(bytes.slice(s1, s1 + 4))
    val txsBytes = bytes.slice(s1 + 4, s1 + 4 + bytes.length-1)
    val optionalTxs = NothingAtStakeCoinNodeNodeViewModifierCompanion.parseTransactionsArray(cntTxs, txsBytes)
    optionalTxs match{
      case Success(txs) => NothingAtStakeCoinBlock(parentId, timestamp, generationSignature, generator, coinAge, txs)
      case Failure(e) => throw e
    }
  }
}
