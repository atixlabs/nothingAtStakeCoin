package scorex.nothingAtStakeCoin.transaction

import io.circe.Json
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.{NodeViewModifier, NodeViewModifierCompanion}
import scorex.core.block.Block
import scorex.core.block.Block.{BlockId, Timestamp, Version}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.nothingAtStakeCoin.state.{NothingAtStakeCoinNodeNodeViewModifierCompanion, NothingAtStakeCoinTransaction}
import shapeless.{::, HNil}
import com.google.common.primitives.{Ints, Longs}
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.nothingAtStakeCoin.transaction.NothingAtStakeCoinBlock.GenerationSignature
import scorex.crypto.encode.Base58
import scorex.nothingAtStakeCoin.transaction.NothingAtStakeCoinBlock.{CoinAgeLength, GenerationSignature}
import io.circe.syntax._
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}

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

  override type M = NothingAtStakeCoinBlock

  override val modifierTypeId: ModifierTypeId = NothingAtStakeCoinBlock.ModifierTypeId

  override def version: Version = NothingAtStakeCoinBlock.Version

  override def blockFields: BlockFields = parentId :: timestamp :: generationSignature :: generator :: version :: coinAge :: txs :: HNil

  override def transactions: Option[Seq[NothingAtStakeCoinTransaction]] = Some(txs)

  override def id: ModifierId = FastCryptographicHash(companion.bytes(this))

  override def companion = NothingAtStakeCoinBlockCompanion

  override def json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "parentId" -> Base58.encode(parentId).asJson,
    "timestamp" -> timestamp.asJson,
    "generationSignature" -> Base58.encode(generationSignature).asJson,
    "generator" -> Base58.encode(generator.pubKeyBytes).asJson,
    "coinAge" -> coinAge.asJson,
    "txs" -> txs.map(_.json).asJson
  ).asJson
}

object NothingAtStakeCoinBlock {

  type CoinAgeLength = Long

  type GenerationSignature = Array[Byte]

  val SignatureLength = 64

  val ModifierTypeId = 1: Byte

  val Version = 1: Byte

  lazy val GenesisBlockId: ModifierId = Array.fill(NodeViewModifier.ModifierIdSize)(1: Byte)

}

object NothingAtStakeCoinBlockCompanion extends NodeViewModifierCompanion[NothingAtStakeCoinBlock] {
  def messageToSign(block: NothingAtStakeCoinBlock): Array[Byte] = {
    block.parentId ++
    Longs.toByteArray(block.timestamp) ++
    Array(block.version) ++
    Longs.toByteArray(block.coinAge) ++
    block.generator.pubKeyBytes ++{
      val cntTxs = Ints.toByteArray(block.txs.size)
      block.txs.foldLeft(cntTxs) { case (bytes, tx) => bytes ++ NothingAtStakeCoinNodeNodeViewModifierCompanion.bytes(tx) }
    }
  }

  override def bytes(block: NothingAtStakeCoinBlock): Array[Byte] = {
    val bytesWithoutSignature = messageToSign(block)
    bytesWithoutSignature.slice(0, Block.BlockIdLength + 17) ++
      block.generationSignature ++
      bytesWithoutSignature.slice(Block.BlockIdLength + 17, bytesWithoutSignature.length)
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

  def signBlock(privKey: PrivateKey25519, unsignedBlock: NothingAtStakeCoinBlock) : NothingAtStakeCoinBlock = {
    val blockSignature = PrivateKey25519Companion.sign(privKey, unsignedBlock.companion.messageToSign(unsignedBlock))
    unsignedBlock.copy(generationSignature = blockSignature.signature)
  }
}
