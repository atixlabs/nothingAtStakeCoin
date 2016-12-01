package scorex.nothingAtStakeCoin.block

import java.nio.ByteBuffer

import com.google.common.primitives.{Ints, Longs}
import io.circe.Json
import io.circe.syntax._
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.block.Block
import scorex.core.block.Block.{Timestamp, Version}
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.core.{NodeViewModifier, NodeViewModifierCompanion}
import scorex.crypto.encode.Base58
import scorex.nothingAtStakeCoin.block.NothingAtStakeCoinBlock.{CoinAgeLength, GenerationSignature}
import scorex.nothingAtStakeCoin.transaction.{NothingAtStakeCoinNodeNodeViewModifierCompanion, NothingAtStakeCoinTransaction}
import scorex.utils.Booleans

import scala.util.{Failure, Success, Try}

case class NothingAtStakeCoinBlockBody(stakeTx: Option[NothingAtStakeCoinTransaction], txs: Seq[NothingAtStakeCoinTransaction]) {
  def allTransactions: Seq[NothingAtStakeCoinTransaction] = Seq(stakeTx).flatten ++ txs
}

case class NothingAtStakeCoinBlock(override val parentId: ModifierId,
                                   override val timestamp: Timestamp,
                                   generationSignature: GenerationSignature,
                                   generator: PublicKey25519Proposition,
                                   coinAge: CoinAgeLength,
                                   body: NothingAtStakeCoinBlockBody
                                  )
  extends Block[PublicKey25519Proposition, NothingAtStakeCoinTransaction] {

  override type M = NothingAtStakeCoinBlock
  type C = NothingAtStakeCoinBlockCompanion.type

  override val modifierTypeId: ModifierTypeId = NothingAtStakeCoinBlock.ModifierTypeId

  override def version: Version = NothingAtStakeCoinBlock.Version

  override def transactions: Option[Seq[NothingAtStakeCoinTransaction]] = Some(body.allTransactions)

  override def id: ModifierId = FastCryptographicHash(companion.bytes(this))

  override def companion: C = NothingAtStakeCoinBlockCompanion

  override def json: Json = Map(
    "id" -> encodedId.asJson,
    "parentId" -> Base58.encode(parentId).asJson,
    "timestamp" -> timestamp.asJson,
    "generationSignature" -> Base58.encode(generationSignature).asJson,
    "generator" -> Base58.encode(generator.pubKeyBytes).asJson,
    "coinAge" -> coinAge.asJson,
    "txs" -> transactions.getOrElse(Seq()).map(_.json).asJson
  ).asJson

}

object NothingAtStakeCoinBlock {

  type CoinAgeLength = Long

  type GenerationSignature = Array[Byte]

  val SignatureLength = 64

  val ModifierTypeId = 1: Byte

  val Version = 1: Byte

  val CoinAgeLengthSize = 8

  lazy val GenesisParentBlockId: ModifierId = Array.fill(NodeViewModifier.ModifierIdSize)(1: Byte)
  lazy val EmptyChainId: ModifierId = Array.fill(NodeViewModifier.ModifierIdSize)(0: Byte)

  def apply(parentId: ModifierId,
            timestamp: Timestamp,
            generatorKeys: PrivateKey25519,
            coinAge: CoinAgeLength,
            stakeTx: Option[NothingAtStakeCoinTransaction],
            txs: Seq[NothingAtStakeCoinTransaction]): NothingAtStakeCoinBlock = {

    val unsignedBlock: NothingAtStakeCoinBlock = NothingAtStakeCoinBlock(
      parentId = parentId,
      timestamp = timestamp,
      generationSignature = Array.fill(NothingAtStakeCoinBlock.SignatureLength)(1: Byte),
      generator = generatorKeys.publicImage,
      coinAge = coinAge,
      body = NothingAtStakeCoinBlockBody(stakeTx = stakeTx, txs = txs)
    )

    val generationSignature = PrivateKey25519Companion.sign(generatorKeys, unsignedBlock.companion.messageToSign(unsignedBlock))

    unsignedBlock.copy(generationSignature = generationSignature.signature)
  }
}

object NothingAtStakeCoinBlockCompanion extends NodeViewModifierCompanion[NothingAtStakeCoinBlock] {
  def messageToSign(block: NothingAtStakeCoinBlock): Array[Byte] = {
    block.parentId ++
      Longs.toByteArray(block.timestamp) ++
      Array(block.version) ++
      Longs.toByteArray(block.coinAge) ++
      block.generator.pubKeyBytes ++
      block.body.stakeTx.fold(ByteBuffer.allocate(1).put(0: Byte).array()) { _ => ByteBuffer.allocate(1).put(1: Byte).array() } ++ {
      val cntTxs = Ints.toByteArray(block.body.allTransactions.size)
      block.body.allTransactions.foldLeft(cntTxs) { case (bytes, tx) => bytes ++ NothingAtStakeCoinNodeNodeViewModifierCompanion.bytes(tx) }
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
    val hasStakeTx = Booleans.fromByteArray(bytes.slice(s1, s1 + 1))
    val s2 = s1 + 1
    val cntTxs = Ints.fromByteArray(bytes.slice(s2, s2 + 4))
    val txsBytes = bytes.slice(s2 + 4, s2 + 4 + bytes.length - 1)
    val optionalTxs = NothingAtStakeCoinNodeNodeViewModifierCompanion.parseTransactionsArray(cntTxs, txsBytes)
    optionalTxs match {
      case Success(txs) => {
        val body = if (hasStakeTx) NothingAtStakeCoinBlockBody(Some(txs.head), txs.tail) else NothingAtStakeCoinBlockBody(None, txs)
        NothingAtStakeCoinBlock(
          parentId = parentId,
          timestamp = timestamp,
          generationSignature = generationSignature,
          generator = generator,
          coinAge = coinAge,
          body = body)
      }
      case Failure(e) => throw e
    }
  }

  def signBlock(privKey: PrivateKey25519, unsignedBlock: NothingAtStakeCoinBlock): NothingAtStakeCoinBlock = {
    val blockSignature = PrivateKey25519Companion.sign(privKey, unsignedBlock.companion.messageToSign(unsignedBlock))
    unsignedBlock.copy(generationSignature = blockSignature.signature)
  }
}
