package scorex.nothingAtStakeCoin.history

import java.nio.ByteBuffer

import org.scalatest.{FeatureSpec, GivenWhenThen, Matchers}
import scorex.core.NodeViewModifier
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.block.Block.BlockId
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58
import scorex.nothingAtStakeCoin.ObjectGenerators
import scorex.nothingAtStakeCoin.transaction.{NothingAtStakeCoinBlock, NothingAtStakeCoinBlockCompanion}

class NothingAtStakeCoinNodeNodeHistorySpec extends FeatureSpec
  with GivenWhenThen
  with Matchers
  with ObjectGenerators
  with ScorexLogging{

  def newBlockCorrectlyInHistory(history: NothingAtStakeCoinHistory, block: NothingAtStakeCoinBlock): Unit ={
    val byteBufferBlockId = blockIdToByteBuffer(block.id)
    assert(history.blocks.get(byteBufferBlockId).isDefined)
    val blockInHistory = history.blocks(byteBufferBlockId)
    blockInHistory shouldEqual block
    assert(history.blocksInfo.get(byteBufferBlockId).isDefined)
    val blockInfoInHistory = history.blocksInfo(byteBufferBlockId)
    assert(blockInfoInHistory.sons==0)
    assert((history.blocks.size==1 && history.blocksInfo.get(byteBufferBlockId).isDefined &&
        history.blocksInfo(byteBufferBlockId).totalCoinAge==block.coinAge) ||
      (history.blocksInfo.get(blockIdToByteBuffer(block.parentId)).isDefined &&
        history.blocksInfo.get(byteBufferBlockId).isDefined &&
        history.blocksInfo(blockIdToByteBuffer(block.parentId)).totalCoinAge + block.coinAge ==
          history.blocksInfo(byteBufferBlockId).totalCoinAge))
  }

  def blockIdToByteBuffer(id : BlockId) = ByteBuffer.wrap(id)

  feature("History append works properly"){
    scenario("Block added to empty history"){
      Given("an empty history")
      val emptyHistory = NothingAtStakeCoinHistory()
      assert(emptyHistory.isEmpty)

      When("a block is added")
      val keyPair = keyGenerator.sample.get
      val block = NothingAtStakeCoinBlockCompanion.signBlock(keyPair._1,
        nothingAtSakeCoinBlockGenerator.sample.get.copy(generator = keyPair._2))
      val optHistoryWithOneBlock = emptyHistory.append(block)

      Then("the block was added successfully")
      assert(optHistoryWithOneBlock.isSuccess)
      val historyWithOneBlock = optHistoryWithOneBlock.get._1

      Then("the block is in history")
      newBlockCorrectlyInHistory(historyWithOneBlock, block)

      Then("there is only one block in history")
      assert(!historyWithOneBlock.isEmpty)
      assert(historyWithOneBlock.blocks.size==1)
      assert(historyWithOneBlock.blocksInfo.size==1)
      assert(historyWithOneBlock.bestNChains.size==1)
      val blockInfoInHistory = historyWithOneBlock.blocksInfo(blockIdToByteBuffer(block.id))

      Then("the block has the correct totalCoinAge")
      assert(blockInfoInHistory.totalCoinAge==block.coinAge)

      Then("the block is the best chain")
      assert(historyWithOneBlock.bestNChains.size == 1)
      assert(historyWithOneBlock.bestNChains.contains(blockIdToByteBuffer(block.id)))
    }

    scenario("Only one best blockchain"){
      Given("an empty history")
      val emptyHistory = NothingAtStakeCoinHistory()
      val blocksSeq : Seq[NothingAtStakeCoinBlock] = nothingAtStakeCoinBlockSeqGenerator.sample.get
      blocksSeq.foldLeft[(NothingAtStakeCoinHistory, ModifierId)](emptyHistory, Array.fill(NodeViewModifier.ModifierIdSize)(1 : Byte)){
        case (rec, block) =>
          val (prevHistory, prevBlockId) = rec
          When("adding a new block")
          val keyPair = keyGenerator.sample.get
          val signedBlock = NothingAtStakeCoinBlockCompanion.signBlock(keyPair._1,
            nothingAtSakeCoinBlockGenerator.sample.get.copy(generator = keyPair._2, parentId = prevBlockId))
          val optNewHistory = prevHistory.append(signedBlock)

          Then("the block was added successfully")
          assert(optNewHistory.isSuccess)
          val newHistory = optNewHistory.get._1

          Then("the block is in history")
          newBlockCorrectlyInHistory(newHistory, signedBlock)

          Then("its parent has now 1 son")
          assert(prevHistory.isEmpty || newHistory.blocksInfo(blockIdToByteBuffer(prevBlockId)).sons==1)

          Then("the new block is the only one in history.bestNChains")
          assert(newHistory.bestNChains.size==1)
          assert(newHistory.bestNChains.head == blockIdToByteBuffer(signedBlock.id))

          (newHistory, signedBlock.id)
      }

    }
    scenario("BlockChain forks without having to delete branches"){
      Given("a history with one block")
      val emptyHistory = NothingAtStakeCoinHistory()
      val keyPair = keyGenerator.sample.get
      val genesisBlock = NothingAtStakeCoinBlockCompanion.signBlock(keyPair._1,
        nothingAtSakeCoinBlockGenerator.sample.get.copy(generator = keyPair._2, parentId = Array.fill(NodeViewModifier.ModifierIdSize)(1 : Byte)))
      val historyWithOneBlock = emptyHistory.append(genesisBlock).get._1

      val blocksSeq : Seq[NothingAtStakeCoinBlock] = genNothingAtStakeCoinBlockSeqGeneratorSeqOfN(NothingAtStakeCoinHistory.N).sample.get
      val historyWithNChains = blocksSeq.foldLeft[NothingAtStakeCoinHistory](historyWithOneBlock){
        case (prevHistory, block) =>
          When("adding new forks")
          val keyPair = keyGenerator.sample.get
          val signedBlock = NothingAtStakeCoinBlockCompanion.signBlock(keyPair._1,
            nothingAtSakeCoinBlockGenerator.sample.get.copy(generator = keyPair._2, parentId = genesisBlock.id))
          val optNewHistory = prevHistory.append(signedBlock)
          val newHistory = optNewHistory.get._1

          Then("the block was added correctly")
          newBlockCorrectlyInHistory(newHistory, signedBlock)

          Then("new history.bestNChains now also contains the new block")
          log.debug(s"${newHistory.bestNChains.size}")
          assert(prevHistory.bestNChains.size==1 || newHistory.bestNChains.size==prevHistory.bestNChains.size + 1)
          assert(newHistory.bestNChains.contains(blockIdToByteBuffer(signedBlock.id)))

          Then("genesisBlock has one more son")
          assert(newHistory.blocksInfo(blockIdToByteBuffer(genesisBlock.id)).sons ==
            prevHistory.blocksInfo(blockIdToByteBuffer(genesisBlock.id)).sons + 1)
          newHistory
      }
    }
    scenario("BlockChain forks and N chains limit is surpassed"){
      Given("a history with N chains")
      val emptyHistory = NothingAtStakeCoinHistory()
      val genesisKeyPair = keyGenerator.sample.get
      val genesisBlock = NothingAtStakeCoinBlockCompanion.signBlock(genesisKeyPair._1,
        nothingAtSakeCoinBlockGenerator.sample.get.copy(generator = genesisKeyPair._2, parentId = Array.fill(NodeViewModifier.ModifierIdSize)(1 : Byte)))
      val historyWithOneBlock = emptyHistory.append(genesisBlock).get._1
      val blocksSeq : Seq[NothingAtStakeCoinBlock] = genNothingAtStakeCoinBlockSeqGeneratorSeqOfN(NothingAtStakeCoinHistory.N).sample.get
      val historyWithNChains = blocksSeq.foldLeft[NothingAtStakeCoinHistory](historyWithOneBlock){
        case (prevHistory, block) =>
          val keyPair = keyGenerator.sample.get
          val signedBlock = NothingAtStakeCoinBlockCompanion.signBlock(keyPair._1,
            nothingAtSakeCoinBlockGenerator.sample.get.copy(generator = keyPair._2, parentId = genesisBlock.id))
          val optNewHistory = prevHistory.append(signedBlock)
          val newHistory = optNewHistory.get._1
          newHistory
      }

      When("adding an extra block")
      val keyPair = keyGenerator.sample.get
      val signedBlock = NothingAtStakeCoinBlockCompanion.signBlock(keyPair._1,
        nothingAtSakeCoinBlockGenerator.sample.get.copy(generator = keyPair._2, parentId = genesisBlock.id))
      val optHistoryWithBlockRemoved = historyWithNChains.append(signedBlock)

      Then("the block was added successfully")
      assert(optHistoryWithBlockRemoved.isSuccess)
      val historyWithBlockRemoved = optHistoryWithBlockRemoved.get._1

      Then("the number of blocks is N")
      assert(historyWithBlockRemoved.blocks.size==historyWithNChains.blocks.size)

      Then("the new history removed the correct block from history.bestNChains")
      val diffPrevWithNew = historyWithNChains.bestNChains diff historyWithBlockRemoved.bestNChains
      val diffNewWithPrev = historyWithBlockRemoved.bestNChains diff historyWithNChains.bestNChains
      assert(diffNewWithPrev.size==1 && diffPrevWithNew.size==1)
      assert(historyWithBlockRemoved.blocksInfo(diffNewWithPrev.head).totalCoinAge >=
        historyWithNChains.blocksInfo(diffPrevWithNew.head).totalCoinAge)

      Then("genesisBlock has N sons")
      assert(historyWithBlockRemoved.blocksInfo(blockIdToByteBuffer(genesisBlock.id)).sons==NothingAtStakeCoinHistory.N)
    }
  }
}