package scorex.nothingAtStakeCoin.history

import java.nio.ByteBuffer

import org.scalatest.{FeatureSpec, GivenWhenThen, Matchers}
import scorex.core.NodeViewModifier
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.block.Block.BlockId
import scorex.core.consensus.History.HistoryComparisonResult
import scorex.core.utils.ScorexLogging
import scorex.nothingAtStakeCoin.ObjectGenerators
import scorex.nothingAtStakeCoin.block.NothingAtStakeCoinBlock.CoinAgeLength
import scorex.nothingAtStakeCoin.block.{NothingAtStakeCoinBlock, NothingAtStakeCoinBlockCompanion}
import scorex.nothingAtStakeCoin.consensus.NothingAtStakeCoinHistory

class NothingAtStakeCoinNodeNodeHistorySpec extends FeatureSpec
  with GivenWhenThen
  with Matchers
  with ObjectGenerators
  with ScorexLogging {

  val numberOfBestChains: Int = 10

  def newBlockCorrectlyInHistory(history: NothingAtStakeCoinHistory, block: NothingAtStakeCoinBlock): Unit = {
    val byteBufferBlockId = blockIdToByteBuffer(block.id)
    assert(history.blocks.get(byteBufferBlockId).isDefined)
    val blockInHistory = history.blocks(byteBufferBlockId)
    blockInHistory shouldEqual block
    assert(history.blocksSons.get(byteBufferBlockId).isDefined)
    val blockSons = history.blocksSons(byteBufferBlockId)
    assert(blockSons.isEmpty)
  }

  def blockIdToByteBuffer(id: BlockId) = ByteBuffer.wrap(id)

  def generateHistory(blockNumber: Int): (NothingAtStakeCoinHistory, NothingAtStakeCoinBlock) = {
    val emptyHistory = NothingAtStakeCoinHistory()
    val genesisBlock = nothingAtSakeCoinBlockGenerator(Some(Array.fill(NodeViewModifier.ModifierIdSize)(1: Byte))).sample.get
    val historyWithOneBlock = emptyHistory.append(genesisBlock).get._1
    val blocksSeq: Seq[NothingAtStakeCoinBlock] = genNothingAtStakeCoinBlockSeqGeneratorSeqOfN(blockNumber).sample.get
    val historyWithNChains = blocksSeq.foldLeft[NothingAtStakeCoinHistory](historyWithOneBlock) {
      case (prevHistory, block) =>
        val signedBlock = nothingAtSakeCoinBlockGenerator(Some(genesisBlock.id)).sample.get
        val optNewHistory = prevHistory.append(signedBlock)
        val newHistory = optNewHistory.get._1
        newHistory
    }

    (historyWithNChains, genesisBlock)
  }

  feature("History append works properly") {
    scenario("Block added to empty history") {
      Given("an empty history")
      val emptyHistory = NothingAtStakeCoinHistory()
      assert(emptyHistory.isEmpty)

      When("a block is added")
      val block = nothingAtSakeCoinBlockGenerator().sample.get
      val optHistoryWithOneBlock = emptyHistory.append(block)

      Then("the block was added successfully")
      assert(optHistoryWithOneBlock.isSuccess)
      val historyWithOneBlock = optHistoryWithOneBlock.get._1

      Then("the block is in history")
      newBlockCorrectlyInHistory(historyWithOneBlock, block)

      Then("there is only one block in history")
      assert(!historyWithOneBlock.isEmpty)
      assert(historyWithOneBlock.blocks.size == 1)
      assert(historyWithOneBlock.blocksSons.size == 1)
      assert(historyWithOneBlock.bestNChains.size == 1)

      Then("the block is the best chain")
      assert(historyWithOneBlock.bestNChains.size == 1)
      assert(historyWithOneBlock.bestNChains.contains(blockIdToByteBuffer(block.id)))
    }

    scenario("Only one best blockchain") {
      Given("an empty history")
      val emptyHistory = NothingAtStakeCoinHistory()
      val blocksSeq: Seq[NothingAtStakeCoinBlock] = nothingAtStakeCoinBlockSeqGenerator().sample.get
      blocksSeq.foldLeft[(NothingAtStakeCoinHistory, ModifierId)](emptyHistory, Array.fill(NodeViewModifier.ModifierIdSize)(1: Byte)) {
        case (rec, block) =>
          val (prevHistory, prevBlockId) = rec
          When("adding a new block")
          val signedBlock = nothingAtSakeCoinBlockGenerator(Some(prevBlockId)).sample.get
          val optNewHistory = prevHistory.append(signedBlock)

          Then("the block was added successfully")
          assert(optNewHistory.isSuccess)
          val newHistory = optNewHistory.get._1

          Then("the block is in history")
          newBlockCorrectlyInHistory(newHistory, signedBlock)

          Then("its parent has now 1 son")
          assert(prevHistory.isEmpty || newHistory.blocksSons(blockIdToByteBuffer(prevBlockId)).length == 1)

          Then("the new block is the only one in history.bestNChains")
          assert(newHistory.bestNChains.size == 1)
          assert(newHistory.bestNChains.head == blockIdToByteBuffer(signedBlock.id))

          (newHistory, signedBlock.id)
      }

    }
    scenario("BlockChain forks without having to delete branches") {
      Given("a history with one block")
      val emptyHistory = NothingAtStakeCoinHistory()
      val genesisBlock = nothingAtSakeCoinBlockGenerator(Some(Array.fill(NodeViewModifier.ModifierIdSize)(1: Byte))).sample.get
      val historyWithOneBlock = emptyHistory.append(genesisBlock).get._1

      val blocksSeq: Seq[NothingAtStakeCoinBlock] = genNothingAtStakeCoinBlockSeqGeneratorSeqOfN(numberOfBestChains).sample.get
      blocksSeq.foldLeft[NothingAtStakeCoinHistory](historyWithOneBlock) {
        case (prevHistory, block) =>
          When("adding new forks")
          val signedBlock = nothingAtSakeCoinBlockGenerator(Some(genesisBlock.id)).sample.get
          val optNewHistory = prevHistory.append(signedBlock)
          val newHistory = optNewHistory.get._1

          Then("the block was added correctly")
          newBlockCorrectlyInHistory(newHistory, signedBlock)

          Then("new history.bestNChains now also contains the new block")
          log.debug(s"${newHistory.bestNChains.size}")
          assert(prevHistory.bestNChains.size == 1 || newHistory.bestNChains.size == prevHistory.bestNChains.size + 1)
          assert(newHistory.bestNChains.contains(blockIdToByteBuffer(signedBlock.id)))

          Then("genesisBlock has one more son")
          assert(newHistory.blocksSons(blockIdToByteBuffer(genesisBlock.id)).length ==
            prevHistory.blocksSons(blockIdToByteBuffer(genesisBlock.id)).length + 1)
          newHistory
      }
    }
    scenario("BlockChain forks and N chains limit is surpassed without recursive removal") {
      Given("a history with N chains")
      val (historyWithNChains, genesisBlock) = generateHistory(numberOfBestChains)

      When("adding an extra block that causes the removal of a chain from history")
      val bestTotalCoinAge: CoinAgeLength = historyWithNChains.bestNChains.map(blockId => historyWithNChains.blocks(blockId).coinAge).max
      val blockCoinAge = bestTotalCoinAge + 1

      val keyPair = keyGenerator.sample.get
      val signedBlock = NothingAtStakeCoinBlockCompanion.signBlock(keyPair._1,
        nothingAtSakeCoinBlockGenerator().sample.get.copy(generator = keyPair._2, parentId = genesisBlock.id, coinAge = blockCoinAge))
      val optHistoryWithBlockRemoved = historyWithNChains.append(signedBlock)

      Then("the block was added successfully")
      assert(optHistoryWithBlockRemoved.isSuccess)
      val historyWithBlockRemoved = optHistoryWithBlockRemoved.get._1
      newBlockCorrectlyInHistory(historyWithBlockRemoved, signedBlock)

      Then("the number of blocks is N+1")
      assert(historyWithBlockRemoved.blocks.size == numberOfBestChains + 1)

      Then("the new history removed the correct block from history")
      val diffPrevWithNew = historyWithNChains.bestNChains diff historyWithBlockRemoved.bestNChains
      val diffNewWithPrev = historyWithBlockRemoved.bestNChains diff historyWithNChains.bestNChains
      assert(diffNewWithPrev.size == 1 && diffPrevWithNew.size == 1)
      val idBlockAdded = diffNewWithPrev.head
      val idBlockRemoved = diffPrevWithNew.head
      assert(historyWithBlockRemoved.blocks.get(idBlockAdded).isDefined)
      assert(historyWithBlockRemoved.blocks.get(idBlockRemoved).isEmpty)
      assert(historyWithBlockRemoved.blocksSons.get(idBlockAdded).isDefined)
      assert(historyWithBlockRemoved.blocksSons.get(idBlockRemoved).isEmpty)

      Then("genesisBlock has N sons")
      assert(historyWithBlockRemoved.blocksSons(blockIdToByteBuffer(genesisBlock.id)).length == numberOfBestChains)
    }
    scenario("BlockChain forks and N chains limit is surpassed with recursive removal") {
      Given("a history with N chains of length 2 and genesys block where fork ocurred")

      When("adding an extra block with the highest totalCoinAge and having genesys block as parent")

      Then("the block was added successfully")

      Then("the number of blocks is 2*N")

      Then("the new history removed the correct chain from history")

      Then("genesisBlock has N sons")
    }
  }

  feature("History can return continuation Ids") {
    scenario("ContinuationIds from an empty history returns None") {
      Given("An empty history")
      val emptyHistory = NothingAtStakeCoinHistory()

      When("Asking for an id that does not exist")
      val blockId = blockIdGenerator.sample.get
      val continuationIds = emptyHistory.continuationIds(from = Seq(NothingAtStakeCoinBlock.ModifierTypeId -> blockId), size = 1)

      Then("History should return none")
      continuationIds shouldBe None
    }

    scenario("ContinuationIds from a chain with items") {
      Given("A loaded history")
      val (history, _) = generateHistory(numberOfBestChains)

      When("Asking for the continuation of a leaf node")
      val leafId = history.bestNChains.head.array()
      val continuationIds = history.continuationIds(from = Seq(NothingAtStakeCoinBlock.ModifierTypeId -> leafId), size = 10)

      Then("History should return none")
      continuationIds shouldBe None
    }

    scenario("ContinuationIds from genesis block") {
      Given("A loaded history")
      val (history, genesisBlock) = generateHistory(numberOfBestChains)

      When("Asking for the continuation for the genesisBlockId")
      val continuationIds = history.continuationIds(from = Seq(NothingAtStakeCoinBlock.ModifierTypeId -> genesisBlock.id), size = numberOfBestChains / 2)

      Then("History should return all nodes ids")
      continuationIds.isDefined shouldBe true
      continuationIds.get.size shouldBe numberOfBestChains / 2
    }

    scenario("ContinuationIds after chaining blocks") {
      Given("A loaded history")
      val (history, genesisBlock) = generateHistory(numberOfBestChains)

      When("Chaining some blocks")
      val idToLookFor = history.bestNChains.head.array()
      val block1 = nothingAtSakeCoinBlockGenerator(Some(idToLookFor)).sample.get
      val block2 = nothingAtSakeCoinBlockGenerator(Some(block1.id)).sample.get
      val block3 = nothingAtSakeCoinBlockGenerator(Some(block2.id)).sample.get
      val historyWithAllBlocks = history.append(block1).get._1.append(block2).get._1.append(block3).get._1

      When("Asking for the continuation for the genesisBlockId")
      val continuation = historyWithAllBlocks.continuation(from = Seq(NothingAtStakeCoinBlock.ModifierTypeId -> idToLookFor), size = numberOfBestChains / 2)

      Then("History should return all appended nodes")
      continuation.isDefined shouldBe true
      val blocks: Seq[NothingAtStakeCoinBlock] = continuation.get
      blocks.size shouldBe 3
      blocks.contains(block1)
      blocks.contains(block2)
      blocks.contains(block3)
    }
  }

  feature("History comparison") {
    scenario("Comparing the same history should return equal") {
      Given("There is a history loaded")
      val (history, _) = generateHistory(numberOfBestChains)

      When("Comparing with itself")
      val syncInfo = history.syncInfo(true)
      val comparisonResult = history.compare(syncInfo)

      Then("History should return none")
      assert(comparisonResult == HistoryComparisonResult.Equal)
    }

    scenario("Comparing a chain with one more block") {
      Given("There is a history1")
      val (history1, _) = generateHistory(numberOfBestChains)
      Given("A block is added resulting in history2")
      val block = nothingAtSakeCoinBlockGenerator(Some(history1.bestNChains.head.array())).sample.get
      val history2 = history1.append(block).get._1

      When("Comparing amongst them")
      val syncInfo1 = history1.syncInfo(true)
      val syncInfo2 = history2.syncInfo(true)
      val comparisonResult1 = history1.compare(syncInfo2)
      val comparisonResult2 = history2.compare(syncInfo1)

      Then("History1 should be younger")
      assert(comparisonResult1 == HistoryComparisonResult.Older)
      Then("History2 should be older")
      assert(comparisonResult2 == HistoryComparisonResult.Younger)
    }
  }
}