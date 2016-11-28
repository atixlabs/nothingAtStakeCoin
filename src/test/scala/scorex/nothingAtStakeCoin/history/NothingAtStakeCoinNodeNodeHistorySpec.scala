package scorex.nothingAtStakeCoin.history

import java.nio.ByteBuffer

import org.scalacheck.Gen
import org.scalatest.{FeatureSpec, GivenWhenThen, Matchers}
import scorex.core.NodeViewModifier
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.block.Block.Timestamp
import scorex.core.consensus.History.HistoryComparisonResult
import scorex.core.transaction.state.PrivateKey25519
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58
import scorex.nothingAtStakeCoin.ObjectGenerators
import scorex.nothingAtStakeCoin.block.NothingAtStakeCoinBlock
import scorex.nothingAtStakeCoin.block.NothingAtStakeCoinBlock.CoinAgeLength
import scorex.nothingAtStakeCoin.consensus.NothingAtStakeCoinHistory
import scorex.nothingAtStakeCoin.transaction.NothingAtStakeCoinTransaction
import scorex.nothingAtStakeCoin.transaction.NothingAtStakeCoinTransaction.{Nonce, Value}

import scala.util.{Success, Try}

class NothingAtStakeCoinNodeNodeHistorySpec extends FeatureSpec
  with GivenWhenThen
  with Matchers
  with ObjectGenerators
  with ScorexLogging {

  val numberOfBestChains: Int = 10
  val numberOfTxsPerBlock: Int = NothingAtStakeCoinHistory.numberOfTxsPerBlock

  def newBlockCorrectlyInHistory(history: NothingAtStakeCoinHistory, block: NothingAtStakeCoinBlock): Unit = {
    val byteBufferBlockId = wrapId(block.id)
    assert(history.blocks.get(byteBufferBlockId).isDefined)
    val blockInHistory = history.blocks(byteBufferBlockId)
    blockInHistory shouldEqual block
    assert(history.blocksNodeInfo.get(byteBufferBlockId).isDefined)
    val blockNodeInfo = history.blocksNodeInfo(byteBufferBlockId)
    assert(blockNodeInfo.sons.isEmpty)
  }

  def wrapId(bytes: Array[Byte]): ByteBuffer = ByteBuffer.wrap(bytes)

  def generateHistory(blockNumber: Int, numberOfBestChains: Option[Int] = None): (NothingAtStakeCoinHistory, NothingAtStakeCoinBlock) = {
    val emptyHistory = NothingAtStakeCoinHistory(numberOfBestChains = numberOfBestChains.getOrElse(this.numberOfBestChains))
    val genesisBlock = nothingAtSakeCoinBlockGenerator(Some(Array.fill(NodeViewModifier.ModifierIdSize)(1: Byte))).sample.get
    val historyWithOneBlock = insertBlock(emptyHistory, genesisBlock).get
    val historyWithNChains = (1 to blockNumber).foldLeft[NothingAtStakeCoinHistory](historyWithOneBlock) {
      case (prevHistory, _) =>
        val block = nothingAtSakeCoinBlockGenerator(Some(genesisBlock.id)).sample.get
        val optNewHistory = insertBlock(prevHistory, block)
        val newHistory = optNewHistory.get
        newHistory
    }

    (historyWithNChains, genesisBlock)
  }

  def historyOneBlockWithTxs(lower: Timestamp, upper: Timestamp, fromPk: PrivateKey25519, txTimestamp: Timestamp):
  (NothingAtStakeCoinHistory, Seq[((Timestamp, Value), Int)], Seq[NothingAtStakeCoinTransaction]) = {
    val txsTimestamp: Seq[Timestamp] = Gen.listOfN(numberOfTxsPerBlock, Gen.choose(lower, upper)).sample.get
    val txsValues: Seq[Value] = Gen.listOfN(numberOfTxsPerBlock, Gen.choose(0: Value, Long.MaxValue)).sample.get
    val txsData: Seq[((Timestamp, Value), Int)] = txsTimestamp.zip(txsValues).zipWithIndex
    val txs: Seq[NothingAtStakeCoinTransaction] = txsData.foldLeft[Seq[NothingAtStakeCoinTransaction]](Seq()) {
      case (prevTxs, (pairTimestampValue, index)) =>
        val (timestamp, value) = pairTimestampValue
        NothingAtStakeCoinTransaction(
          fromPk: PrivateKey25519,
          IndexedSeq(index: Nonce),
          IndexedSeq((fromPk.publicImage, value)),
          0: Long,
          timestamp
        ) +: prevTxs
    }
    val historyWithBlock = insertBlock(NothingAtStakeCoinHistory(),
      NothingAtStakeCoinBlock(NothingAtStakeCoinBlock.GenesisBlockId, txTimestamp, fromPk, 0, emptyTx +: txs)).get
    (historyWithBlock, txsData, txs)
  }

  def insertBlock(history: NothingAtStakeCoinHistory, block: NothingAtStakeCoinBlock): Try[NothingAtStakeCoinHistory] = {
    if (history.applicable(block)) history.append(block).map(_._1)
    else Success(history)
  }

  def equalHistories(history1: NothingAtStakeCoinHistory, history2: NothingAtStakeCoinHistory): Unit = {
    assert(history1.blocks.keys.toSet == history2.blocks.keys.toSet)
    assert(history1.blocksNodeInfo.keys.toSet == history2.blocksNodeInfo.keys.toSet)
    assert(history1.bestNChains.toSet == history2.bestNChains.toSet)
    assert(history1.outputBlockLocations.keys.map(boxId => Base58.encode(boxId.array)).toSet == history2.outputBlockLocations.keys.map(boxId => Base58.encode(boxId.array)).toSet)
  }

  feature("Blocks can be appended to history") {
    scenario("GenesisBlock added to empty history") {
      Given("an empty history")
      val emptyHistory = NothingAtStakeCoinHistory()
      assert(emptyHistory.isEmpty)

      When("a block is added")
      val block = nothingAtSakeCoinBlockGenerator().sample.get
      val optHistoryWithOneBlock = insertBlock(emptyHistory, block)

      Then("the block was added successfully")
      assert(optHistoryWithOneBlock.isSuccess)
      val historyWithOneBlock = optHistoryWithOneBlock.get

      Then("the block is in history")
      newBlockCorrectlyInHistory(historyWithOneBlock, block)

      Then("there is only one block in history")
      assert(!historyWithOneBlock.isEmpty)
      assert(historyWithOneBlock.blocks.size == 1)
      assert(historyWithOneBlock.blocksNodeInfo.size == 1)
      assert(historyWithOneBlock.bestNChains.size == 1)

      Then("the block is the best chain")
      assert(historyWithOneBlock.bestNChains.size == 1)
      assert(historyWithOneBlock.bestNChains.contains(wrapId(block.id)))
    }

    scenario("Only one best blockchain") {
      Given("an empty history")
      val emptyHistory = NothingAtStakeCoinHistory()
      val numBlocks = 20
      (1 to numBlocks).foldLeft[(NothingAtStakeCoinHistory, ModifierId)](emptyHistory, Array.fill(NodeViewModifier.ModifierIdSize)(1: Byte)) {
        case ((prevHistory, prevBlockId), _) =>
          When("adding a new block")
          val block = nothingAtSakeCoinBlockGenerator(Some(prevBlockId)).sample.get
          val optNewHistory = insertBlock(prevHistory, block)

          Then("the block was added successfully")
          assert(optNewHistory.isSuccess)
          val newHistory = optNewHistory.get

          Then("the block is in history")
          newBlockCorrectlyInHistory(newHistory, block)

          Then("its parent has now 1 son")
          assert(prevHistory.isEmpty || newHistory.blocksNodeInfo(wrapId(prevBlockId)).sons.length == 1)

          Then("the new block is the only one in history.bestNChains")
          assert(newHistory.bestNChains.size == 1)
          assert(newHistory.bestNChains.head == wrapId(block.id))

          (newHistory, block.id)
      }

    }

    scenario("BlockChain forks without having to delete branches") {
      Given("a history with one block")
      val emptyHistory = NothingAtStakeCoinHistory()
      val genesisBlock = nothingAtSakeCoinBlockGenerator(Some(Array.fill(NodeViewModifier.ModifierIdSize)(1: Byte))).sample.get
      val historyWithOneBlock = insertBlock(emptyHistory, genesisBlock).get

      (1 to numberOfBestChains).foldLeft[NothingAtStakeCoinHistory](historyWithOneBlock) {
        case (prevHistory, _) =>
          When("adding new forks")
          val block = nothingAtSakeCoinBlockGenerator(Some(genesisBlock.id)).sample.get
          val optNewHistory = insertBlock(prevHistory, block)
          val newHistory = optNewHistory.get

          Then("the block was added correctly")
          newBlockCorrectlyInHistory(newHistory, block)

          Then("new history.bestNChains now also contains the new block")
          assert(prevHistory.bestNChains.size == 1 || newHistory.bestNChains.size == prevHistory.bestNChains.size + 1)
          assert(newHistory.bestNChains.contains(wrapId(block.id)))

          Then("genesisBlock has one more son")
          assert(newHistory.blocksNodeInfo(wrapId(genesisBlock.id)).sons.length ==
            prevHistory.blocksNodeInfo(wrapId(genesisBlock.id)).sons.length + 1)
          newHistory
      }
    }

    scenario("BlockChain forks and N chains limit is surpassed without recursive removal") {
      Given("a history with N chains")
      val (historyWithNChains, genesisBlock) = generateHistory(numberOfBestChains, Some(numberOfBestChains))

      When("adding an extra block that causes the removal of a chain from history")
      val block = nothingAtSakeCoinBlockGenerator(Some(genesisBlock.id), Some(Long.MaxValue)).sample.get
      val optHistoryWithBlockRemoved = insertBlock(historyWithNChains, block)

      Then("the block was added successfully")
      assert(optHistoryWithBlockRemoved.isSuccess)
      val historyWithBlockRemoved = optHistoryWithBlockRemoved.get
      newBlockCorrectlyInHistory(historyWithBlockRemoved, block)

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
      assert(historyWithBlockRemoved.blocksNodeInfo.get(idBlockAdded).isDefined)
      assert(historyWithBlockRemoved.blocksNodeInfo.get(idBlockRemoved).isEmpty)

      Then("genesisBlock has N sons")
      assert(historyWithBlockRemoved.blocksNodeInfo(wrapId(genesisBlock.id)).sons.length == numberOfBestChains)
    }

    scenario("BlockChain forks with N chains and a block is appended to history with no effect due to its coin age") {
      Given("a history with N chains")
      val (historyWithNChains, genesisBlock) = generateHistory(numberOfBestChains, Some(numberOfBestChains))

      When("adding an extra block that should cause no effect")
      val block = nothingAtSakeCoinBlockGenerator(Some(genesisBlock.id), Some(0)).sample.get
      val optHistoryWithBlockRemoved = insertBlock(historyWithNChains, block)

      Then("the append was successful")
      assert(optHistoryWithBlockRemoved.isSuccess)
      val historyWithBlockAppended = optHistoryWithBlockRemoved.get

      Then("the block was not added to the history")
      val blockByteBufferId = wrapId(block.id)
      assert(historyWithBlockAppended.blocks.get(blockByteBufferId).isEmpty)
      assert(historyWithBlockAppended.blocksNodeInfo.get(blockByteBufferId).isEmpty)
      assert(!historyWithBlockAppended.bestNChains.contains(blockByteBufferId) &&
        (historyWithBlockAppended.bestNChains diff historyWithNChains.bestNChains).isEmpty)
      assert(historyWithBlockAppended.outputBlockLocations.get(blockByteBufferId).isEmpty)
    }

    scenario("BlockChain forks and N chains limit is surpassed with recursive removal") {
      Given("A created history with 2 best chains tracking")
      val numberOfBestChains = 2
      val (history, genesisBlock) = generateHistory(2, Some(numberOfBestChains))

      When("Appending two blocks to one of the best chains")
      val newBlocksGen = nothingAtSakeCoinBlockGenerator(p = Some(history.bestNChains.head.array()), ca = Some(Long.MaxValue))
      val block1 = newBlocksGen.sample.get
      val block2 = newBlocksGen.sample.get
      val afterAppendHistory = history.append(block1).get._1.append(block2).get._1

      Then("The new history should have the last 2 added blocks")
      afterAppendHistory.blocks.size shouldEqual 4 // genesis, first child and 2 blocks manually added
      afterAppendHistory.bestNChains.size shouldEqual numberOfBestChains // be the maxValue
      afterAppendHistory.bestNChains.contains(ByteBuffer.wrap(block1.id)) shouldEqual true
      afterAppendHistory.bestNChains.contains(ByteBuffer.wrap(block2.id)) shouldEqual true
    }

    scenario("Append of a block already in history") {
      Given("a history")
      val historyWithNChains = generateHistory(numberOfBestChains)._1

      When("a block already in the history is appended")
      val blockToAppend = historyWithNChains.blocks.head._2
      val optHistoryWithBlockAppended = insertBlock(historyWithNChains, blockToAppend)

      Then("the appending was successful")
      assert(optHistoryWithBlockAppended.isSuccess)
      val historyWithBlockAppended = optHistoryWithBlockAppended.get

      Then("the new history is the same as the previous one")
      equalHistories(historyWithBlockAppended, historyWithNChains)
    }
  }

  feature("After appending, history removes a set of blocks to return to Minimal State") {
    scenario("Branch prune is performed when other blocks are created") {
      /**
        * History graph
        *                   /---[B7}
        *           /---[B1]
        *          /        \---[NEW_BLOCK_TO_ADD that will remove B4]
        *   Genesis
        *          \                  /---[B6]
        *           \---[B2]---[b3]---
        *                             \---[B4]---[B5]
        */
      Given("A created history with 3 best chains tracking")
      val numberOfBestChains = 3
      val (history, genesisBlock) = generateHistory(0, Some(numberOfBestChains))
      val newBlocksGen = nothingAtSakeCoinBlockGenerator(p = Some(genesisBlock.id), ca = Some(Long.MaxValue - 100))
      val block1 = newBlocksGen.sample.get
      val block2 = newBlocksGen.sample.get
      val block3 = nothingAtSakeCoinBlockGenerator(p = Some(block2.id), ca = Some(Long.MaxValue - 50)).sample.get
      val block4 = nothingAtSakeCoinBlockGenerator(p = Some(block3.id), ca = Some(Long.MaxValue - 25)).sample.get
      val block5 = nothingAtSakeCoinBlockGenerator(p = Some(block4.id), ca = Some(Long.MaxValue - 10)).sample.get
      val block6 = nothingAtSakeCoinBlockGenerator(p = Some(block3.id), ca = Some(Long.MaxValue - 5)).sample.get
      val block7 = nothingAtSakeCoinBlockGenerator(p = Some(block1.id), ca = Some(Long.MaxValue)).sample.get
      val beforeAppendHistory = Seq(block1, block2, block3, block4, block5, block6, block7).foldLeft[NothingAtStakeCoinHistory](history) { (h, b) =>
        h.append(b).get._1
      }

      When("Appending two blocks to one of the best chains")
      val block8 = nothingAtSakeCoinBlockGenerator(p = Some(block1.id), ca = Some(Long.MaxValue)).sample.get
      val (afterAppendHistory, rollbackTo) = beforeAppendHistory.append(block8).get

      Then("The new history should have the correct amount of elements")
      afterAppendHistory.blocks.size shouldEqual 7 // genesis, B1, B2, B3, B6, B7, B8
      Then("The new history should the correct bestNChains")
      afterAppendHistory.bestNChains.size shouldEqual numberOfBestChains // be the maxValue
      afterAppendHistory.bestNChains.contains(ByteBuffer.wrap(block6.id)) shouldEqual true
      afterAppendHistory.bestNChains.contains(ByteBuffer.wrap(block7.id)) shouldEqual true
      afterAppendHistory.bestNChains.contains(ByteBuffer.wrap(block8.id)) shouldEqual true
      Then("Rollback history should be ok")
      rollbackTo.isDefined shouldEqual true
      rollbackTo.get.to sameElements genesisBlock.id shouldEqual true
      rollbackTo.get.thrown.size shouldEqual 2
      rollbackTo.get.thrown.contains(block4) shouldEqual true
      rollbackTo.get.thrown.contains(block5) shouldEqual true
      rollbackTo.get.applied.size shouldEqual 6
      rollbackTo.get.applied.contains(block1) shouldEqual true
      rollbackTo.get.applied.contains(block2) shouldEqual true
      rollbackTo.get.applied.contains(block3) shouldEqual true
      rollbackTo.get.applied.contains(block6) shouldEqual true
      rollbackTo.get.applied.contains(block7) shouldEqual true
      rollbackTo.get.applied.contains(block8) shouldEqual true
    }
  }

  feature("tx coin age is correctly calculated") {
    scenario("tx input are all between the STAKE_MIN_AGE and STAKE_MAX_AGE period") {
      Given("a history with one block")
      val STAKE_MIN_AGE = NothingAtStakeCoinHistory.STAKE_MIN_AGE
      val STAKE_MAX_AGE = NothingAtStakeCoinHistory.STAKE_MAX_AGE
      val fromPk = keyGenerator.sample.get._1
      val txTimestamp = STAKE_MAX_AGE
      val (historyWithBlock, txsData, txs) = historyOneBlockWithTxs(0, STAKE_MAX_AGE - STAKE_MIN_AGE, fromPk, txTimestamp)

      When("a tx from the outputs of the block is created")
      val newTx = NothingAtStakeCoinTransaction(
        fromPk,
        txs.flatMap(t => t.newBoxes.map(b => b.nonce)).toIndexedSeq,
        IndexedSeq((fromPk.publicImage, 100: Value)),
        0: Long,
        STAKE_MAX_AGE
      )

      Then("the coin age of the tx obtained with getCoinAge is correct")
      val txCoinAge = txsData.foldLeft[CoinAgeLength](0) {
        case (prevCoinAge, txData) => prevCoinAge + (txTimestamp - txData._1._1) * txData._1._2 / NothingAtStakeCoinHistory.CENT
      } * NothingAtStakeCoinHistory.CENT / NothingAtStakeCoinHistory.COIN / (24 * 60 * 60)
      val maybeTxCoinAgeInHistory = historyWithBlock.getCoinAge(newTx)
      assert(maybeTxCoinAgeInHistory.isSuccess)
      val txCoinAgeInHistory = maybeTxCoinAgeInHistory.get
      assert(txCoinAge == txCoinAgeInHistory)
    }

    scenario("some tx are below STAKE_MIN_AGE") {
      Given("a history with one block")
      val STAKE_MIN_AGE = NothingAtStakeCoinHistory.STAKE_MIN_AGE
      val STAKE_MAX_AGE = NothingAtStakeCoinHistory.STAKE_MAX_AGE
      val fromPk = keyGenerator.sample.get._1
      val txTimestamp = STAKE_MAX_AGE - STAKE_MIN_AGE
      val (historyWithBlock, txsData, txs) = historyOneBlockWithTxs(0, STAKE_MAX_AGE - STAKE_MIN_AGE, fromPk, txTimestamp)

      When("a tx from the outputs of the block is created")
      val newTx = NothingAtStakeCoinTransaction(
        fromPk,
        txs.flatMap(tx => tx.newBoxes.map(box => box.nonce)).toIndexedSeq,
        IndexedSeq((fromPk.publicImage, 100: Value)),
        0: Long,
        txTimestamp
      )

      Then("the coin age of the tx obtained with getCoinAge is correct")
      val txCoinAge = txsData.foldLeft[CoinAgeLength](0) {
        case (prevCoinAge, txData) if txTimestamp - txData._1._1 < STAKE_MIN_AGE => prevCoinAge
        case (prevCoinAge, txData) if txTimestamp - txData._1._1 >= STAKE_MIN_AGE =>
          prevCoinAge + (txTimestamp - txData._1._1) * txData._1._2 / NothingAtStakeCoinHistory.CENT
      } * NothingAtStakeCoinHistory.CENT / NothingAtStakeCoinHistory.COIN / (24 * 60 * 60)
      val maybeTxCoinAgeInHistory = historyWithBlock.getCoinAge(newTx)
      assert(maybeTxCoinAgeInHistory.isSuccess)
      val txCoinAgeInHistory = maybeTxCoinAgeInHistory.get
      assert(txCoinAge == txCoinAgeInHistory)
    }

    scenario("some tx are greater than STAKE_MAX_AGE") {
      Given("a history with one block")
      val STAKE_MIN_AGE = NothingAtStakeCoinHistory.STAKE_MIN_AGE
      val STAKE_MAX_AGE = NothingAtStakeCoinHistory.STAKE_MAX_AGE
      val fromPk = keyGenerator.sample.get._1
      val txTimestamp = STAKE_MAX_AGE + 2 * STAKE_MIN_AGE
      val (historyWithBlock, txsData, txs) = historyOneBlockWithTxs(0, STAKE_MAX_AGE + STAKE_MIN_AGE, fromPk, txTimestamp)

      When("a tx from the outputs of the block is created")
      val newTx = NothingAtStakeCoinTransaction(
        fromPk,
        txs.flatMap(t => t.newBoxes.map(b => b.nonce)).toIndexedSeq,
        IndexedSeq((fromPk.publicImage, 100: Value)),
        0: Long,
        txTimestamp
      )

      Then("the coin age of the tx obtained with getCoinAge is correct")
      val txCoinAge = txsData.foldLeft[CoinAgeLength](0) { case (prevCoinAge, txData) =>
        val diffTim = txTimestamp - txData._1._1 match {
          case diffTimestamp if diffTimestamp > STAKE_MAX_AGE => STAKE_MAX_AGE
          case diffTimestamp if diffTimestamp <= STAKE_MAX_AGE => diffTimestamp
        }
        prevCoinAge + diffTim * txData._1._2 / NothingAtStakeCoinHistory.CENT
      } * NothingAtStakeCoinHistory.CENT / NothingAtStakeCoinHistory.COIN / (24 * 60 * 60)
      val maybeTxCoinAgeInHistory = historyWithBlock.getCoinAge(newTx)
      assert(maybeTxCoinAgeInHistory.isSuccess)
      val txCoinAgeInHistory = maybeTxCoinAgeInHistory.get
      assert(txCoinAge == txCoinAgeInHistory)
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
      // FIXME: doesn't work:
      // val historyWithAllBlocks = insertBlock(insertBlock(insertBlock(history, block1).get, block2).get, block3).get
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
      val block = nothingAtSakeCoinBlockGenerator(Some(history1.bestNChains.head.array()), Some(Long.MaxValue)).sample.get
      val history2 = insertBlock(history1, block).get

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

    scenario("Comparing histories with different lengths of their bestNChains") {
      Given("There is a history2")
      val (history2, genesisBlock) = generateHistory(numberOfBestChains - 1)
      Given("A block is added resulting in history1")
      val block = nothingAtSakeCoinBlockGenerator(Some(genesisBlock.id), Some(Long.MaxValue)).sample.get
      val history1 = insertBlock(history2, block).get

      When("Comparing amongst them")
      val syncInfo1 = history1.syncInfo(true)
      val syncInfo2 = history2.syncInfo(true)
      val comparisonResult1 = history1.compare(syncInfo2)
      val comparisonResult2 = history2.compare(syncInfo1)

      Then("History1 should be older")
      assert(comparisonResult1 == HistoryComparisonResult.Younger)
      Then("History2 should be younger")
      assert(comparisonResult2 == HistoryComparisonResult.Older)
    }

    scenario("Comparing histories with the same length of their bestNChains but with different blocks in them") {
      Given("There is a history")
      val (history, genesisBlock) = generateHistory(numberOfBestChains - 1)
      Given("A block is added resulting in history1")
      val block1 = nothingAtSakeCoinBlockGenerator(Some(genesisBlock.id), Some(Long.MaxValue)).sample.get
      val history1 = insertBlock(history, block1).get
      Given("A block is added resulting in history2")
      val block2 = nothingAtSakeCoinBlockGenerator(Some(genesisBlock.id), Some(0)).sample.get
      val history2 = insertBlock(history, block2).get

      When("Comparing amongst them")
      val syncInfo1 = history1.syncInfo(true)
      val syncInfo2 = history2.syncInfo(true)
      val comparisonResult1 = history1.compare(syncInfo2)
      val comparisonResult2 = history2.compare(syncInfo1)

      Then("History1 should be older")
      assert(comparisonResult1 == HistoryComparisonResult.Younger)
      Then("History2 should be younger")
      assert(comparisonResult2 == HistoryComparisonResult.Older)
    }
  }
}