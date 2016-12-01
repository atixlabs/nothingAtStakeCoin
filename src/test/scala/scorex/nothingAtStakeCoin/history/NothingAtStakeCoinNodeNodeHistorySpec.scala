package scorex.nothingAtStakeCoin.history

import java.nio.ByteBuffer

import org.scalatest.{FeatureSpec, GivenWhenThen, Matchers}
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.block.Block.Timestamp
import scorex.core.consensus.History.{HistoryComparisonResult, RollbackTo}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.PrivateKey25519
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58
import scorex.nothingAtStakeCoin.ObjectGenerators
import scorex.nothingAtStakeCoin.block.NothingAtStakeCoinBlock
import scorex.nothingAtStakeCoin.block.NothingAtStakeCoinBlock.CoinAgeLength
import scorex.nothingAtStakeCoin.consensus.{HistorySettings, NothingAtStakeCoinHistory}
import scorex.nothingAtStakeCoin.history.NothingAtStakeCoinNodeNodeHistorySpec._
import scorex.nothingAtStakeCoin.transaction.NothingAtStakeCoinTransaction
import scorex.nothingAtStakeCoin.transaction.NothingAtStakeCoinTransaction.{Nonce, Value}
import scorex.nothingAtStakeCoin.transaction.account.PublicKey25519NoncedBox

import scala.util.{Success, Try}

class NothingAtStakeCoinNodeNodeHistorySpec extends FeatureSpec
  with GivenWhenThen
  with Matchers
  with ObjectGenerators
  with ScorexLogging {

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
      val historyWithOneBlock = optHistoryWithOneBlock.get._1

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
      val fromPk = keyGenerator.sample.get._1
      val (historyWithGenesisBlock, genesisBlock, genesisHistoryUnusedBoxes, _) =
        constructHistory(
          blockNumber = 0,
          numberOfChains = None,
          timestamp = Some(0),
          fromPk = fromPk
        )
      val numBlocks = 20
      (1 to numBlocks).foldLeft[(NothingAtStakeCoinHistory, Seq[PublicKey25519NoncedBox], ModifierId)](historyWithGenesisBlock, genesisHistoryUnusedBoxes, genesisBlock.id) {
        case ((prevHistory, prevUnusedBlocks, prevBlockId), _) =>
          When("adding a new block")
          val blockTimestamp = prevHistory.blockById(prevBlockId).get.timestamp + 1
          val (block, newUnusedBlocks) = nothingAtStakeCoinBlockConstructor(
            parentId = Some(prevBlockId),
            timestamp = Some(blockTimestamp),
            history = prevHistory,
            unusedBoxes = prevUnusedBlocks,
            fromPk = fromPk
          )
          val optNewHistory = insertBlock(prevHistory, block)

          Then("the block was added successfully")
          assert(optNewHistory.isSuccess)
          val newHistory = optNewHistory.get._1

          Then("the block is in history")
          newBlockCorrectlyInHistory(newHistory, block)

          Then("its parent has now 1 son")
          assert(prevHistory.isEmpty || newHistory.blocksNodeInfo(wrapId(prevBlockId)).sons.length == 1)

          Then("the new block is the only one in history.bestNChains")
          assert(newHistory.bestNChains.size == 1)
          assert(newHistory.bestNChains.head == wrapId(block.id))

          (newHistory, newUnusedBlocks, block.id)
      }
    }

    scenario("BlockChain forks without having to delete branches") {
      Given("a history with one block")
      val fromPk = keyGenerator.sample.get._1
      val (historyWithOneBlock, genesisBlock, genesisUnusedBoxes, _) =
        constructHistory(
          blockNumber = 0,
          numberOfChains = None,
          timestamp = None,
          fromPk = fromPk
        )

      (1 to numberOfBestChains).foldLeft[(NothingAtStakeCoinHistory, Seq[PublicKey25519NoncedBox])](historyWithOneBlock, genesisUnusedBoxes) {
        case ((prevHistory, prevUnusedBoxes), _) =>
          When("adding new forks")
          val (block, newUnusedBoxes) = nothingAtStakeCoinBlockConstructor(
            parentId = Some(genesisBlock.id),
            timestamp = None,
            history = prevHistory,
            unusedBoxes = prevUnusedBoxes,
            fromPk = fromPk
          )

          val optNewHistory = insertBlock(prevHistory, block)
          val newHistory = optNewHistory.get._1

          Then("the block was added correctly")
          newBlockCorrectlyInHistory(newHistory, block)

          Then("new history.bestNChains now also contains the new block")
          assert(prevHistory.bestNChains.size == 1 || newHistory.bestNChains.size == prevHistory.bestNChains.size + 1)
          assert(newHistory.bestNChains.contains(wrapId(block.id)))

          Then("genesisBlock has one more son")
          assert(newHistory.blocksNodeInfo(wrapId(genesisBlock.id)).sons.length ==
            prevHistory.blocksNodeInfo(wrapId(genesisBlock.id)).sons.length + 1)
          (newHistory, newUnusedBoxes)
      }
    }

    scenario("BlockChain forks and N chains limit is surpassed without recursive removal") {
      Given("a history with N chains")
      val fromPk = keyGenerator.sample.get._1
      val (historyWithNChains, genesisBlock, unusedBoxes, _) =
        constructHistory(
          blockNumber = numberOfBestChains,
          numberOfChains = None,
          timestamp = Some(daysToMs(STAKE_MIN_AGE)),
          fromPk = fromPk
        )

      When("adding an extra block that causes the removal of a chain from history")
      val (block, _) = nothingAtStakeCoinBlockConstructor(
        parentId = Some(genesisBlock.id),
        timestamp = Some(daysToMs(STAKE_MAX_AGE)),
        history = historyWithNChains,
        unusedBoxes = unusedBoxes,
        fromPk = fromPk
      )
      val optHistoryWithBlockRemoved = insertBlock(historyWithNChains, block)

      Then("the block was added successfully")
      assert(optHistoryWithBlockRemoved.isSuccess)
      val historyWithBlockRemoved = optHistoryWithBlockRemoved.get._1
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
      val fromPk = keyGenerator.sample.get._1
      val (historyWithNChains, genesisBlock, unusedBoxes, outputsForExtraBlock) =
        constructHistory(
          blockNumber = numberOfBestChains,
          numberOfChains = None,
          timestamp = Some(STAKE_MIN_AGE),
          reservedGenesisBlockOutputsPerTx = 1,
          fromPk = fromPk
        )

      When("adding an extra block that should cause no effect")
      val (block, _) = nothingAtStakeCoinBlockConstructor(
        parentId = Some(genesisBlock.id),
        timestamp = Some(0),
        history = historyWithNChains,
        unusedBoxes = outputsForExtraBlock,
        fromPk = fromPk
      )
      val optHistoryWithBlockRemoved = insertBlock(historyWithNChains, block)

      Then("the append was successful")
      assert(optHistoryWithBlockRemoved.isSuccess)
      val historyWithBlockAppended = optHistoryWithBlockRemoved.get._1

      Then("the block was not added to the history")
      val blockByteBufferId = wrapId(block.id)
      assert(historyWithBlockAppended.blocks.get(blockByteBufferId).isEmpty)
      assert(historyWithBlockAppended.blocksNodeInfo.get(blockByteBufferId).isEmpty)
      assert(!historyWithBlockAppended.bestNChains.contains(blockByteBufferId) &&
        (historyWithBlockAppended.bestNChains diff historyWithNChains.bestNChains).isEmpty)
      assert(historyWithBlockAppended.outputBlockLocations.get(blockByteBufferId).isEmpty)
    }

    scenario("BlockChain forks and N chains limit is surpassed with recursive removal") {
      Given("a history with N chains")
      val fromPk = keyGenerator.sample.get._1
      val (historyWithOneBlock, genesisBlock, _, genesisUnusedBoxes) = constructHistory(
        blockNumber = 1,
        numberOfChains = Some(2),
        timestamp = Some(STAKE_MAX_AGE),
        reservedGenesisBlockOutputsPerTx = 2,
        fromPk = fromPk
      )
      val (unusedBoxesChain, reservedBoxes) = genesisUnusedBoxes.splitAt(numberOfTxsPerBlock)
      val (block1, unusedBoxesBlock1) = nothingAtStakeCoinBlockConstructor(
        parentId = Some(genesisBlock.id),
        timestamp = Some(0),
        history = historyWithOneBlock,
        unusedBoxes = unusedBoxesChain,
        fromPk = fromPk
      )
      val historyWithBlock1 = insertBlock(historyWithOneBlock, block1).get._1
      val (block2, unusedBoxesBlock2) = nothingAtStakeCoinBlockConstructor(
        parentId = Some(block1.id),
        timestamp = Some(STAKE_MIN_AGE),
        history = historyWithBlock1,
        unusedBoxes = unusedBoxesBlock1,
        fromPk = fromPk
      )
      val historyWithChain = insertBlock(historyWithBlock1, block2).get._1

      When("adding an extra block that causes the removal of a chain from history")
      val newBlock = nothingAtStakeCoinBlockConstructor(
        parentId = Some(genesisBlock.id),
        timestamp = Some(STAKE_MAX_AGE),
        history = historyWithChain,
        unusedBoxes = reservedBoxes,
        fromPk = fromPk
      )._1

      val optHistoryWithBlocksRemoved = insertBlock(historyWithChain, newBlock)

      Then("the block was added successfully")
      assert(optHistoryWithBlocksRemoved.isSuccess)
      val historyWithBlocksRemoved = optHistoryWithBlocksRemoved.get._1
      newBlockCorrectlyInHistory(historyWithBlocksRemoved, newBlock)

      Then("the number of blocks is N+1")
      assert(historyWithBlocksRemoved.blocks.size == 3)

      Then("the new history removed the correct block from history")
      val diffPrevWithNew = historyWithChain.bestNChains diff historyWithBlocksRemoved.bestNChains
      val diffNewWithPrev = historyWithBlocksRemoved.bestNChains diff historyWithChain.bestNChains
      assert(diffNewWithPrev.size == 1 && diffPrevWithNew.size == 1)
      val idBlockAdded = diffNewWithPrev.head
      val idBlockRemoved = diffPrevWithNew.head
      assert(historyWithBlocksRemoved.blocks.get(idBlockAdded).isDefined)
      assert(historyWithBlocksRemoved.blocks.get(idBlockRemoved).isEmpty)
      assert(historyWithBlocksRemoved.blocksNodeInfo.get(idBlockAdded).isDefined)
      assert(historyWithBlocksRemoved.blocksNodeInfo.get(idBlockRemoved).isEmpty)

      Then("genesisBlock has N sons")
      assert(historyWithBlocksRemoved.blocksNodeInfo(wrapId(genesisBlock.id)).sons.size == 2)
    }

    scenario("Append of a block already in history") {
      Given("a history")
      val fromPk = keyGenerator.sample.get._1
      val historyWithNChains = constructHistory(
        blockNumber = numberOfBestChains,
        numberOfChains = Some(numberOfBestChains),
        timestamp = None,
        fromPk = fromPk
      )._1

      When("a block already in the history is appended")
      val blockToAppend = historyWithNChains.blocks.head._2
      val optHistoryWithBlockAppended = insertBlock(historyWithNChains, blockToAppend)

      Then("the appending was successful")
      assert(optHistoryWithBlockAppended.isSuccess)
      val historyWithBlockAppended = optHistoryWithBlockAppended.get._1

      Then("the new history is the same as the previous one")
      equalHistories(historyWithBlockAppended, historyWithNChains)
    }
  }

  feature("After appending, history removes a set of blocks to return to Minimal State") {
    scenario("Branch prune is performed when other blocks are created") {
      // @formatter:off
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
      // @formatter:on
      Given("A created history with 3 best chains tracking")
      val numberOfBestChains = 3
      val fromPk = keyGenerator.sample.get._1
      val (history, genesisBlock, _, unusedBoxesHistory) = constructHistory(
        blockNumber = 0,
        numberOfChains = Some(numberOfBestChains),
        timestamp = None,
        reservedGenesisBlockOutputsPerTx = 8,
        fromPk = fromPk
      )
      val newBlocksGen = nothingAtSakeCoinBlockGenerator(p = Some(genesisBlock.id), ca = Some(Long.MaxValue - 100))
      val (block1, _) = nothingAtStakeCoinBlockConstructor(
        parentId = Some(genesisBlock.id),
        timestamp = Some(0),
        history = history,
        unusedBoxes = unusedBoxesHistory.take(numberOfTxsPerBlock),
        fromPk = fromPk
      )
      val unusedBoxesBlock1 = unusedBoxesHistory.drop(numberOfTxsPerBlock)
      val historyBlock1 = insertBlock(history, block1).get._1
      val (block2, _) = nothingAtStakeCoinBlockConstructor(
        parentId = Some(genesisBlock.id),
        timestamp = Some(0),
        history = historyBlock1,
        unusedBoxes = unusedBoxesBlock1.take(numberOfTxsPerBlock),
        fromPk = fromPk
      )
      val unusedBoxesBlock2 = unusedBoxesBlock1.drop(numberOfTxsPerBlock)
      val historyBlock2 = insertBlock(historyBlock1, block2).get._1
      val (block3, _) = nothingAtStakeCoinBlockConstructor(
        parentId = Some(block2.id),
        timestamp = Some(0),
        history = historyBlock2,
        unusedBoxes = unusedBoxesBlock2.take(numberOfTxsPerBlock),
        fromPk = fromPk
      )
      val unusedBoxesBlock3 = unusedBoxesBlock2.drop(numberOfTxsPerBlock)
      val historyBlock3 = insertBlock(historyBlock2, block3).get._1
      val timestampStepping = (STAKE_MAX_AGE - STAKE_MIN_AGE) / 4 + STAKE_MIN_AGE
      val (block4, _) = nothingAtStakeCoinBlockConstructor(
        parentId = Some(block3.id),
        timestamp = Some(0),
        history = historyBlock3,
        unusedBoxes = unusedBoxesBlock3.take(numberOfTxsPerBlock),
        fromPk = fromPk
      )
      val unusedBoxesBlock4 = unusedBoxesBlock3.drop(numberOfTxsPerBlock)
      val historyBlock4 = insertBlock(historyBlock3, block4).get._1
      val (block5, _) = nothingAtStakeCoinBlockConstructor(
        parentId = Some(block4.id),
        timestamp = Some(timestampStepping + STAKE_MIN_AGE),
        history = historyBlock4,
        unusedBoxes = unusedBoxesBlock4.take(numberOfTxsPerBlock),
        fromPk = fromPk
      )
      val unusedBoxesBlock5 = unusedBoxesBlock4.drop(numberOfTxsPerBlock)
      val historyBlock5 = insertBlock(historyBlock4, block5).get._1
      val (block6, _) = nothingAtStakeCoinBlockConstructor(
        parentId = Some(block3.id),
        timestamp = Some(2 * timestampStepping + STAKE_MIN_AGE),
        history = historyBlock5,
        unusedBoxes = unusedBoxesBlock5.take(numberOfTxsPerBlock),
        fromPk = fromPk
      )
      val unusedBoxesBlock6 = unusedBoxesBlock5.drop(numberOfTxsPerBlock)
      val historyBlock6 = insertBlock(historyBlock5, block6).get._1
      val (block7, _) = nothingAtStakeCoinBlockConstructor(
        parentId = Some(block1.id),
        timestamp = Some(3 * timestampStepping + STAKE_MIN_AGE),
        history = historyBlock6,
        unusedBoxes = unusedBoxesBlock6.take(numberOfTxsPerBlock),
        fromPk = fromPk
      )
      val unusedBoxesBlock7 = unusedBoxesBlock6.drop(numberOfTxsPerBlock)
      val beforeAppendHistory = insertBlock(historyBlock6, block7).get._1

      When("Appending two blocks to one of the best chains")
      val (block8, _) = nothingAtStakeCoinBlockConstructor(
        parentId = Some(block1.id),
        timestamp = Some(STAKE_MAX_AGE),
        history = beforeAppendHistory,
        unusedBoxes = unusedBoxesBlock7,
        fromPk = fromPk
      )
      val (afterAppendHistory, rollbackTo) = insertBlock(beforeAppendHistory, block8).get

      Then("The new history should have the correct amount of elements")
      afterAppendHistory.blocks.size shouldEqual 7 // genesis, B1, B2, B3, B6, B7, B8

      Then("The new history should the correct bestNChains")
      afterAppendHistory.bestNChains.size shouldEqual numberOfBestChains // be the maxValue
      for (block <- Seq(block6, block7, block8))
        afterAppendHistory.bestNChains.contains(ByteBuffer.wrap(block.id)) shouldEqual true

      Then("Rollback history should be ok")
      rollbackTo.isDefined shouldEqual true
      rollbackTo.get.to sameElements genesisBlock.id shouldEqual true
      rollbackTo.get.thrown.size shouldEqual 2
      for (thrown <- Seq(block4, block5))
        rollbackTo.get.thrown.contains(thrown) shouldEqual true
      rollbackTo.get.applied.size shouldEqual 6
      for ((appliedBlock, block) <- rollbackTo.get.applied.zip(Seq(block1, block2, block3, block6, block7, block8)))
        appliedBlock.id sameElements block.id shouldEqual true
    }
  }

  feature("tx coin age is correctly calculated") {
    /*
     *  We use:   STAKE_MIN_AGE = 30
     *            STAKE_MAX_AGE = 90
     *            numberOfTxsPerBlock = 10
     */
    scenario("tx input has timestamp less than STAKE_MIN_AGE") {
      Given("a history with a genesis block with tx of timestamp 0")
      val daysToMs = NothingAtStakeCoinHistory.daysToMs
      val fromPk = keyGenerator.sample.get._1
      val genesisBlockTxTimestampDays = 0
      val genesisBlockTxValue = 2000000
      val (history, genesisBlock) = historyOneBlockWithTxs(genesisBlockTxTimestampDays * daysToMs, fromPk, genesisBlockTxValue)

      When("a tx from the outputs of genesis block is created")
      val txTimestampDays = 15
      val genesisBlockGeneratedBoxes = genesisBlock.txs.flatMap(t => t.newBoxes)
      val newTx = NothingAtStakeCoinTransaction(
        fromPk,
        genesisBlockGeneratedBoxes.map(b => b.nonce).toIndexedSeq,
        IndexedSeq((fromPk.publicImage, genesisBlockGeneratedBoxes.map(b => b.value).sum)),
        0: Long,
        txTimestampDays * daysToMs
      )

      Then("the coinAge from the new tx is correct")
      val maybeTxCoinAge = history.getCoinAge(newTx)
      assert(maybeTxCoinAge.isSuccess)
      val txCoinAge = maybeTxCoinAge.get
      txCoinAge should not equal 300 //Value if the calculation doesn't take into account the 30 days limit
      txCoinAge shouldEqual 0
    }

    scenario("tx input has timestamp equal to STAKE_MIN_AGE-1") {
      Given("a history with a genesis block with tx of timestamp 0")
      val daysToMs = NothingAtStakeCoinHistory.daysToMs
      val fromPk = keyGenerator.sample.get._1
      val genesisBlockTxTimestampDays = 0
      val genesisBlockTxValue = 2000000
      val (history, genesisBlock) = historyOneBlockWithTxs(genesisBlockTxTimestampDays * daysToMs, fromPk, genesisBlockTxValue)

      When("a tx from the outputs of genesis block is created")
      val txTimestamp = 30 * daysToMs - 1
      val genesisBlockGeneratedBoxes = genesisBlock.txs.flatMap(t => t.newBoxes)
      val newTx = NothingAtStakeCoinTransaction(
        fromPk,
        genesisBlockGeneratedBoxes.map(b => b.nonce).toIndexedSeq,
        IndexedSeq((fromPk.publicImage, genesisBlockGeneratedBoxes.map(b => b.value).sum)),
        0: Long,
        txTimestamp
      )

      Then("the coinAge from the new tx is correct")
      val maybeTxCoinAge = history.getCoinAge(newTx)
      assert(maybeTxCoinAge.isSuccess)
      val txCoinAge = maybeTxCoinAge.get
      txCoinAge should not equal 599 //Value if the calculation doesn't take into account the 30 days limit
      txCoinAge shouldEqual 0
    }

    scenario("tx input has timestamp equal to STAKE_MIN_AGE") {
      Given("a history with a genesis block with tx of timestamp 0")
      val daysToMs = NothingAtStakeCoinHistory.daysToMs
      val fromPk = keyGenerator.sample.get._1
      val genesisBlockTxTimestampDays = 0
      val genesisBlockTxValue = 2000000
      val (history, genesisBlock) = historyOneBlockWithTxs(genesisBlockTxTimestampDays * daysToMs, fromPk, genesisBlockTxValue)

      When("a tx from the outputs of genesis block is created")
      val txTimestampDays = 30
      val genesisBlockGeneratedBoxes = genesisBlock.txs.flatMap(t => t.newBoxes)
      val newTx = NothingAtStakeCoinTransaction(
        fromPk,
        genesisBlockGeneratedBoxes.map(b => b.nonce).toIndexedSeq,
        IndexedSeq((fromPk.publicImage, genesisBlockGeneratedBoxes.map(b => b.value).sum)),
        0: Long,
        txTimestampDays * daysToMs
      )

      Then("the coinAge from the new tx is correct")
      val maybeTxCoinAge = history.getCoinAge(newTx)
      assert(maybeTxCoinAge.isSuccess)
      val txCoinAge = maybeTxCoinAge.get
      txCoinAge shouldEqual 600
    }

    scenario("tx input has timestamp equal to STAKE_MIN_AGE+1") {
      Given("a history with a genesis block with tx of timestamp 0")
      val daysToMs = NothingAtStakeCoinHistory.daysToMs
      val fromPk = keyGenerator.sample.get._1
      val genesisBlockTxTimestampDays = 0
      val genesisBlockTxValue = 2000000
      val (history, genesisBlock) = historyOneBlockWithTxs(genesisBlockTxTimestampDays * daysToMs, fromPk, genesisBlockTxValue)

      When("a tx from the outputs of genesis block is created")
      val txTimestamp = 30 * daysToMs + 1
      val genesisBlockGeneratedBoxes = genesisBlock.txs.flatMap(t => t.newBoxes)
      val newTx = NothingAtStakeCoinTransaction(
        fromPk,
        genesisBlockGeneratedBoxes.map(b => b.nonce).toIndexedSeq,
        IndexedSeq((fromPk.publicImage, genesisBlockGeneratedBoxes.map(b => b.value).sum)),
        0: Long,
        txTimestamp
      )

      Then("the coinAge from the new tx is correct")
      val maybeTxCoinAge = history.getCoinAge(newTx)
      assert(maybeTxCoinAge.isSuccess)
      val txCoinAge = maybeTxCoinAge.get
      txCoinAge shouldEqual 600
    }

    scenario("tx input has timestamp greater than STAKE_MIN_AGE and less to STAKE_MAX_AGE") {
      Given("a history with a genesis block with tx of timestamp 0")
      val daysToMs = NothingAtStakeCoinHistory.daysToMs
      val fromPk = keyGenerator.sample.get._1
      val genesisBlockTxTimestampDays = 0
      val genesisBlockTxValue = 2000000
      val (history, genesisBlock) = historyOneBlockWithTxs(genesisBlockTxTimestampDays * daysToMs, fromPk, genesisBlockTxValue)

      When("a tx from the outputs of genesis block is created")
      val txTimestampDays = 60
      val genesisBlockGeneratedBoxes = genesisBlock.txs.flatMap(t => t.newBoxes)
      val newTx = NothingAtStakeCoinTransaction(
        fromPk,
        genesisBlockGeneratedBoxes.map(b => b.nonce).toIndexedSeq,
        IndexedSeq((fromPk.publicImage, genesisBlockGeneratedBoxes.map(b => b.value).sum)),
        0: Long,
        txTimestampDays * daysToMs
      )

      Then("the coinAge from the new tx is correct")
      val maybeTxCoinAge = history.getCoinAge(newTx)
      assert(maybeTxCoinAge.isSuccess)
      val txCoinAge = maybeTxCoinAge.get
      txCoinAge shouldEqual 1200
    }

    scenario("tx input has timestamp equal to STAKE_MAX_AGE-1") {
      Given("a history with a genesis block with tx of timestamp 0")
      val daysToMs = NothingAtStakeCoinHistory.daysToMs
      val fromPk = keyGenerator.sample.get._1
      val genesisBlockTxTimestampDays = 0
      val genesisBlockTxValue = 2000000
      val (history, genesisBlock) = historyOneBlockWithTxs(genesisBlockTxTimestampDays * daysToMs, fromPk, genesisBlockTxValue)

      When("a tx from the outputs of genesis block is created")
      val txTimestamp = 90 * daysToMs - 1
      val genesisBlockGeneratedBoxes = genesisBlock.txs.flatMap(t => t.newBoxes)
      val newTx = NothingAtStakeCoinTransaction(
        fromPk,
        genesisBlockGeneratedBoxes.map(b => b.nonce).toIndexedSeq,
        IndexedSeq((fromPk.publicImage, genesisBlockGeneratedBoxes.map(b => b.value).sum)),
        0: Long,
        txTimestamp
      )

      Then("the coinAge from the new tx is correct")
      val maybeTxCoinAge = history.getCoinAge(newTx)
      assert(maybeTxCoinAge.isSuccess)
      val txCoinAge = maybeTxCoinAge.get
      txCoinAge shouldEqual 1799
    }

    scenario("tx input has timestamp equal to STAKE_MAX_AGE") {
      Given("a history with a genesis block with tx of timestamp 0")
      val daysToMs = NothingAtStakeCoinHistory.daysToMs
      val fromPk = keyGenerator.sample.get._1
      val genesisBlockTxTimestampDays = 0
      val genesisBlockTxValue = 2000000
      val (history, genesisBlock) = historyOneBlockWithTxs(genesisBlockTxTimestampDays * daysToMs, fromPk, genesisBlockTxValue)

      When("a tx from the outputs of genesis block is created")
      val txTimestampDays = 90
      val genesisBlockGeneratedBoxes = genesisBlock.txs.flatMap(t => t.newBoxes)
      val newTx = NothingAtStakeCoinTransaction(
        fromPk,
        genesisBlockGeneratedBoxes.map(b => b.nonce).toIndexedSeq,
        IndexedSeq((fromPk.publicImage, genesisBlockGeneratedBoxes.map(b => b.value).sum)),
        0: Long,
        txTimestampDays * daysToMs
      )

      Then("the coinAge from the new tx is correct")
      val maybeTxCoinAge = history.getCoinAge(newTx)
      assert(maybeTxCoinAge.isSuccess)
      val txCoinAge = maybeTxCoinAge.get
      txCoinAge shouldEqual 1800
    }

    scenario("tx input has timestamp equal to STAKE_MAX_AGE+1") {
      Given("a history with a genesis block with tx of timestamp 0")
      val daysToMs = NothingAtStakeCoinHistory.daysToMs
      val fromPk = keyGenerator.sample.get._1
      val genesisBlockTxTimestampDays = 0
      val genesisBlockTxValue = 2000000
      val (history, genesisBlock) = historyOneBlockWithTxs(genesisBlockTxTimestampDays * daysToMs, fromPk, genesisBlockTxValue)

      When("a tx from the outputs of genesis block is created")
      val txTimestamp = 90 * daysToMs + 1
      val genesisBlockGeneratedBoxes = genesisBlock.txs.flatMap(t => t.newBoxes)
      val newTx = NothingAtStakeCoinTransaction(
        fromPk,
        genesisBlockGeneratedBoxes.map(b => b.nonce).toIndexedSeq,
        IndexedSeq((fromPk.publicImage, genesisBlockGeneratedBoxes.map(b => b.value).sum)),
        0: Long,
        txTimestamp
      )

      Then("the coinAge from the new tx is correct")
      val maybeTxCoinAge = history.getCoinAge(newTx)
      assert(maybeTxCoinAge.isSuccess)
      val txCoinAge = maybeTxCoinAge.get
      txCoinAge shouldEqual 1800
    }

    scenario("tx input has timestamp greater than STAKE_MAX_AGE") {
      Given("a history with a genesis block with tx of timestamp 0")
      val daysToMs = NothingAtStakeCoinHistory.daysToMs
      val fromPk = keyGenerator.sample.get._1
      val genesisBlockTxTimestampDays = 0
      val genesisBlockTxValue = 2000000
      val (history, genesisBlock) = historyOneBlockWithTxs(genesisBlockTxTimestampDays * daysToMs, fromPk, genesisBlockTxValue)

      When("a tx from the outputs of genesis block is created")
      val txTimestampDays = 120
      val genesisBlockGeneratedBoxes = genesisBlock.txs.flatMap(t => t.newBoxes)
      val newTx = NothingAtStakeCoinTransaction(
        fromPk,
        genesisBlockGeneratedBoxes.map(b => b.nonce).toIndexedSeq,
        IndexedSeq((fromPk.publicImage, genesisBlockGeneratedBoxes.map(b => b.value).sum)),
        0: Long,
        txTimestampDays * daysToMs
      )

      Then("the coinAge from the new tx is correct")
      val maybeTxCoinAge = history.getCoinAge(newTx)
      assert(maybeTxCoinAge.isSuccess)
      val txCoinAge = maybeTxCoinAge.get
      txCoinAge should not equal 2400 //Value if the calculation doesn't take into account the 90 days limit
      txCoinAge shouldEqual 1800
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
      val history = constructHistory(
        blockNumber = numberOfBestChains,
        numberOfChains = Some(numberOfBestChains),
        timestamp = Some(0),
        fromPk = keyGenerator.sample.get._1
      )._1

      When("Asking for the continuation of a leaf node")
      val leafId = history.bestNChains.head.array()
      val continuationIds = history.continuationIds(from = Seq(NothingAtStakeCoinBlock.ModifierTypeId -> leafId), size = 10)

      Then("History should return none")
      continuationIds shouldBe None
    }

    scenario("ContinuationIds from genesis block") {
      Given("A loaded history")
      val (history, genesisBlock, _, _) = constructHistory(
        blockNumber = numberOfBestChains,
        numberOfChains = Some(numberOfBestChains),
        timestamp = Some(0),
        fromPk = keyGenerator.sample.get._1
      )

      When("Asking for the continuation for the genesisBlockId")
      val continuationIds = history.continuationIds(from = Seq(NothingAtStakeCoinBlock.ModifierTypeId -> genesisBlock.id), size = numberOfBestChains / 2)

      Then("History should return all nodes ids")
      continuationIds.isDefined shouldBe true
      continuationIds.get.size shouldBe numberOfBestChains / 2
    }

    scenario("ContinuationIds after chaining blocks") {
      Given("A loaded history")
      val fromPk = keyGenerator.sample.get._1
      val (history, genesisBlock, unusedBoxesHistory, _) = constructHistory(
        blockNumber = numberOfBestChains,
        numberOfChains = Some(numberOfBestChains),
        timestamp = Some(0),
        fromPk = fromPk
      )

      When("Chaining some blocks")
      val idToLookFor = history.bestNChains.head.array()
      val (block1, unusedBoxesBlock1) = nothingAtStakeCoinBlockConstructor(
        parentId = Some(idToLookFor),
        timestamp = Some(STAKE_MIN_AGE),
        history = history,
        unusedBoxes = unusedBoxesHistory,
        fromPk = fromPk
      )
      val historyWithBlock1 = insertBlock(history, block1).get._1
      val (block2, unusedBoxesBlock2) = nothingAtStakeCoinBlockConstructor(
        parentId = Some(block1.id),
        timestamp = Some(STAKE_MIN_AGE),
        history = historyWithBlock1,
        unusedBoxes = unusedBoxesBlock1,
        fromPk = fromPk
      )
      val historyWithBlock2 = insertBlock(historyWithBlock1, block2).get._1
      val (block3, unusedBoxesBlock3) = nothingAtStakeCoinBlockConstructor(
        parentId = Some(block2.id),
        timestamp = Some(STAKE_MIN_AGE),
        history = historyWithBlock2,
        unusedBoxes = unusedBoxesBlock2,
        fromPk = fromPk
      )
      val historyWithAllBlocks = insertBlock(historyWithBlock2, block3).get._1

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
      val history = constructHistory(
        blockNumber = numberOfBestChains,
        numberOfChains = Some(numberOfBestChains),
        timestamp = Some(0),
        fromPk = keyGenerator.sample.get._1
      )._1

      When("Comparing with itself")
      val syncInfo = history.syncInfo(true)
      val comparisonResult = history.compare(syncInfo)

      Then("History should return none")
      assert(comparisonResult == HistoryComparisonResult.Equal)
    }

    scenario("Comparing a chain with one more block") {
      Given("There is a history1")
      val fromPk = keyGenerator.sample.get._1
      val (history1, _, unusedBoxes, genesisUnusedBoxes) = constructHistory(
        blockNumber = numberOfBestChains,
        numberOfChains = Some(numberOfBestChains),
        timestamp = Some(0),
        reservedGenesisBlockOutputsPerTx = 1,
        fromPk = fromPk
      )
      Given("A block is added resulting in history2")
      val block = nothingAtStakeCoinBlockConstructor(
        parentId = Some(history1.bestNChains.head.array()),
        timestamp = Some(STAKE_MAX_AGE),
        history = history1,
        unusedBoxes = genesisUnusedBoxes,
        fromPk = fromPk
      )._1
      val history2 = insertBlock(history1, block).get._1

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
      val fromPk = keyGenerator.sample.get._1
      val (history2, genesisBlock, unusedBoxes, _) = constructHistory(
        blockNumber = numberOfBestChains - 1,
        numberOfChains = Some(numberOfBestChains),
        timestamp = Some(0),
        fromPk = fromPk
      )

      Given("A block is added resulting in history1")
      val block = nothingAtStakeCoinBlockConstructor(
        parentId = Some(genesisBlock.id),
        timestamp = Some(STAKE_MAX_AGE),
        history = history2,
        unusedBoxes = unusedBoxes,
        fromPk = fromPk
      )._1
      val history1 = insertBlock(history2, block).get._1

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
      val fromPk = keyGenerator.sample.get._1
      val (history, genesisBlock, unusedBoxes, _) = constructHistory(
        blockNumber = numberOfBestChains - 1,
        numberOfChains = Some(numberOfBestChains),
        timestamp = Some(0),
        fromPk = fromPk
      )
      Given("A block is added resulting in history1")
      val block1 = nothingAtStakeCoinBlockConstructor(
        parentId = Some(genesisBlock.id),
        timestamp = Some(STAKE_MAX_AGE),
        history = history,
        unusedBoxes = unusedBoxes,
        fromPk = fromPk
      )._1
      //      val block1 = nothingAtSakeCoinBlockGenerator(Some(genesisBlock.id), Some(Long.MaxValue)).sample.get
      val history1 = insertBlock(history, block1).get._1
      Given("A block is added resulting in history2")
      val block2 = nothingAtStakeCoinBlockConstructor(
        parentId = Some(genesisBlock.id),
        timestamp = Some(0),
        history = history,
        unusedBoxes = unusedBoxes,
        fromPk = fromPk
      )._1
      //      val block2 = nothingAtSakeCoinBlockGenerator(Some(genesisBlock.id), Some(0)).sample.get
      val history2 = insertBlock(history, block2).get._1

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

  def newBlockCorrectlyInHistory(history: NothingAtStakeCoinHistory, block: NothingAtStakeCoinBlock): Unit = {
    val byteBufferBlockId = wrapId(block.id)
    assert(history.blocks.get(byteBufferBlockId).isDefined)
    val blockInHistory = history.blocks(byteBufferBlockId)
    blockInHistory shouldEqual block
    assert(history.blocksNodeInfo.get(byteBufferBlockId).isDefined)
    val blockNodeInfo = history.blocksNodeInfo(byteBufferBlockId)
    assert(blockNodeInfo.sons.isEmpty)
  }
}

object NothingAtStakeCoinNodeNodeHistorySpec {

  val numberOfBestChains: Int = 10
  val numberOfTxsPerBlock: Int = 10
  val STAKE_MIN_AGE: Long = 30 * 60 * 60 * 24 * 1000L
  val STAKE_MAX_AGE: Long = 90 * 60 * 60 * 24 * 1000L

  def wrapId(bytes: Array[Byte]): ByteBuffer = ByteBuffer.wrap(bytes)

  def historyOneBlockWithTxs(timestamp: Timestamp, fromPk: PrivateKey25519, value: Value): (NothingAtStakeCoinHistory, NothingAtStakeCoinBlock) = {
    val txs: Seq[NothingAtStakeCoinTransaction] = (1 to numberOfTxsPerBlock).foldLeft[Seq[NothingAtStakeCoinTransaction]](Seq()) {
      (prevTxs, index) =>
        NothingAtStakeCoinTransaction(
          fromPk: PrivateKey25519,
          IndexedSeq(index: Nonce),
          IndexedSeq((fromPk.publicImage, value)),
          0: Long,
          timestamp
        ) +: prevTxs
    }

    val emptyTx = NothingAtStakeCoinTransaction(IndexedSeq(), IndexedSeq(), IndexedSeq(), 0, 0)
    val genesisBlock = NothingAtStakeCoinBlock(NothingAtStakeCoinBlock.GenesisParentBlockId, timestamp, fromPk, 0, emptyTx +: txs)
    val history = insertBlock(NothingAtStakeCoinHistory(), genesisBlock).get._1
    (history, genesisBlock)
  }

  def insertBlock(history: NothingAtStakeCoinHistory,
                  block: NothingAtStakeCoinBlock): Try[(NothingAtStakeCoinHistory, Option[RollbackTo[NothingAtStakeCoinBlock]])] = {
    if (history.applicable(block)) history.append(block)
    else Success(history, None)
  }

  def equalHistories(history1: NothingAtStakeCoinHistory, history2: NothingAtStakeCoinHistory): Unit = {
    assert(history1.blocks.keys.toSet == history2.blocks.keys.toSet)
    assert(history1.blocksNodeInfo.keys.toSet == history2.blocksNodeInfo.keys.toSet)
    assert(history1.bestNChains.toSet == history2.bestNChains.toSet)
    assert(history1.outputBlockLocations.keys.map(boxId => Base58.encode(boxId.array)).toSet == history2.outputBlockLocations.keys.map(boxId => Base58.encode(boxId.array)).toSet)
  }

  def daysToMs(days: Long): Long = days * NothingAtStakeCoinHistory.daysToMs

  /* Constructor functions */
  def nothingAtStakeCoinTransactionConstructor(timestamp: Timestamp,
                                               unusedBoxes: Seq[PublicKey25519NoncedBox],
                                               numberOfOutputsPerTx: Int = 1,
                                               fromPk: PrivateKey25519): (NothingAtStakeCoinTransaction, Seq[PublicKey25519NoncedBox]) = {
    val inputBox = unusedBoxes.head
    val outputsWithoutLast = (1 until numberOfOutputsPerTx).map(_ => (fromPk.publicImage, inputBox.value / numberOfOutputsPerTx))
    val outputs: Seq[(PublicKey25519Proposition, Value)] = (fromPk.publicImage, inputBox.value - outputsWithoutLast.map(_._2).sum) +: outputsWithoutLast
    val tx = NothingAtStakeCoinTransaction(
      fromPk = fromPk,
      from = Seq(inputBox.nonce).toIndexedSeq,
      to = outputs.toIndexedSeq,
      fee = 0,
      timestamp = timestamp
    )
    val newUnusedBoxes = unusedBoxes.tail ++ tx.newBoxes
    (tx, newUnusedBoxes)
  }

  def nothingAtStakeCoinTransactionSeqConstructor(numberTxs: Int,
                                                  timestamp: Timestamp,
                                                  unusedBoxes: Seq[PublicKey25519NoncedBox],
                                                  numberOfOutputsPerTx: Int = 1,
                                                  fromPk: PrivateKey25519
                                                 ): (Seq[NothingAtStakeCoinTransaction], Seq[PublicKey25519NoncedBox]) = {
    (1 to numberTxs).foldLeft[(Seq[NothingAtStakeCoinTransaction], Seq[PublicKey25519NoncedBox])](Seq(), unusedBoxes) {
      case ((prevTxs, prevUnusedBoxes), _) =>
        val (tx, newUnusedBoxes) = nothingAtStakeCoinTransactionConstructor(timestamp, prevUnusedBoxes, numberOfOutputsPerTx, fromPk)
        (tx +: prevTxs, newUnusedBoxes)
    }
  }

  def nothingAtStakeCoinBlockConstructor(parentId: Option[ModifierId],
                                         timestamp: Option[Timestamp] = None,
                                         history: NothingAtStakeCoinHistory,
                                         unusedBoxes: Seq[PublicKey25519NoncedBox],
                                         numberOfOutputsPerTx: Int = 1,
                                         fromPk: PrivateKey25519
                                        ): (NothingAtStakeCoinBlock, Seq[PublicKey25519NoncedBox]) = {
    //If timestamp.isEmpty then it will have a timestamp higher than the rest of the blocks in history
    val blockTimestamp: Timestamp = timestamp.getOrElse(history.blocks.map(_._2.timestamp).max) + (1: Timestamp)
    val (txs, newUnusedBoxes) = nothingAtStakeCoinTransactionSeqConstructor(numberOfTxsPerBlock, blockTimestamp, unusedBoxes, numberOfOutputsPerTx, fromPk)
    val coinStakeTx = NothingAtStakeCoinTransaction(IndexedSeq(), IndexedSeq(), IndexedSeq(), 0, blockTimestamp)
    val blockCoinAge: CoinAgeLength = if (parentId.isDefined) history.getCoinAge(txs).get else 0
    val coinAgeParent: CoinAgeLength = if (parentId.isDefined) history.blocks(ByteBuffer.wrap(parentId.get)).coinAge else 0
    (NothingAtStakeCoinBlock(
      //If parentId.isEmpty then we are creating a genesis block
      parentId = parentId.getOrElse(NothingAtStakeCoinBlock.GenesisParentBlockId),
      timestamp = blockTimestamp,
      generatorKeys = fromPk,
      coinAge = blockCoinAge + coinAgeParent,
      txs = coinStakeTx +: txs
    ), newUnusedBoxes)
  }

  def constructHistory(blockNumber: Int,
                       numberOfChains: Option[Int],
                       timestamp: Option[Timestamp],
                       reservedGenesisBlockOutputsPerTx: Int = 0,
                       fromPk: PrivateKey25519): (NothingAtStakeCoinHistory, NothingAtStakeCoinBlock, Seq[PublicKey25519NoncedBox], Seq[PublicKey25519NoncedBox]) = {
    val emptyHistory = NothingAtStakeCoinHistory(HistorySettings(numberOfBestChains = numberOfChains.getOrElse(numberOfBestChains)))
    val genesisBoxes: Seq[PublicKey25519NoncedBox] = (1 to numberOfTxsPerBlock).map(nonce => PublicKey25519NoncedBox(fromPk.publicImage, nonce, 2000000))
    val (genesisBlock: NothingAtStakeCoinBlock, genesisUnusedBoxes: Seq[PublicKey25519NoncedBox]) = nothingAtStakeCoinBlockConstructor(
      parentId = None,
      timestamp = Some(0: Timestamp),
      history = emptyHistory,
      unusedBoxes = genesisBoxes,
      numberOfOutputsPerTx = reservedGenesisBlockOutputsPerTx + 1,
      fromPk = fromPk
    )
    val (reservedGenesisUnusedBoxes, notReservedGenesisUnusedBoxes) =
      genesisUnusedBoxes.splitAt(reservedGenesisBlockOutputsPerTx * numberOfTxsPerBlock)
    val historyWithOneBlock = insertBlock(emptyHistory, genesisBlock).get._1
    val (historyWithNChains, unusedBoxes) =
      (1 to blockNumber).foldLeft[(NothingAtStakeCoinHistory, Seq[PublicKey25519NoncedBox])](historyWithOneBlock, notReservedGenesisUnusedBoxes) {
        case ((prevHistory, prevUnusedBoxes), _) =>
          val (block, newUnusedBoxes) =
            nothingAtStakeCoinBlockConstructor(
              parentId = Some(genesisBlock.id),
              timestamp = timestamp,
              history = prevHistory,
              unusedBoxes = prevUnusedBoxes,
              numberOfOutputsPerTx = 1,
              fromPk = fromPk
            )
          val newHistory = insertBlock(prevHistory, block).get._1
          (newHistory, newUnusedBoxes)
      }
    (historyWithNChains, genesisBlock, unusedBoxes, reservedGenesisUnusedBoxes)
  }
}