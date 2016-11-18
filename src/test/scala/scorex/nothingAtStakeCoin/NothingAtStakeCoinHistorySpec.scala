package scorex.nothingAtStakeCoin.history

import java.nio.ByteBuffer

import org.scalatest.{FeatureSpec, GivenWhenThen, Matchers}
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
    assert(history.blocks.get(ByteBuffer.wrap(block.id)).isDefined)
    val blockInHistory = history.blocks(ByteBuffer.wrap(block.id))
    blockInHistory shouldEqual block
    assert(history.blocksInfo.get(ByteBuffer.wrap(block.id)).isDefined)
    val blockInfoInHistory = history.blocksInfo(ByteBuffer.wrap(block.id))
    assert(blockInfoInHistory.sons==0)
    assert(history.blocks.size==1 ||
      (history.blocks.get(ByteBuffer.wrap(block.parentId)).isDefined &&
        history.blocksInfo(ByteBuffer.wrap(block.parentId)).totalCoinAge + block.coinAge == history.blocksInfo(ByteBuffer.wrap(block.id)).totalCoinAge))
  }

  feature("History well implemented"){
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
      val blockInfoInHistory = historyWithOneBlock.blocksInfo(ByteBuffer.wrap(block.id))

      Then("the block has the correct totalCoinAge")
      assert(blockInfoInHistory.totalCoinAge==block.coinAge)

      Then("the block is the best chain")
      assert(historyWithOneBlock.bestNChains.contains(ByteBuffer.wrap(block.id)))
    }
    scenario("Only one best blockchain"){
      Given("a history with one block")
      val emptyHistory = NothingAtStakeCoinHistory()
      val keyPair = keyGenerator.sample.get
      val firstBlock : NothingAtStakeCoinBlock = NothingAtStakeCoinBlockCompanion.signBlock(keyPair._1,
        nothingAtSakeCoinBlockGenerator.sample.get.copy(generator = keyPair._2))
      val historyWithOneBlock = emptyHistory.append(firstBlock).get._1

      When("a second block is added")
      val secondBlock : NothingAtStakeCoinBlock = NothingAtStakeCoinBlockCompanion.signBlock(keyPair._1,
        nothingAtSakeCoinBlockGenerator.sample.get.copy(generator = keyPair._2))

      Then("the second block is in history")
      newBlockCorrectlyInHistory(historyWithOneBlock, secondBlock)

      Then("its parent has now 1 son")

      Then("the second block is the only one in history.bestNChains")

    }
    scenario("Only blockchain forked"){

    }
  }
}