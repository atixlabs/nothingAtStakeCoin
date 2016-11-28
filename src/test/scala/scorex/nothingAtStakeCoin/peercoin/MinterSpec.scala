package scorex.nothingAtStakeCoin.peercoin

import java.nio.ByteBuffer

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.TestActor.AutoPilot
import akka.testkit.{TestActor, TestActorRef, TestKit, TestProbe}
import io.circe
import org.scalatest.{FeatureSpecLike, GivenWhenThen, Matchers}
import scorex.core.LocalInterface.LocallyGeneratedModifier
import scorex.core.NodeViewHolder
import scorex.core.NodeViewHolder.CurrentView
import scorex.nothingAtStakeCoin.ObjectGenerators
import scorex.nothingAtStakeCoin.consensus.NothingAtStakeCoinHistory
import scorex.nothingAtStakeCoin.peercoin.Minter.StartMinting
import scorex.nothingAtStakeCoin.settings.NothingAtStakeCoinSettings
import scorex.nothingAtStakeCoin.transaction.account.PublicKey25519NoncedBox
import scorex.nothingAtStakeCoin.transaction.state.NothingAtStakeCoinMinimalState
import scorex.nothingAtStakeCoin.transaction.wallet.NothingAtStakeCoinWallet
import scorex.nothingAtStakeCoin.transaction.{NothingAtStakeCoinMemoryPool, NothingAtStakeCoinTransaction}

import scala.concurrent.duration._

class MinterSpec extends TestKit(ActorSystem("MinterSpec"))
  with FeatureSpecLike
  with GivenWhenThen
  with ObjectGenerators
  with Matchers {

  lazy val fakeSettings: NothingAtStakeCoinSettings = new NothingAtStakeCoinSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile("test-settings.json")
  }

  feature("Minter can be started and stopped") {
    scenario("Starting a stopped minter") {
      Given("There is a Minter process stopped")
      val probe = TestProbe()
      val minterRef = TestActorRef(Props(classOf[Minter], fakeSettings, probe.ref))

      When("It receives a Start Message")
      minterRef ! StartMinting

      Then("It should start minting")
      probe expectMsg NodeViewHolder.GetCurrentView
    }
  }

  feature("Minter block generation") {
    scenario("It should not generate a block if there is no history") {
      Given("There is a Minter process")
      val probe = TestProbe()
      val minterRef = TestActorRef(Props(classOf[Minter], fakeSettings, probe.ref))

      When("It receives empty history as CurrentView after starting")
      val emptyHistoryCurrentView = CurrentView(new NothingAtStakeCoinHistory(), NothingAtStakeCoinMinimalState.genesisState(), NothingAtStakeCoinWallet(fakeSettings), NothingAtStakeCoinMemoryPool.emptyPool)
      probe.setAutoPilot(new TestActor.AutoPilot {
        override def run(sender: ActorRef, msg: Any): AutoPilot = msg match {
          case NodeViewHolder.GetCurrentView => sender ! emptyHistoryCurrentView; TestActor.KeepRunning
        }
      })
      minterRef ! StartMinting

      Then("It should not generate a block")
      probe expectMsgAllClassOf NodeViewHolder.GetCurrentView.getClass // No Block message received
    }

    scenario("It should not generate a block if there aren't enough transactions") {
      Given("There is a Minter process with a single transaction")
      val probe = TestProbe()
      val minterRef = TestActorRef(Props(classOf[Minter], fakeSettings, probe.ref))

      When("It receives a single tx")
      val block = nothingAtSakeCoinBlockGenerator().sample.get
      val history = new NothingAtStakeCoinHistory(blocks = Map(
        ByteBuffer.wrap(block.id) -> block
      ))
      val tx = nothingAtSakeCoinTransactionGenerator.sample.get
      val memoryPool = NothingAtStakeCoinMemoryPool(Map(
        ByteBuffer.wrap(tx.id) -> tx
      ))

      val singleTransacion = CurrentView(history, NothingAtStakeCoinMinimalState.genesisState(), NothingAtStakeCoinWallet(fakeSettings), memoryPool)
      probe.setAutoPilot(new TestActor.AutoPilot {
        override def run(sender: ActorRef, msg: Any): AutoPilot = msg match {
          case NodeViewHolder.GetCurrentView => sender ! singleTransacion; TestActor.NoAutoPilot
        }
      })
      minterRef ! StartMinting

      Then("It should not generate a block")
      probe expectMsgAllClassOf NodeViewHolder.GetCurrentView.getClass // No Block message received
    }

    scenario("It should not generate a block if there is no stake to use while mining") {
      Given("There is a Minter process with a single transaction")
      val probe = TestProbe()
      val minterRef = TestActorRef(Props(classOf[Minter], fakeSettings, probe.ref))

      When("It receives empty history as CurrentView after starting")
      val wallet = NothingAtStakeCoinWallet(fakeSettings)
      val genesisBlock = nothingAtSakeCoinBlockGenerator().sample.get
      val history = (new NothingAtStakeCoinHistory).append(genesisBlock).get._1
      val tx1 = NothingAtStakeCoinTransaction(
        wallet.secrets.head,
        IndexedSeq(1L),
        IndexedSeq((wallet.publicKeys.head, 90L)),
        10,
        0
      )
      val tx2 = NothingAtStakeCoinTransaction(
        wallet.secrets.head,
        IndexedSeq(2L),
        IndexedSeq((wallet.publicKeys.head, 180L)),
        20,
        0
      )

      val toBeUsedInMint = PublicKey25519NoncedBox(wallet.publicKeys.head, 0L, 50L)
      val tx1Input = PublicKey25519NoncedBox(wallet.publicKeys.head, 1L, 100L)
      val tx2Input = PublicKey25519NoncedBox(wallet.publicKeys.head, 2L, 200L)

      val minimalState = NothingAtStakeCoinMinimalState(
        Array.emptyByteArray,
        Map.empty,
        Map(
          ByteBuffer.wrap(tx1Input.id) -> tx1Input,
          ByteBuffer.wrap(tx2Input.id) -> tx2Input
        )
      )

      val memoryPool = NothingAtStakeCoinMemoryPool(Map(
        ByteBuffer.wrap(tx1.id) -> tx1,
        ByteBuffer.wrap(tx2.id) -> tx2
      ))
      val currentView = CurrentView(history, minimalState, wallet, memoryPool)
      probe.setAutoPilot(new TestActor.AutoPilot {
        override def run(sender: ActorRef, msg: Any): AutoPilot = msg match {
          case NodeViewHolder.GetCurrentView => sender ! currentView; TestActor.NoAutoPilot
        }
      })
      minterRef ! StartMinting

      Then("It should not generate a block")
      probe expectMsgAllClassOf NodeViewHolder.GetCurrentView.getClass // No Block message received
    }

    scenario("Successful block generation") {
      Given("There is a Minter process with a single transaction")
      val probe = TestProbe()
      val minterRef = TestActorRef(Props(classOf[Minter], fakeSettings, probe.ref))

      When("It receives empty history as CurrentView after starting")
      val wallet = NothingAtStakeCoinWallet(fakeSettings)
      val genesisBlock = nothingAtSakeCoinBlockGenerator().sample.get
      val history = (new NothingAtStakeCoinHistory).append(genesisBlock).get._1
      val tx1 = NothingAtStakeCoinTransaction(
        wallet.secrets.head,
        IndexedSeq(1L),
        IndexedSeq((wallet.publicKeys.head, 90L)),
        10,
        0
      )
      val tx2 = NothingAtStakeCoinTransaction(
        wallet.secrets.head,
        IndexedSeq(2L),
        IndexedSeq((wallet.publicKeys.head, 180L)),
        20,
        0
      )

      val toBeUsedInMint = PublicKey25519NoncedBox(wallet.publicKeys.head, 0L, 50L)
      val tx1Input = PublicKey25519NoncedBox(wallet.publicKeys.head, 1L, 100L)
      val tx2Input = PublicKey25519NoncedBox(wallet.publicKeys.head, 2L, 200L)

      val minimalState = NothingAtStakeCoinMinimalState(
        Array.emptyByteArray,
        Map.empty,
        Map(
          ByteBuffer.wrap(toBeUsedInMint.id) -> toBeUsedInMint,
          ByteBuffer.wrap(tx1Input.id) -> tx1Input,
          ByteBuffer.wrap(tx2Input.id) -> tx2Input
        )
      )

      val memoryPool = NothingAtStakeCoinMemoryPool(Map(
        ByteBuffer.wrap(tx1.id) -> tx1,
        ByteBuffer.wrap(tx2.id) -> tx2
      ))
      val currentView = CurrentView(history, minimalState, wallet, memoryPool)
      probe.setAutoPilot(new TestActor.AutoPilot {
        override def run(sender: ActorRef, msg: Any): AutoPilot = msg match {
          case NodeViewHolder.GetCurrentView => sender ! currentView; TestActor.NoAutoPilot
        }
      })
      minterRef ! StartMinting

      Then("It should not generate a block")
      probe.fishForMessage(10.seconds, "Waiting For New Block") {
        case LocallyGeneratedModifier(generatedBlock) => {
          val generatedTxs = generatedBlock.transactions.get.flatMap(t => t.asInstanceOf[NothingAtStakeCoinTransaction].to)
          generatedBlock.parentId.sameElements(genesisBlock.id) &&
            generatedBlock.transactions.get.size == 3 && // CoinStake Tx + 2 mined ones
            generatedTxs.head.value == toBeUsedInMint.value &&
            generatedTxs.tail.map(_.value).sum == tx1.to.map(_.value).sum + tx2.to.map(_.value).sum
        }
        case _ => false
      }
    }
  }
}

