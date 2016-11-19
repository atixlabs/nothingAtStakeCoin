package scorex.nothingAtStakeCoin.peercoin

import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.{Actor, ActorRef}
import scorex.core.LocalInterface.LocallyGeneratedModifier
import scorex.core.NodeViewHolder
import scorex.core.NodeViewHolder.CurrentView
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.PrivateKey25519
import scorex.core.utils.{NetworkTime, ScorexLogging}
import scorex.nothingAtStakeCoin.history.NothingAtStakeCoinHistory
import scorex.nothingAtStakeCoin.peercoin.Minter.{MintLoop, StartMinting, StopMiniting}
import scorex.nothingAtStakeCoin.settings.NothingAtStakeCoinSettings
import scorex.nothingAtStakeCoin.transaction.account.PublicKey25519NoncedBox
import scorex.nothingAtStakeCoin.transaction.{NothingAtStakeCoinBlock, NothingAtStakeCoinMemoryPool, NothingAtStakeCoinTransaction}
import scorex.nothingAtStakeCoin.{NothingAtStakeCoinMinimalState, NothingAtStakeCoinWallet}

import scala.concurrent.duration._
import scala.language.postfixOps

class Minter(settings: NothingAtStakeCoinSettings, viewHolderRef: ActorRef) extends Actor with ScorexLogging {

  private val minterLoopDelay = 5000 millisecond

  def stopped: Receive = {
    case StartMinting => {
      context become minting
      self ! MintLoop
    }
  }

  def minting: Receive = {
    case StopMiniting => context become stopped
    case MintLoop => {
      log.info("[MintLoop] Start, asking nodeViewHolder it's current view")
      viewHolderRef ! NodeViewHolder.GetCurrentView
    }
    case CurrentView(history: NothingAtStakeCoinHistory, minimalState: NothingAtStakeCoinMinimalState, wallet: NothingAtStakeCoinWallet, memoryPool: NothingAtStakeCoinMemoryPool) => {
      log.info("[MintLoop] Current view received")
      if (!history.isEmpty) {
        val block: Option[NothingAtStakeCoinBlock] = getTxs(memoryPool).filter(minimalState.isValid) match {
          case txs: Iterable[NothingAtStakeCoinTransaction] if txs.nonEmpty => {
            log.info(s"[MintLoop] Transactions ${txs.size}  found")
            val coinStakeBoxes = getCoinStakeBoxes(wallet, minimalState, txs).toSeq
            log.info(s"[MintLoop] ${coinStakeBoxes.size} coinstake boxes found found")
            if (coinStakeBoxes.nonEmpty) {
              log.info("[MintLoop] Stake found, about to generate block")
              val parentBlocks = history.openSurfaceIds()
                .foldLeft(Seq[Option[NothingAtStakeCoinBlock]]())((list, blockId) => list :+ history.blockById(blockId))
                .flatten
                .sortBy(_.timestamp)
              val txsToIncludeInBlock = txs.take(settings.transactionsPerBlock - 1).toSeq // At least we need to include one coinStake TX
              Some(generateBlock(wallet.secrets.head, parentBlocks.head, coinStakeBoxes, txsToIncludeInBlock, NetworkTime.time()))
            } else {
              log.info("[MintLoop] No stake to mint found")
              None
            }
          }
          case txs: Iterable[NothingAtStakeCoinTransaction] if txs.isEmpty => None
        }

        if (block.isDefined) {
          log.info(s"[MintLoop] Generated block! ${block}")
          viewHolderRef ! LocallyGeneratedModifier.apply[PublicKey25519Proposition, NothingAtStakeCoinTransaction, NothingAtStakeCoinBlock](block.get)
          log.info(s"[MintLoop] Block sent to view holder ${block}")
        } else {
          log.info("[MintLoop] No block generated")
        }
        context.system.scheduler.scheduleOnce(minterLoopDelay, self, MintLoop)
      }
    }
  }

  /**
    * This method gets from minimal state all boxes that can be used in coinstake tx as input, ie, the ones that are
    * not being used as input in the current transaction
    *
    * @param minimalState
    * @param txs
    * @return Boxes that can be used in minting process
    */
  def getCoinStakeBoxes(wallet: NothingAtStakeCoinWallet,
                        minimalState: NothingAtStakeCoinMinimalState,
                        txs: Iterable[NothingAtStakeCoinTransaction]): Set[PublicKey25519NoncedBox] = {

    val walletUnspents: Set[PublicKey25519NoncedBox] = wallet.publicKeys.flatMap(minimalState.boxesOf)

    txs.flatMap(_.from).foldLeft(Set[PublicKey25519NoncedBox]()) {
      case (unspents, input) => unspents ++ walletUnspents.filter(p => p.nonce != input.nonce)
    }

  }

  override def receive: Receive = stopped

  private def getTxs(memoryPool: NothingAtStakeCoinMemoryPool): Iterable[NothingAtStakeCoinTransaction] = {
    // FIXME Maybe it's better to take TX with better fees if we include fees in this project
    memoryPool.take(settings.transactionsPerBlock)
  }


  def generateBlock(minterPk: PrivateKey25519,
                    parent: NothingAtStakeCoinBlock,
                    coinStakeBoxes: Seq[PublicKey25519NoncedBox],
                    txs: Seq[NothingAtStakeCoinTransaction],
                    timestamp: Long): NothingAtStakeCoinBlock = {

    //val minterCoinAge = calculateCoinAge()

    val stakeTransaction = NothingAtStakeCoinTransaction(
      minterPk,
      coinStakeBoxes.map(_.nonce).toIndexedSeq,
      coinStakeBoxes.map(box => (minterPk.publicImage, box.value)).toIndexedSeq,
      timestamp,
      timestamp
    )

    NothingAtStakeCoinBlock(
      parentId = parent.id,
      timestamp = timestamp,
      generatorKeys = minterPk,
      coinAge = Long.MaxValue,
      txs = stakeTransaction +: txs
    )
  }
}

object Minter {

  case object StartMinting

  case object StopMiniting

  case object MintLoop

  def calculateCoinAge(minimalState: NothingAtStakeCoinMinimalState, proposition: PublicKey25519Proposition) = {
    minimalState.boxesOf(proposition)
  }
}