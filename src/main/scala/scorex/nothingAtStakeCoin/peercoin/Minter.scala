package scorex.nothingAtStakeCoin.peercoin

import akka.actor.{Actor, ActorRef}
import scorex.core.LocalInterface.LocallyGeneratedModifier
import scorex.core.NodeViewHolder
import scorex.core.NodeViewHolder.CurrentView
import scorex.core.block.Block.Timestamp
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.PrivateKey25519
import scorex.core.utils.{NetworkTime, ScorexLogging}
import scorex.nothingAtStakeCoin.block.NothingAtStakeCoinBlock
import scorex.nothingAtStakeCoin.consensus.NothingAtStakeCoinHistory
import scorex.nothingAtStakeCoin.peercoin.Minter.{MintLoop, StartMinting, StopMiniting}
import scorex.nothingAtStakeCoin.settings.NothingAtStakeCoinSettings
import scorex.nothingAtStakeCoin.transaction.NothingAtStakeCoinTransaction._
import scorex.nothingAtStakeCoin.transaction.account.PublicKey25519NoncedBox
import scorex.nothingAtStakeCoin.transaction.state.NothingAtStakeCoinMinimalState
import scorex.nothingAtStakeCoin.transaction.wallet.NothingAtStakeCoinWallet
import scorex.nothingAtStakeCoin.transaction.{NothingAtStakeCoinInput, NothingAtStakeCoinMemoryPool, NothingAtStakeCoinTransaction}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Success}

class Minter(settings: NothingAtStakeCoinSettings, viewHolderRef: ActorRef) extends Actor with ScorexLogging {

  private val minterLoopDelay = 5000 millisecond

  def stopped: Receive = {
    case StartMinting =>
      context become minting
      self ! MintLoop
  }

  def minting: Receive = {
    case StopMiniting => context become stopped
    case MintLoop =>
      log.info("[MintLoop] Start, asking nodeViewHolder it's current view")
      viewHolderRef ! NodeViewHolder.GetCurrentView
    case CurrentView(
    history: NothingAtStakeCoinHistory,
    minimalState: NothingAtStakeCoinMinimalState,
    wallet: NothingAtStakeCoinWallet,
    memoryPool: NothingAtStakeCoinMemoryPool) =>
      log.info("[MintLoop] Current view received")
      if (!history.isEmpty) {
        val block: Option[NothingAtStakeCoinBlock] = memoryPool.take(settings.transactionsPerBlock).filter(minimalState.isValid) match {
          case txs: Iterable[NothingAtStakeCoinTransaction] if txs.size == settings.transactionsPerBlock =>
            log.info(s"[MintLoop] Transactions ${txs.size}  found")
            val timestamp = NetworkTime.time()
            val coinStakeBoxes = getCoinStakeBoxes(history, wallet, minimalState, txs, timestamp).toSeq
            log.info(s"[MintLoop] ${coinStakeBoxes.size} coinstake boxes found")
            if (coinStakeBoxes.nonEmpty) {
              log.info("[MintLoop] Stake found, about to generate block")
              val parentBlocks = history.openSurfaceIds()
                .foldLeft(Seq[Option[NothingAtStakeCoinBlock]]())((list, blockId) => history.blockById(blockId) +: list)
                .flatten
                .sortBy(_.timestamp)
              val txsToIncludeInBlock = txs.take(settings.transactionsPerBlock).toSeq
              generateBlock(wallet.secrets.head, parentBlocks.head, coinStakeBoxes, txsToIncludeInBlock, timestamp, history)
            } else {
              log.info("[MintLoop] No stake to mint found")
              None
            }
          case txs: Iterable[NothingAtStakeCoinTransaction] => None
        }

        if (block.isDefined) {
          log.info(s"[MintLoop] Generated block! ${block.get.encodedId}")
          viewHolderRef ! LocallyGeneratedModifier.apply[PublicKey25519Proposition, NothingAtStakeCoinTransaction, NothingAtStakeCoinBlock](block.get)
          log.info(s"[MintLoop] Block sent to view holder ${block.get.encodedId}")
        } else {
          log.info("[MintLoop] No block generated")
        }
        context.system.scheduler.scheduleOnce(minterLoopDelay, self, MintLoop)
      }
  }

  /**
    * This method gets from minimal state all boxes that can be used in coinstake tx as input, ie, the ones that are
    * not being used as input in the current transaction and generate some reward
    *
    * @return Boxes that can be used in minting process
    */
  def getCoinStakeBoxes(history: NothingAtStakeCoinHistory,
                        wallet: NothingAtStakeCoinWallet,
                        minimalState: NothingAtStakeCoinMinimalState,
                        txs: Iterable[NothingAtStakeCoinTransaction],
                        timestamp: Timestamp): Set[PublicKey25519NoncedBox] = {

    val txUnspents = txs.flatMap(_.from).foldLeft(Set[(PublicKey25519Proposition, Nonce)]()) {
      case (unspents, input) => unspents + (input.proposition -> input.nonce)
    }

    wallet.publicKeys.flatMap(minimalState.boxesOf)
      .filter(unspent => !txUnspents.contains(unspent.proposition -> unspent.nonce))
      .filter(unspent => {
        val reward = history.getStakeReward(IndexedSeq(NothingAtStakeCoinInput(unspent.proposition, unspent.nonce)), timestamp)
        reward.isSuccess && reward.get > 0
      })
  }

  override def receive: Receive = stopped

  def generateBlock(minterPk: PrivateKey25519,
                    parent: NothingAtStakeCoinBlock,
                    coinStakeBoxes: Seq[PublicKey25519NoncedBox],
                    txs: Seq[NothingAtStakeCoinTransaction],
                    timestamp: Long,
                    history: NothingAtStakeCoinHistory): Option[NothingAtStakeCoinBlock] = {

    val nonceFrom = coinStakeBoxes.map(_.nonce).toIndexedSeq
    val to = coinStakeBoxes.map(box => (minterPk.publicImage, box.value)).toIndexedSeq
    val from = nonceFrom.map(NothingAtStakeCoinInput(minterPk.publicImage, _))

    val maybeStakeReward = history.getStakeReward(from, timestamp)
    maybeStakeReward match {
      case Success(reward) =>
        val stakeTransactionWithReward = NothingAtStakeCoinTransaction(
          fromPk = minterPk,
          from = nonceFrom,
          to = (minterPk.publicImage, reward) +: to,
          fee = 0,
          timestamp = timestamp
        )
        history.getCoinAge(stakeTransactionWithReward +: txs) match {
          case Success(blockCoinAge) => Some(NothingAtStakeCoinBlock(
            parentId = parent.id,
            timestamp = timestamp,
            generatorKeys = minterPk,
            coinAge = blockCoinAge,
            stakeTx = Some(stakeTransactionWithReward),
            txs = txs
          ))
          case Failure(e) => None
        }
      case Failure(e) =>
        log.error("Error in minter", e)
        None
    }
  }
}

object Minter {

  case object StartMinting

  case object StopMiniting

  case object MintLoop

}
