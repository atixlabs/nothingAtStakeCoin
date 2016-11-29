package scorex.nothingAtStakeCoin.api

import javax.ws.rs.Path

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import io.swagger.annotations._
import scorex.core.LocalInterface.LocallyGeneratedTransaction
import scorex.core.NodeViewHolder.{CurrentView, GetCurrentView}
import scorex.core.api.http.{ApiError, ApiRoute}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.NetworkTime
import scorex.nothingAtStakeCoin.consensus.NothingAtStakeCoinHistory
import scorex.nothingAtStakeCoin.settings.NothingAtStakeCoinSettings
import scorex.nothingAtStakeCoin.transaction.state.NothingAtStakeCoinMinimalState
import scorex.nothingAtStakeCoin.transaction.wallet.NothingAtStakeCoinWallet
import scorex.nothingAtStakeCoin.transaction.{NothingAtStakeCoinMemoryPool, NothingAtStakeCoinNodeNodeViewModifierCompanion, NothingAtStakeCoinTransaction}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success}

sealed case class PaymentBody(amount: IndexedSeq[Long], fee: Long, from: String, to: IndexedSeq[String])

@Path("/payment")
@Api(value = "/payment")
case class PaymentApiRoute(override val settings: NothingAtStakeCoinSettings, nodeViewHolderRef: ActorRef)(implicit val context: ActorRefFactory)
  extends ApiRoute {

  type HIS = NothingAtStakeCoinHistory
  type MS = NothingAtStakeCoinMinimalState
  type VL = NothingAtStakeCoinWallet
  type MP = NothingAtStakeCoinMemoryPool

  val waitForCurrentViewInterval = 10.seconds

  override lazy val route = payment

  @ApiOperation(value = "Send payment",
    notes = "Send payment to another wallet",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.nothingAtStakeCoin.api.PaymentBody",
      defaultValue = "{\n\t\"amount\":400,\n\t\"fee\":1,\n\t\"sender\":\"senderId\",\n\t\"recipient\":\"recipientId\"\n}"
    )
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with response or error")
  ))
  def payment: Route = path("payment") {
    entity(as[String]) { body =>
      withAuth {
        postJsonRoute {
          decode[PaymentBody](body).map { case PaymentBody(amount, fee, from, to) =>
            val future = nodeViewHolderRef ? GetCurrentView
            val currentView = Await.result(future, waitForCurrentViewInterval).asInstanceOf[CurrentView[HIS, MS, VL, MP]]

            NothingAtStakeCoinNodeNodeViewModifierCompanion.createTransaction(
              state = currentView.state,
              fromPk = currentView.vault.secrets.head,
              from = from,
              to = to,
              amount = amount,
              fee = fee,
              timestamp = NetworkTime.time()) match {
              case Success(tx) => {
                nodeViewHolderRef ! LocallyGeneratedTransaction.apply[PublicKey25519Proposition, NothingAtStakeCoinTransaction](tx)
                Map("msg" -> "Transaction created").asJson
              }
              case Failure(err: Throwable) => ApiError.failure(err).asJson
            }
          }.getOrElse(ApiError.wrongJson)
        }
      }
    }
  }
}
