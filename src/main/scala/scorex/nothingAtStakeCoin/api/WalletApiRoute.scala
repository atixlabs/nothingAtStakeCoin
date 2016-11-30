package scorex.nothingAtStakeCoin.api

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.server.Route
import io.circe.syntax._
import io.swagger.annotations._
import scorex.core.api.http.ApiRoute
import scorex.nothingAtStakeCoin.settings.NothingAtStakeCoinSettings
import scorex.nothingAtStakeCoin.transaction.wallet.NothingAtStakeCoinWallet

@Path("/wallet")
@Api(value = "/wallet", produces = "application/json")
case class WalletApiRoute(override val settings: NothingAtStakeCoinSettings)
                         (implicit val context: ActorRefFactory)
  extends ApiRoute {

  override lazy val route = address

  @ApiOperation(
    value = "Get Wallet Address",
    notes = "Returns your wallet address in order to receive payments",
    httpMethod = "GET")
  def address: Route = path("wallet") {
    getJsonRoute {
      Map("address" -> NothingAtStakeCoinWallet(settings).publicKeys.head.address).asJson
    }
  }
}
