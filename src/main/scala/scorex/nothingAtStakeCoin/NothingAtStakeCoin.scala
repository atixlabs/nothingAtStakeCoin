package scorex.nothingAtStakeCoin

import akka.actor.{ActorRef, Props}
import io.circe
import scorex.core.api.http.{ApiRoute, NodeViewApiRoute, UtilsApiRoute}
import scorex.core.app.{Application, ApplicationVersion}
import scorex.core.network.NodeViewSynchronizer
import scorex.core.network.message.MessageSpec
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.nothingAtStakeCoin.api.PaymentApiRoute
import scorex.nothingAtStakeCoin.block.{NothingAtStakeCoinBlock, NothingAtStakeCoinSyncInfo, NothingAtStakeCoinSyncInfoSpec}
import scorex.nothingAtStakeCoin.peercoin.Minter
import scorex.nothingAtStakeCoin.peercoin.Minter.StartMinting
import scorex.nothingAtStakeCoin.settings.NothingAtStakeCoinSettings
import scorex.nothingAtStakeCoin.transaction.NothingAtStakeCoinTransaction

import scala.reflect.runtime.universe._

class NothingAtStakeCoin(settingsFilename: String) extends Application {
  implicit lazy val settings = new NothingAtStakeCoinSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile(settingsFilename)
  }

  override lazy val applicationName: String = "NothingAtStakeCoin"

  override val appVersion: ApplicationVersion = ApplicationVersion(0, 0, 1)

  override type P = PublicKey25519Proposition
  override type TX = NothingAtStakeCoinTransaction
  override type PMOD = NothingAtStakeCoinBlock
  override type NVHT = NothingAtStakeCoinNodeViewHolder

  override val nodeViewHolderRef: ActorRef = actorSystem.actorOf(Props(classOf[NothingAtStakeCoinNodeViewHolder], settings))

  override val localInterface: ActorRef = actorSystem.actorOf(Props(classOf[NothingAtStakeCoinLocalInterface], nodeViewHolderRef))

  override val apiTypes: Seq[Type] = Seq(typeOf[UtilsApiRoute], typeOf[NodeViewApiRoute[P, TX]], typeOf[PaymentApiRoute])

  override protected lazy val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(NothingAtStakeCoinSyncInfoSpec)

  override val nodeViewSynchronizer: ActorRef =
    actorSystem.actorOf(Props(classOf[NodeViewSynchronizer[P, TX, NothingAtStakeCoinSyncInfo, NothingAtStakeCoinSyncInfoSpec.type]],
      networkController, nodeViewHolderRef, localInterface, NothingAtStakeCoinSyncInfoSpec))

  override val apiRoutes: Seq[ApiRoute] = Seq(
    UtilsApiRoute(settings),
    NodeViewApiRoute[P, TX](settings, nodeViewHolderRef),
    PaymentApiRoute(settings, nodeViewHolderRef)
  )

  val minter: ActorRef = actorSystem.actorOf(Props(classOf[Minter], settings, nodeViewHolderRef))
  minter ! StartMinting
}

object NothingAtStakeCoin extends App {
  val settingsFilename = args.headOption.getOrElse("settings.json")
  new NothingAtStakeCoin(settingsFilename).run()
}

