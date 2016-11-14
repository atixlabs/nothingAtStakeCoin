package scorex.nothingAtStakeCoin

import akka.actor.{ActorRef, ActorSystem, Props}
import io.circe
import scorex.core.api.http.{ApiRoute, NodeViewApiRoute, UtilsApiRoute}
import scorex.core.app.{Application, ApplicationVersion}
import scorex.core.network.NodeViewSynchronizer
import scorex.core.network.message.MessageSpec
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.nothingAtStakeCoin.consensus.{NothingAtStakeCoinSyncInfo, NothingAtStakeSyncInfoSpec}
import scorex.nothingAtStakeCoin.state.NothingAtStakeCoinTransaction
import scorex.nothingAtStakeCoin.transaction.NothingAtStakeCoinBlock

import scala.reflect.runtime.universe._

class NothingAtStakeCoin(settingsFilename: String) extends Application {
  override val applicationName: String = "Nothing at Stake Coin"

  override def appVersion: ApplicationVersion = ApplicationVersion(0, 0, 1)

  override implicit val settings: Settings = new Settings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile(settingsFilename)
  }

  override type P = PublicKey25519Proposition
  override type TX = NothingAtStakeCoinTransaction
  override type PMOD = NothingAtStakeCoinBlock
  override type NVHT = NothingAtStakeCoinNodeViewHolder

  override val nodeViewHolderRef: ActorRef = actorSystem.actorOf(Props(classOf[NothingAtStakeCoinNodeViewHolder], settings))

  override val apiRoutes: Seq[ApiRoute] = Seq(
  UtilsApiRoute(settings),
  NodeViewApiRoute[P, TX](settings, nodeViewHolderRef))

  override val apiTypes: Seq[Type] = Seq(typeOf[UtilsApiRoute], typeOf[NodeViewApiRoute[P, TX]])

  override protected val additionalMessageSpecs: Seq[MessageSpec[_]] = _

  override val nodeViewSynchronizer: ActorRef = actorSystem.actorOf(Props(classOf[NodeViewSynchronizer[P, TX, NothingAtStakeCoinSyncInfo, NothingAtStakeSyncInfoSpec]]))
  override val localInterface: ActorRef = actorSystem.actorOf(Props(classOf[NothingAtStakeCoinLocalInterface]))
}

object NothingAtStakeCoin extends App {
  val settingsFilename = args.headOption.getOrElse("settings.json")
  new NothingAtStakeCoin(settingsFilename).run()
}

