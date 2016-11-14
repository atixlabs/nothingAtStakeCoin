package scorex.nothingAtStakeCoin

import akka.actor.ActorRef
import io.circe
import scorex.core.api.http.ApiRoute
import scorex.core.app.{Application, ApplicationVersion}
import scorex.core.network.message.MessageSpec
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

class NothingAtStakeCoin(settingsFilename: String) extends Application {
  override val applicationName: String = "Nothing at Stake Coin"

  override def appVersion: ApplicationVersion = ApplicationVersion(0, 0, 1)

  override implicit val settings: Settings = new Settings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile(settingsFilename)
  }

  override type P = PublicKey25519Proposition
  override type TX = NothingAtStakeCoinTransaction
  override type PMOD = this.type
  override type NVHT = this.type
  override val apiRoutes: Seq[ApiRoute] = _
  override val apiTypes: Seq[_root_.scala.reflect.runtime.universe.Type] = _
  override protected val additionalMessageSpecs: Seq[MessageSpec[_]] = _

  override val nodeViewHolderRef: ActorRef = _
  override val nodeViewSynchronizer: ActorRef = _
  override val localInterface: ActorRef = _
}

object NothingAtStakeCoin extends App {
  val settingsFilename = args.headOption.getOrElse("settings.json")
  new NothingAtStakeCoin(settingsFilename).run()
}

