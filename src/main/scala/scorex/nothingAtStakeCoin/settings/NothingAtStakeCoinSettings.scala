package scorex.nothingAtStakeCoin.settings

import io.circe.Json
import scorex.core.settings.Settings

abstract class NothingAtStakeCoinSettings extends Settings {
  def nothingAtStakeCoinSettings: Map[String, Json] = settingsJSON("nothingAtStakeCoin").asObject.get.toMap

  def genesisTransactions: Int = nothingAtStakeCoinSettings("genesisTransactions").as[Int].toTry.get

  def transactionsPerBlock: Int = nothingAtStakeCoinSettings("transactionsPerBlock").as[Int].toTry.get

  def genesisTransactionAmount: Long = nothingAtStakeCoinSettings("genesisTransactionAmount").as[Long].toTry.get

  def minStakeMinutes: Long = nothingAtStakeCoinSettings("minStakeMinutes").as[Long].toTry.get

  def maxStakeMinutes: Long = nothingAtStakeCoinSettings("maxStakeMinutes").as[Long].toTry.get

  def numberOfBestChains: Int = nothingAtStakeCoinSettings("numberOfBestChains").as[Int].toTry.get

}
