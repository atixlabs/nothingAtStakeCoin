package scorex.nothingAtStakeCoin.settings

import scorex.core.settings.Settings

abstract class NothingAtStakeCoinSettings extends Settings {
  def nothingAtStakeCoinSettings = settingsJSON("nothingAtStakeCoin").asObject.get.toMap

  def genesisTransactions: Int = nothingAtStakeCoinSettings("genesisTransactions").as[Int].getOrElse(10)

  def transactionsPerBlock: Int = nothingAtStakeCoinSettings("transactionsPerBlock").as[Int].getOrElse(10)

  def genesisTransactionAmount: Long = nothingAtStakeCoinSettings("genesisTransactionAmount").as[Long].getOrElse(100000)

  def minStakeMinutes: Long = nothingAtStakeCoinSettings("minStakeMinutes").as[Long].getOrElse(1)

  def maxStakeMinutes: Long = nothingAtStakeCoinSettings("maxStakeMinutes").as[Long].getOrElse(10)
}
