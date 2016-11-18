package scorex.nothingAtStakeCoin.settings

import scorex.core.settings.Settings

abstract class NothingAtStakeCoinSettings extends Settings {
  def nothingAtStakeCoinSettings = settingsJSON("nothingAtStakeCoin").asObject.get.toMap

  def genesisTxs = nothingAtStakeCoinSettings("genesisTxs").asNumber.get.toInt.get
}
