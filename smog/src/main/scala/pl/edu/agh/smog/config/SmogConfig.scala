package pl.edu.agh.smog.config

import pl.edu.agh.xinuk.config.{GuiType, XinukConfig}
import pl.edu.agh.xinuk.model.Signal

final case class SmogConfig(
                             gridSize: Int,
                             guiCellSize: Int,
                             signalSuppressionFactor: Double,
                             signalAttenuationFactor: Double,
                             workersRoot: Int,
                             shardingMod: Int,

                             guiType: GuiType,
                             isSupervisor: Boolean,
                             signalSpeedRatio: Int,
                             iterationsNumber: Long,

                             chimneyFrequency: Int,
                             chimneyAmount: Int,
                             smogAttenuationFactor: Int,
                             smogStartIntensity: Int,

                             smogInitialSignal: Signal,
                             windInitialSignal: Signal,
                             crossBendFactor: Double,
                             straightBendFactor: Double,
                           ) extends XinukConfig