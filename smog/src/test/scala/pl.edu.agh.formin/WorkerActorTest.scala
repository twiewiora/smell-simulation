package pl.edu.agh.smog

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import org.scalatest.concurrent.{Eventually, PatienceConfiguration, ScalaFutures}
import org.scalatest.{FlatSpecLike, Matchers}
import pl.edu.agh.smog.algorithm.SmogMovesController
import pl.edu.agh.smog.config.SmogConfig
import pl.edu.agh.smog.model.parallel.SmogConflictResolver
import pl.edu.agh.xinuk.config.GuiType
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.parallel.{Neighbour, NeighbourPosition}
import pl.edu.agh.xinuk.simulation.WorkerActor

class WorkerActorTest extends FlatSpecLike with Matchers with Eventually with ScalaFutures {
  implicit val config: SmogConfig = SmogConfig(
    foraminiferaStartEnergy = Energy(0.5),
    foraminiferaReproductionCost = Energy(0.2),
    foraminiferaReproductionThreshold = Energy(0.3),
    foraminiferaLifeActivityCost = Energy(0.1),
    algaeReproductionFrequency = 2,
    algaeEnergeticCapacity = Energy(0.4),
    signalSpeedRatio = 2,
    signalSuppressionFactor = 0.5,
    signalAttenuationFactor = 1,
    gridSize = 5,
    spawnChance = 0.1,
    foraminiferaSpawnChance = 0.5,
    foraminiferaInitialSignal = Signal(-1),
    algaeInitialSignal = Signal(1),
    guiType = GuiType.None,
    guiCellSize = 4,
    workersRoot = 2,
    iterationsNumber = 3,
    isSupervisor = true,
    shardingMod = 1
  )

  trait Fixture {
    implicit val system: ActorSystem = ActorSystem("WorkerActorTest")
  }

  "A WorkerActor" should "start first iteration when neighbours are initialized" in new Fixture {
    val workerRegion = TestProbe("workerRegion")
    val worker = system.actorOf(WorkerActor.props[SmogConfig](workerRegion.ref, (bufferZone, config) =>
      new SmogMovesController(bufferZone)(config), SmogConflictResolver,
      DefaultSmellPropagation.calculateSmellAddendsStandard
    ))

    worker ! WorkerActor.NeighboursInitialized(WorkerId(2), Vector(Neighbour(NeighbourPosition.Bottom)))

    val msg = workerRegion.expectMsgAnyClassOf(classOf[WorkerActor.IterationPartFinished])
    msg.iteration shouldBe 1
  }

  it should "start next iteration when neighbours finished actual" in new Fixture {

    import scala.concurrent.duration._

    val workerRegion = TestProbe("workerRegion")
    val worker = system.actorOf(WorkerActor.props[SmogConfig](workerRegion.ref, (bufferZone, config) =>
      new SmogMovesController(bufferZone)(config), SmogConflictResolver,
      DefaultSmellPropagation.calculateSmellAddendsStandard
    ))

    worker ! WorkerActor.NeighboursInitialized(WorkerId(2), Vector(Neighbour(NeighbourPosition.Bottom)))

    val msgIteration1 = workerRegion.expectMsgAnyClassOf(classOf[WorkerActor.IterationPartFinished])
    msgIteration1.iteration shouldBe 1

    worker ! WorkerActor.IterationPartFinished(WorkerId(4), WorkerId(2), 1, Array.empty)

    val msgIteration2 = workerRegion.expectMsgAnyClassOf(classOf[WorkerActor.IterationPartFinished])
    msgIteration2.iteration shouldBe 2

    worker ! WorkerActor.IterationPartFinished(WorkerId(4), WorkerId(2), 2, Array.empty)

    val msgIteration3 = workerRegion.expectMsgAnyClassOf(classOf[WorkerActor.IterationPartFinished])
    msgIteration3.iteration shouldBe 3

    worker ! WorkerActor.IterationPartFinished(WorkerId(4), WorkerId(2), 3, Array.empty)

    noException shouldBe thrownBy(system.whenTerminated.futureValue(PatienceConfiguration.Timeout(20.seconds)))
  }

  it should "shutdown actor system when iterations limit is reached" in new Fixture {
    val workerRegion = TestProbe("workerRegion")
    val worker = system.actorOf(WorkerActor.props[SmogConfig](workerRegion.ref, (bufferZone, config) =>
      new SmogMovesController(bufferZone)(config), SmogConflictResolver,
      DefaultSmellPropagation.calculateSmellAddendsStandard
    ))

    worker ! WorkerActor.NeighboursInitialized(WorkerId(2), Vector())

    import scala.concurrent.duration._

    noException shouldBe thrownBy(system.whenTerminated.futureValue(PatienceConfiguration.Timeout(20.seconds)))
  }


}
