import services.SimulationService

object Main extends App {
  val sim = new SimulationService()
  val runners = sim.makeRunners(30)

  runners foreach (runner => println(sim.printStats(runner)))
  val runSim = sim.runSimulationMany(runners, 30)
  println("\n" * 10)
  runSim foreach (runner => println(sim.printStats(runner)))
}
