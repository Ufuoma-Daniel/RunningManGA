package services

import models._
import MutationService._

import scala.util.Random

class SimulationService {

  def printStats(runner: Runner): String = s"Name :${runner.name},  H,W,S = [${runner.height}, ${runner.weight}, ${runner.stamina}]  LL:${runner.legLength} Single step is :${runner.calcStep} and Hundred steps is :${runner.calcHundredSteps} \n"

  def printStatsBMI(runner: Runner): String = s"H,W = [${runner.height}, ${runner.weight}], SS:${runner.calcStep}, HS:${runner.calcHundredSteps}, BMI:${runner.getBMIRating} ${runner.calcBMI} \n"

  def orderRunnersByStep(runningList : Seq[Runner]): Seq[Runner] = runningList.sortBy(runner => runner.calcHundredSteps)

  def makeRunners(noOfRunners: Int): Seq[Runner] = Seq.fill(noOfRunners)(makeRunningMan)

  def makeRunningMan: Runner = {

    val height = Random.nextInt(225) + 51
    val weight = Random.nextInt(180) + 21
    val stamina = Random.nextInt(30)+1

    Runner(height, weight, stamina)
  }

  def makeMutatedRunners(survivorPool: Seq[Runner]): Seq[Runner] = survivorPool map randomMutation

  def makeSinglePointRunner(survivorPool: Seq[Runner], runningList : Seq[Runner], index: Int): Seq[Runner] = {
    if(index == 0) runningList else {
      val newRunner = singlePointCrossover(survivorPool(Random.nextInt(survivorPool.length)), survivorPool(Random.nextInt(survivorPool.length)))
      makeSinglePointRunner(survivorPool, runningList :+ newRunner, index-1)
    }
  }

  def makeUniformRunner(survivorPool: Seq[Runner], runningList : Seq[Runner], index: Int): Seq[Runner] = {
    survivorPool map(runner => uniformCrossover(runner, survivorPool(Random.nextInt(survivorPool.length))))
  }

  def runSimulationMany(currentGeneration: Seq [Runner], numOfGenerations: Int): Seq[Runner] = {
    val evolvedRunners = advanceGeneration(orderRunnersByStep(currentGeneration))
    val orderedRunners = orderRunnersByStep(evolvedRunners)

    if(numOfGenerations == 0) orderedRunners else runSimulationMany(orderedRunners, numOfGenerations-1)
  }

  def advanceGeneration(survivors: Seq[Runner]): Seq[Runner] = {
    val (_, goodRunners) = survivors.splitAt((survivors.length*3)/4)
    val uniformNewInfants = makeUniformRunner(goodRunners, Seq(), survivors.length/4)
    val singlePointNewInfants = makeSinglePointRunner(goodRunners, Seq(), survivors.length/4)
    val mutatedNewInfants = makeMutatedRunners(goodRunners)

    goodRunners ++ uniformNewInfants ++ singlePointNewInfants ++ mutatedNewInfants
  }
}