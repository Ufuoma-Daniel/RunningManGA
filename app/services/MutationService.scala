package services

import models.Runner

import scala.util.Random

object MutationService {

  def singlePointCrossover(parentA: Runner, parentB : Runner): Runner = {
    val crossoverPoint = Random.nextInt(2)
    val firstParent = convertRunnerToSeq(parentA).splitAt(crossoverPoint)._1
    val secondParent = convertRunnerToSeq(parentB).splitAt(crossoverPoint)._2

    convertSeqToRunner(firstParent ++ secondParent)
  }

  def uniformCrossover(parentA: Runner, parentB : Runner): Runner = {
    val firstParent = convertRunnerToSeq(parentA)
    val secondParent = convertRunnerToSeq(parentB)

    val crossoverChance = Seq.fill(3)(Random.nextBoolean())
    def crossover(index: Int) = if(crossoverChance(index)) firstParent(index) else secondParent(index)

    Runner (crossover(0), crossover(1), crossover(2))
  }

  def randomMutation(individual: Runner): Runner = {
    Random.nextInt(3) match {
      case 0 => individual.copy(height = Random.nextInt(225)+51)
      case 1 => individual.copy(weight = Random.nextInt(180)+21)
      case 2 => individual.copy(stamina = Random.nextInt(30)+1)
    }
  }

  def convertRunnerToSeq(runner: Runner): Seq[Int] = Seq(runner.height, runner.weight, runner.stamina)

  def convertSeqToRunner(seq: Seq[Int]): Runner = Runner(seq.head, seq(1), seq(2))
}
