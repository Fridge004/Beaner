package org.tearne.beaner.cross

import math.abs
import org.tearne.beaner._
import plant._
import cross._
import chroma._
import model._

class Gameter(val recombinationModel:RecombinationModel) 
    extends GameterSingleSelection
	with GameterDoubleSelection{
	
  /** Do a cross without any selection  
   */
  def withoutSelection(chromosome: Chromosome): Chromatid = {
    val firstChromatid = chromosome.firstChromatid
    val secondChromatid = chromosome.secondChromatid

    val gameteArray = new Array[Centimorgan](firstChromatid.size)
    for (i <- 0 until firstChromatid.size) {
      gameteArray(i) = firstChromatid(i).gameteify(secondChromatid(i), 0.5)
    }

    new Chromatid(gameteArray)
  }
  
  /** Probability that a gamete of the chromosome
   * contains the plant gene at the index
   */
  def probContains(plant: Plant, index: Int, chromosome: Chromosome):Double = {
    val firstChromatid = chromosome.firstChromatid
    val secondChromatid = chromosome.secondChromatid

    0.5 * (firstChromatid.probabilityOf(plant, index) + secondChromatid.probabilityOf(plant, index))
  }

  /** Make a gamete selecting for the given plant at the index 
   */
  def selectOn(plant: Plant, index: Int, chromosome: Chromosome): Chromatid = {
    val firstChromatid = chromosome.firstChromatid
    val secondChromatid = chromosome.secondChromatid

    if (firstChromatid(index).probabilityOf(plant) == 1) {
      if (secondChromatid(index).probabilityOf(plant) == 1) {
        selectOnBothTids(index, plant, chromosome)
      } else
        selectOnSingleTid(true, index, plant, chromosome)
    } else if (secondChromatid(index).probabilityOf(plant) == 1) {
      selectOnSingleTid(false, index, plant, chromosome)
    } else {
      throw new GameterException("Allele not present with probability one on either chromatid")
    }
  }
  
   /** Probability that a gamete produced by the chromosome
   *  would contain the plant gene at two specified indexes
   */
  def probContains(plant: Plant, firstIndex: Int, secondIndex: Int, chromosome: Chromosome):Double = {
	throw new UnsupportedOperationException("Not written yet")
  }
  
  /** Make gamete by selection at two centiMorgans
   */
  def selectOn(plant: Plant, firstIndex: Int, secondIndex: Int, chromosome: Chromosome): Chromatid = {
    throw new UnsupportedOperationException("Not written yet")
  }

  private def selectOnBothTids(index: Int, plant: Plant, chromosome: Chromosome) = {
    val firstChromatid = chromosome.firstChromatid
    val secondChromatid = chromosome.secondChromatid

    val gamete = new Chromatid(firstChromatid)
    for (i <- 0 until gamete.size) {
      if (i == index && (firstChromatid(i).alleles(plant) != 1.0 || secondChromatid(i).alleles(plant) != 1.0))
        throw new GameterException("Allele unexpectidly not present with probability one on both chromatids")
      else{
        //gamete(i) = new Centimorgan(plant)
    	  gamete(i) = firstChromatid(i).combinedWith(secondChromatid(i), 0.5)
      }
    }
    gamete
  }

  private def selectOnSingleTid(selectFirstChromatid: Boolean, index: Int, plant: Plant, chromosome: Chromosome) = {
    val firstChromatid = chromosome.firstChromatid
    val secondChromatid = chromosome.secondChromatid

    var cMa, cMb: Centimorgan = null
    val gamete = new Chromatid(firstChromatid)
    var tid1, tid2: Chromatid = null
    if (selectFirstChromatid) {
      tid1 = firstChromatid
      tid2 = secondChromatid
    } else {
      tid1 = secondChromatid
      tid2 = firstChromatid
    }

    for (i <- 0 until gamete.size) {
      if (i == index)
        if (tid1(i).alleles(plant) != 1.0)
          throw new GameterException("Allele unexpectidly not present with probability one on chromatid")
        else
          gamete(i) = new Centimorgan(plant)
      else {
        cMa = tid1(i); cMb = tid2(i)
        gamete(i) = cMa.combinedWith(cMb, getProbAtDist(index - i))
      }
    }
    gamete
  }

  private def getProbAtDist(d: Int) = recombinationModel.probInAtDistance(abs(d))
  
}