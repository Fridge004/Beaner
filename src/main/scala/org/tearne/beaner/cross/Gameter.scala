/*
 * Copyright (c) Oliver Tearne (tearne at gmail dot com)
 *
 * This program is free software: you can redistribute it and/or modify it under the terms of
 * the GNU General Public License as published by the Free Software Foundation, either version
 * 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with this program.
 * If not, see <http://www.gnu.org/licenses/>.
 */

package org.tearne.beaner.cross

import org.tearne.beaner.chroma._
import org.tearne.beaner.plant._
import org.tearne.beaner.model._
import scala.math._

class Gameter(private val model: RecombinationModel){

  def probContains(plant: Plant, index: Int, chromosome: Chromosome) = {
    val firstChromatid = chromosome.firstChromatid
    val secondChromatid = chromosome.secondChromatid

    0.5 * (firstChromatid.probabilityOf(plant, index) + secondChromatid.probabilityOf(plant, index))
  }

  def selectOn(plant: Plant, index: Int, chromosome: Chromosome): Chromatid = {
    val firstChromatid = chromosome.firstChromatid
    val secondChromatid = chromosome.secondChromatid

    if (firstChromatid(index).probabilityOf(plant) > 0) {
      if (secondChromatid(index).probabilityOf(plant) > 0) {
        selectOnBothTids(index, plant, chromosome)
      } else
        selectOnSingleTid(true, index, plant, chromosome)
    } else if (secondChromatid(index).probabilityOf(plant) > 0) {
      selectOnSingleTid(false, index, plant, chromosome)
    } else {
      //Not possible to select
      null
    }
  }

  private def selectOnBothTids(index: Int, plant: Plant, chromosome: Chromosome) = {
    val firstChromatid = chromosome.firstChromatid
    val secondChromatid = chromosome.secondChromatid

    val gamete = new Chromatid(firstChromatid)
    for (i <- 0 until gamete.size) {
      if (i == index && (firstChromatid(i).alleles(plant) != 1.0 || secondChromatid(i).alleles(plant) != 1.0))
        throw new ChromasomeException("Can only select when allele is present with probability one")
      else
        gamete(i) = new Centimorgan(plant)
      gamete(i) = firstChromatid(i).combinedWith(secondChromatid(i), 0.5)
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
          throw new ChromasomeException("Can only select when allele is present with probability one on one of the chromatids")
        else
          gamete(i) = new Centimorgan(plant)
      else {
        cMa = tid1(i); cMb = tid2(i)
        gamete(i) = cMa.combinedWith(cMb, getProbAtDist(index - i))
      }
    }
    gamete
  }

  def withoutSelection(chromosome: Chromosome): Chromatid = {
    val firstChromatid = chromosome.firstChromatid
    val secondChromatid = chromosome.secondChromatid

    val gameteArray = new Array[Centimorgan](firstChromatid.size)
    for (i <- 0 until firstChromatid.size) {
      gameteArray(i) = firstChromatid(i).gameteify(secondChromatid(i), 0.5)
    }

    new Chromatid(gameteArray)
  }

  private def getProbAtDist(d: Int) = model.probInAtDistance(abs(d))
}