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

package org.tearne.beaner.plant

import org.tearne.beaner.chroma._
import org.tearne.beaner.cross._
import org.apache.commons.math.distribution.BinomialDistributionImpl

case class OffspringPlant(
  val chromosomes: Array[Chromosome],
  val spec: PlantSpec,
	val selectionProbability: Option[Double],
  name: Option[String] = None,
  val parents: Option[PlantPair] = None) extends Plant {

  type Self = OffspringPlant

  def this(chromasomes: Array[Chromosome], 
	   spec: PlantSpec) = 
    this(chromasomes, spec, None)
  
  def this(chromasomes: Array[Chromosome], 
	   spec: PlantSpec, 
	   selectionProbability: Double) =
    this(chromasomes, spec, {
      if (selectionProbability > 1.0 || selectionProbability < 0.0)
        throw new OffspringPlantException("Selection probability out of range: " + selectionProbability)
      Some(selectionProbability)
    })

  for (i <- 0 until chromosomes.size) {
    val length1 = chromosomes(i).size
    val length = spec.chromosomeLengths(i)
    if (length1 != length)
      throw new OffspringPlantException("Chromosome " + i + " does not match spec (actual=" + chromosomes(i).size + ",spec=" + spec.chromosomeLengths(i) + ")")
  }

  @Override
  def named(newName:String) = this.copy(name=Option(newName))

  def proportionOf(plant: Plant): Double = {
    var sumP = 0.0
    var cMCount = 0
    for (i <- 0 until chromosomes.size) {
      sumP += chromosomes(i).proportionOf(plant) * spec.chromosomeLengths(i)
      cMCount += spec.chromosomeLengths(i)
    }
    sumP / cMCount
  }

  def numPlantsForConfidence(confidence: Double, numPlantsReq: Int): Int = {
    var binom = new BinomialDistributionImpl(numPlantsReq-1, selectionProbability.get)

    while(binom.cumulativeProbability(numPlantsReq-1) > 1-confidence){
      binom = new BinomialDistributionImpl(binom.getNumberOfTrials()+1, selectionProbability.get)
    }

    binom.getNumberOfTrials()
  }
}
