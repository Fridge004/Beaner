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
import org.tearne.beaner.cross._

class PlantCrosser(chromaCrosser: ChromosomeCrosser) {

  private def checkCanCross(plants: PlantPair) = {
    if (plants.first.spec != plants.second.spec)
      throw new PlantCrosserException("Cannot cross plants with different specs")
  }

  def selectHeterozygousOffspring(plants: PlantPair, criteriaList: Set[Criterion]): Option[OffspringPlant] = {
    checkCanCross(plants)

    val resultGenome = new Array[Chromosome](plants.first.spec.chromasomeLengths.size)
    var selectionProbability = 0.0;

    criteriaList.foreach(criteria => {
      val chromaNum = criteria.chromasomeIndex
      if (resultGenome(chromaNum) != null)
        throw new UnsupportedOperationException("Not supported yet: multiple criteria on single chromasome")

      val selectedChroma = getHeterozygousSelectedChroma(plants, criteria)
      if (selectedChroma == None)
        return None
      resultGenome(chromaNum) = selectedChroma.get
    })

    for (i <- 0 until resultGenome.size) {
      if (resultGenome(i) == null)
        resultGenome(i) = getUnselectedtedChroma(plants, i)
    }

    Some(new OffspringPlant(resultGenome, plants.first.spec, getSelectionProbability(resultGenome)))
  }

  private def getHeterozygousSelectedChroma(plants: PlantPair, criteria: Criterion): Option[Chromosome] = {
    val chromaNum = criteria.chromasomeIndex
    val cMNum = criteria.cMIndex
    val plant = criteria.plant
    chromaCrosser.selectHeterozygousOffspring(plants.first.chromasomes(chromaNum), plants.second.chromasomes(chromaNum), plant, cMNum)
  }

  private def getUnselectedtedChroma(plants: PlantPair, chromaNum: Int): Chromosome = {
    chromaCrosser.getOffspringWithoutSelection(plants.first.chromasomes(chromaNum), plants.second.chromasomes(chromaNum)).get
  }

  def selectHomozygousOffspring(plants: PlantPair, criteriaList: Set[Criterion]): Option[OffspringPlant] = {
    checkCanCross(plants)

    val resultGenome = new Array[Chromosome](plants.first.spec.chromasomeLengths.size)
    var selectionProbability = 0.0
    var chromasomeSize = 0

    criteriaList.foreach(criteria => {
      val chromaNum = criteria.chromasomeIndex
      if (resultGenome(chromaNum) != null)
        throw new UnsupportedOperationException("Not supported yet: multiple criteria on single chromasome")
      val selectedChroma = getHomozygousSelectedChroma(plants, criteria)
      if (selectedChroma == None)
        return None
      resultGenome(chromaNum) = selectedChroma.get
    })

    for (i <- 0 until resultGenome.size) {
      if (resultGenome(i) == null)
        resultGenome(i) = getUnselectedtedChroma(plants, i)
    }

    Some(new OffspringPlant(resultGenome, plants.first.spec, getSelectionProbability(resultGenome)))
  }

  private def getHomozygousSelectedChroma(plants: PlantPair, criteria: Criterion): Option[Chromosome] = {
    val chromaNum = criteria.chromasomeIndex
    val cMNum = criteria.cMIndex
    val plant = criteria.plant
    chromaCrosser.selectHomozygousOffspring(plants.first.chromasomes(chromaNum), plants.second.chromasomes(chromaNum), plant, cMNum)
  }

  private def getSelectionProbability(chromasomes: Array[Chromosome]): Double = {
    var prob = 1.0

    chromasomes.foreach(chroma => {
      chroma.selectionProbability match {
        case Some(p) => prob *= p
        case _ => None
      }
    })

    prob
  }
}
