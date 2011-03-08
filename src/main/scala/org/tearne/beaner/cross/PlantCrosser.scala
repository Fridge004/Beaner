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

class PlantCrosser(chromaCrosser: ChromosomeCrosser) {

  private def checkCanCross(pair: PlantPair) = {
    if (pair.first.spec != pair.second.spec)
      throw new PlantCrosserException("Cannot cross plants with different specs")
    if(!pair.bothNamed)
      throw new UnnamedPlantException()
  }

  def selectHeterozygousOffspring(pair: PlantPair, criteria: Set[Criterion]): Option[OffspringPlant] = {
    checkCanCross(pair)

    val resultGenome = new Array[Chromosome](pair.first.spec.chromosomeLengths.size)
    var selectionProbability = 0.0;

    criteria.foreach(criteria => {
      val chromaNum = criteria.chromasomeIndex
      if (resultGenome(chromaNum) != null)
        throw new UnsupportedOperationException("Not supported yet: multiple criteria on single chromasome")

      val selectedChroma = getHeterozygousSelectedChroma(pair, criteria)
      if (selectedChroma == None)
        return None
      resultGenome(chromaNum) = selectedChroma.get
    })

    for (i <- 0 until resultGenome.size) {
      if (resultGenome(i) == null)
        resultGenome(i) = getUnselectedtedChroma(pair, i)
    }

    Some(new OffspringPlant(resultGenome, pair.first.spec, getSelectionProbability(resultGenome)))
  }

  private def getHeterozygousSelectedChroma(pair: PlantPair, criterion: Criterion): Option[Chromosome] = {
    val chromaNum = criterion.chromasomeIndex
    val cMNum = criterion.cMIndex
    val plant = criterion.plant
    chromaCrosser.selectHeterozygousOffspring(pair.first.chromosomes(chromaNum), pair.second.chromosomes(chromaNum), plant, cMNum)
  }

  private def getUnselectedtedChroma(pair: PlantPair, chromaNum: Int): Chromosome = {
    chromaCrosser.getOffspringWithoutSelection(pair.first.chromosomes(chromaNum), pair.second.chromosomes(chromaNum)).get
  }

  def selectHomozygousOffspring(pair: PlantPair, criteriaList: Set[Criterion]): Option[OffspringPlant] = {
    checkCanCross(pair)

    val resultGenome = new Array[Chromosome](pair.first.spec.chromosomeLengths.size)
    var selectionProbability = 0.0
    var chromasomeSize = 0

    criteriaList.foreach(criteria => {
      val chromaNum = criteria.chromasomeIndex
      if (resultGenome(chromaNum) != null)
        throw new UnsupportedOperationException("Not supported yet: multiple criteria on single chromasome")
      val selectedChroma = getHomozygousSelectedChroma(pair, criteria)
      if (selectedChroma == None)
        return None
      resultGenome(chromaNum) = selectedChroma.get
    })

    for (i <- 0 until resultGenome.size) {
      if (resultGenome(i) == null)
        resultGenome(i) = getUnselectedtedChroma(pair, i)
    }

    Some(new OffspringPlant(resultGenome, pair.first.spec, getSelectionProbability(resultGenome)))
  }

  private def getHomozygousSelectedChroma(pair: PlantPair, criteria: Criterion): Option[Chromosome] = {
    val chromaNum = criteria.chromasomeIndex
    val cMNum = criteria.cMIndex
    val plant = criteria.plant
    chromaCrosser.selectHomozygousOffspring(pair.first.chromosomes(chromaNum), pair.second.chromosomes(chromaNum), plant, cMNum)
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
