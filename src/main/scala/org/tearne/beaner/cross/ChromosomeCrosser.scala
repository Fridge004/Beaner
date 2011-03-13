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

import org.tearne.beaner.plant._
import org.tearne.beaner.chroma._

class ChromosomeCrosser(val gameter: Gameter){
	
	def getOffspringWithoutSelection(
			firstChromasome:Chromosome,
			secondChromasome:Chromosome):Chromosome = {
		new Chromosome(
      gameter.withoutSelection(firstChromasome),
      gameter.withoutSelection(secondChromasome)
    )
	}
	
	def selectHomozygousOffspring(
			firstChromasome:Chromosome,
			secondChromasome:Chromosome,
			plantToSelectFor:Plant,
			positionForSelection:Int):Chromosome = {

		val selectionProbability = 
			gameter.probContains(plantToSelectFor, positionForSelection, firstChromasome) *
			gameter.probContains(plantToSelectFor, positionForSelection, secondChromasome)
		
		if(selectionProbability==0.0)
			throw new ChromasomeCrosserException("Chromosomes can't both provide allele")
		else
		  new Chromosome(
				gameter.selectOn(plantToSelectFor, positionForSelection, firstChromasome),
				gameter.selectOn(plantToSelectFor, positionForSelection, secondChromasome),
				selectionProbability
			)
	}
	
	def selectHeterozygousOffspring(
			firstChromosome:Chromosome,
			secondChromosome:Chromosome,
			plantToSelectFor:Plant, 
			positionForSelection:Int):Chromosome = {

		if(gameter.probContains(plantToSelectFor, positionForSelection, firstChromosome)>0){
			if(gameter.probContains(plantToSelectFor, positionForSelection, secondChromosome)>0){
				//Both chromatids
				throw new ChromasomeCrosserException("Not supported: Heterozygous selection when both chromasome can supply allele")
			}
			//Only first chromatid
			getResultGivenSelectionFromSingleChromasome(firstChromosome, secondChromosome, plantToSelectFor:Plant, positionForSelection:Int)
		} else if(gameter.probContains(plantToSelectFor, positionForSelection, secondChromosome)>0){
			//Only second chromatid
			getResultGivenSelectionFromSingleChromasome(secondChromosome, firstChromosome, plantToSelectFor:Plant, positionForSelection:Int)
		} else {
			throw new ChromasomeCrosserException("Neither chromosome can provide allele")
		}
	}

	private def getResultGivenSelectionFromSingleChromasome(
      firstChromosome:Chromosome,
      secondChromosome:Chromosome,
      plantToSelectFor:Plant,
      positionForSelection:Int) = {

		val resultChromatidA = gameter.selectOn(plantToSelectFor, positionForSelection, firstChromosome)
		val resultChromatidB = gameter.withoutSelection(secondChromosome);
    val selectionProb = gameter.probContains(plantToSelectFor, positionForSelection, firstChromosome)

		new Chromosome(resultChromatidA, resultChromatidB, selectionProb)
	}
}
