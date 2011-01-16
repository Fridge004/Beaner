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

class ChromosomeCrosser{
	
	def getOffspringWithoutSelection(
			firstChromasome:Chromosome,
			secondChromasome:Chromosome):Option[Chromosome] = {
		Some(new Chromosome(firstChromasome.makeGameteNoSelection, secondChromasome.makeGameteNoSelection))
	}
	
	def selectHomozygousOffspring(
			firstChromasome:Chromosome,
			secondChromasome:Chromosome,
			plantToSelectFor:Plant,
			positionForSelection:Int):Option[Chromosome] = {
		
		val selectionProbability = 
			firstChromasome.probabilityGameteContains(plantToSelectFor, positionForSelection)*
			secondChromasome.probabilityGameteContains(plantToSelectFor, positionForSelection)
		
		if(selectionProbability==0.0)
			None
		else
			Some(new Chromosome(
				firstChromasome.makeGameteSelectingFor(plantToSelectFor, positionForSelection),
				secondChromasome.makeGameteSelectingFor(plantToSelectFor, positionForSelection),
				selectionProbability
			))
	}
	
	def selectHeterozygousOffspring(
			firstChromasome:Chromosome, 
			secondChromasome:Chromosome, 
			plantToSelectFor:Plant, 
			positionForSelection:Int):Option[Chromosome] = {
		
		if(firstChromasome.probabilityGameteContains(plantToSelectFor, positionForSelection)>0){
			if(secondChromasome.probabilityGameteContains(plantToSelectFor, positionForSelection)>0){
				//Both chromatids
				throw new ChromasomeCrosserException("Not supported: Heterozygous selection when both chromasome can supply allele")
			}
			//Only first chromatid
			Some(getResultGivenSelectionFromSingleChromasome(firstChromasome, secondChromasome, plantToSelectFor:Plant, positionForSelection:Int))
		} else if(secondChromasome.probabilityGameteContains(plantToSelectFor, positionForSelection)>0){
			//Only second chromatid
			Some(getResultGivenSelectionFromSingleChromasome(secondChromasome, firstChromasome, plantToSelectFor:Plant, positionForSelection:Int))
		} else {
			None
		}
	}

	private def getResultGivenSelectionFromSingleChromasome(
    firstChromasome:Chromosome,
    secondChromasome:Chromosome,
    plantToSelectFor:Plant,
    positionForSelection:Int) = {

		var resultChromatidA:Chromatid = null
		var resultChromatidB:Chromatid = null
		
		resultChromatidA = firstChromasome.makeGameteSelectingFor(plantToSelectFor, positionForSelection)
		resultChromatidB = secondChromasome.makeGameteNoSelection;
		
		new Chromosome(resultChromatidA, resultChromatidB, firstChromasome.probabilityGameteContains(plantToSelectFor, positionForSelection))
	}
}
