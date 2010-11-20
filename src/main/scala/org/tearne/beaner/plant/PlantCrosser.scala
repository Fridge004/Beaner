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
import org.tearne.beaner.plant.selection._

class PlantCrosser(firstPlant: Plant, secondPlant:Plant, chromaCrosser:ChromasomeCrosser) {

	if(firstPlant.spec != secondPlant.spec)
		throw new PlantCrosserException("Cannot cross plants with different specs")
	
	def selectHeterozygousOffspring(criteriaList:List[Criteria]):Option[OffspringPlant] = {
		val resultGenome = new Array[Chromasome](firstPlant.spec.chromasomeLengths.size)
		var selectionProbability = 0.0;
		
		criteriaList.foreach(criteria=>{
			val chromaNum = criteria.chromasomeIndex
			if(resultGenome(chromaNum)!=null)
				throw new UnsupportedOperationException("Not supported yet: multiple criteria on single chromasome")
			
			val selectedChroma = getHeterozygousSelectedChroma(criteria)
			if(selectedChroma==None)
				return None
			resultGenome(chromaNum) = selectedChroma.get
		})
		
		for(i<-0 until resultGenome.size){
			if(resultGenome(i)==null)
				resultGenome(i) = getUnselectedtedChroma(i)
		}
		
		Some(new OffspringPlant("Cross", resultGenome, firstPlant.spec, getSelectionProbability(resultGenome)))
	}
	
	private def getHeterozygousSelectedChroma(criteria:Criteria):Option[Chromasome] = {
			val chromaNum = criteria.chromasomeIndex
			val cMNum = criteria.cMIndex
			val plant = criteria.plant
			chromaCrosser.selectHeterozygousOffspring(firstPlant.chromasomes(chromaNum), secondPlant.chromasomes(chromaNum), plant, cMNum)
	}
	
	private def getUnselectedtedChroma(chromaNum:Int):Chromasome = {
		chromaCrosser.getOffspringWithoutSelection(firstPlant.chromasomes(chromaNum), secondPlant.chromasomes(chromaNum)).get
	}
	
	def selectHomozygousOffspring(criteriaList:List[Criteria]):Option[OffspringPlant] = {
		val resultGenome = new Array[Chromasome](firstPlant.spec.chromasomeLengths.size)
		var selectionProbability = 0.0
		var chromasomeSize = 0
		
		criteriaList.foreach(criteria=>{
			val chromaNum = criteria.chromasomeIndex
			if(resultGenome(chromaNum)!=null)
				throw new UnsupportedOperationException("Not supported yet: multiple criteria on single chromasome")
			val selectedChroma = getHomozygousSelectedChroma(criteria)
			if(selectedChroma==None)
				return None
			resultGenome(chromaNum) = selectedChroma.get
		})
		
		for(i<-0 until resultGenome.size){
			if(resultGenome(i)==null)
				resultGenome(i) = getUnselectedtedChroma(i)
		}
		
		Some(new OffspringPlant("Cross", resultGenome, firstPlant.spec, getSelectionProbability(resultGenome)))
	}
	
	private def getHomozygousSelectedChroma(criteria:Criteria):Option[Chromasome] = {
		val chromaNum = criteria.chromasomeIndex
		val cMNum = criteria.cMIndex
		val plant = criteria.plant
		chromaCrosser.selectHomozygousOffspring(firstPlant.chromasomes(chromaNum), secondPlant.chromasomes(chromaNum), plant, cMNum)
	}
	
	private def getSelectionProbability(chromasomes:Array[Chromasome]):Double = {
		var prob = 1.0
		
		chromasomes.foreach(chroma=>{
			chroma.selectionProbability match{
				case Some(p) => prob *= p
				case _ => None
			}
		})
		
		prob
	}
}
