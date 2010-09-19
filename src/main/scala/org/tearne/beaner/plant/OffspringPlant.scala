/*******************************************************************************
 * Copyright (c) 2010 Oliver Tearne (tearne at gmail dot com).
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/
package org.tearne.beaner.plant

import org.tearne.beaner.chroma._
import org.tearne.beaner.plant.spec._

class OffspringPlant(val name:String, val chromasomes:Array[Chromasome], val spec:PlantSpec, val selectionProbability:Option[Double]) extends Plant{
	def this(name:String, chromasomes:Array[Chromasome], spec:PlantSpec) = this(name, chromasomes, spec, None)
	def this(name:String, chromasomes:Array[Chromasome], spec:PlantSpec, selectionProbability:Double) =
		this(name, chromasomes, spec, {
			if(selectionProbability > 1.0 || selectionProbability < 0.0)
				throw new OffspringPlantException("Selection probability out of range: "+selectionProbability)
			Some(selectionProbability)
		})
	
	for(i <- 0 until chromasomes.size){
		val length1 = chromasomes(i).size
		val length = spec.chromasomeLengths(i)
		if(length1  != length)
			throw new OffspringPlantException("Chromasome "+i+" does not match spec (actual="+chromasomes(i).size+",spec="+spec.chromasomeLengths(i)+")")
	}
	
	def proportionOf(plant:Plant):Double = {
		var sumP = 0.0
		var cMCount = 0
		for(i <- 0 until chromasomes.size){
			sumP += chromasomes(i).proportionOf(plant)*spec.chromasomeLengths(i)
			cMCount += spec.chromasomeLengths(i)
		}
		
		sumP / cMCount
	}
}
