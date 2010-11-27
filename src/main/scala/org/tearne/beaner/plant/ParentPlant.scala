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

import org.tearne.beaner.plant.spec.PlantSpec
import org.tearne.beaner.chroma._

class ParentPlant(val spec:PlantSpec) extends Plant{
	
	val chromasomes:Array[Chromasome] = new Array[Chromasome](spec.chromasomeLengths.size)
	
	for(i<- 0 until spec.chromasomeLengths.size){
		chromasomes(i) = new Chromasome(this, spec.chromasomeLengths(i))
	}
}
