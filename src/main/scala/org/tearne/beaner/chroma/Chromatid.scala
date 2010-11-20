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

package org.tearne.beaner.chroma

import org.tearne.beaner.plant._

class Chromatid(cMArray:Array[Centimorgan]) {
	def this(plant:Plant, size:Int) = this{
		val cMs = new Array[Centimorgan](size)
		for(i <- 0 until cMs.length)
			cMs(i) = new Centimorgan(plant)
		cMs
	}
	
	def this(that:Chromatid) = this{
		val cMs = new Array[Centimorgan](that.size)
		for(i <- 0 until cMs.length)
			cMs(i) = that(i)
		cMs
	}
	
	val size = cMArray.length
	
	def sumProbabilitiesOf(p:Plant):Double={
		var accumulatedProbability = 0.0
		for(i <- 0 until size){
			accumulatedProbability += cMArray(i).probabilityOf(p)
		}
		
		accumulatedProbability
	}
	
	def update(index:Int, value:Centimorgan){
		cMArray(index) = value
	}
	
	def apply(i:Int):Centimorgan = {
		cMArray(i)
	}
	
	def probabilityOf(p:Plant, index:Int):Double = {
		cMArray(index).probabilityOf(p)
	}
}
