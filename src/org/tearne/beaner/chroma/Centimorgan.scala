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
package beaner.chroma

import beaner.plant._
import scala.collection.mutable.Map

class Centimorgan(p:Plant) {
	def this() = this(null)
	
	val alleles = Map[Plant,Double]()
	
	if(p!=null)
		alleles(p)=1.0
	
	def combinedWith(cM:Centimorgan, proportionThis:Double):Centimorgan = {
		var total = 0.0;
		for(prob <- alleles.values){
			total += prob
		}
		if(total<1.0)
			throw new CentimorganException("Cannot combine since this Centimorgan's values don't sum to 1.0")
		
		total=0.0
		for(prob <- cM.alleles.values){
			total += prob
		}
		if(total<1.0)
			throw new CentimorganException("Cannot combine with chromasome who's values don't sum to 1.0")
		
		val result = new Centimorgan()
		val proportionThat = 1.0-proportionThis
		
		for(a <- this.alleles.keySet){
			result.alleles(a) = this.alleles(a)*proportionThis
		}
		for(a <- cM.alleles.keySet){
			if(result.alleles.contains(a))
				result.alleles(a) = cM.alleles(a)*proportionThat+result.alleles(a)
			else
				result.alleles(a) = cM.alleles(a)*proportionThat
		}
		
		result
	}
		
	def probabilityOf(plant:Plant) = {
		if(alleles.contains(plant))
			alleles(plant)
		else
			0.0
	}
		
	def gameteify(that:Centimorgan, thisProb:Double) = {
		var sum = 0.0;
		for(i<-alleles.values)
			sum += i
		if(sum!= 1.0)
			throw new CentimorganException("Probabilities don't add up to one")
		
		sum = 0.0;
		for(i<-that.alleles.values)
			sum += i
		if(sum!= 1.0)
			throw new CentimorganException("Probabilities don't add up to one")
		
		val result = new Centimorgan()
		
		for(a <- this.alleles.keySet){
			result.alleles(a) = this.alleles(a)*thisProb
		}
		for(a <- that.alleles.keySet){
			if(result.alleles.contains(a))
				result.alleles(a) = result.alleles(a)+that.alleles(a)*(1-thisProb)
			else
				result.alleles(a) = that.alleles(a)*(1-thisProb)
		}
		
		result
	}
}
