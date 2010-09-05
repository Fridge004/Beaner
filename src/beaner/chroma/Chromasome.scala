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
import beaner.chroma.exception._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith


@RunWith(classOf[JUnitRunner])
class Chromasome(val firstChromatid:Chromatid, val secondChromatid:Chromatid, val selectionProbability:Option[Double]) {
	def this(firstChromatid:Chromatid, secondChromatid:Chromatid, selectionProbability:Double) 
		= this(firstChromatid, secondChromatid, Some(selectionProbability))
	def this(firstChromatid:Chromatid, secondChromatid:Chromatid) 
		= this(firstChromatid, secondChromatid, None)
	def this(plant:Plant, size:Int) 
	= this(new Chromatid(plant,size), new Chromatid(plant,size))
	
	if(firstChromatid.size != secondChromatid.size)
			throw new ChromasomeException("Chromatids are not of the same size")
	
	val size = firstChromatid.size
	
	def probabilityGameteContains(plant:Plant, index:Int) = {		
		0.5*(firstChromatid.probabilityOf(plant, index) + secondChromatid.probabilityOf(plant, index))
	}
	
	def makeGameteSelectingFor(plant:Plant, index:Int):Chromatid = {
		if(firstChromatid(index).probabilityOf(plant)>0){
			if(secondChromatid(index).probabilityOf(plant)>0){
				makeGameteSelectingBothTids(index, plant)
			}
			else
				makeGameteSelectingSingleTid(true, index, plant)
		}else if(secondChromatid(index).probabilityOf(plant)>0){
			makeGameteSelectingSingleTid(false, index, plant)
		}else{
			//Not possible to select
			null
		}
	}
	
	private def makeGameteSelectingBothTids(index:Int, plant:Plant)={
		val gamete = new Chromatid(firstChromatid)
		for(i <- 0  until gamete.size){
				if(i==index && (firstChromatid(i).alleles(plant)!=1.0 || secondChromatid(i).alleles(plant)!=1.0))
					throw new ChromasomeException("Can only select when allele is present with probability on one")
				else
					gamete(i) = new Centimorgan(plant)	
			gamete(i) = firstChromatid(i).combinedWith(secondChromatid(i), 0.5)
		}
		gamete
	}
	
	private def makeGameteSelectingSingleTid(selectFirstChromatid:Boolean, index:Int, plant:Plant)={
		var cMa, cMb:Centimorgan = null
		val gamete = new Chromatid(firstChromatid)
		var tid1, tid2:Chromatid = null
		if(selectFirstChromatid){
			tid1 = firstChromatid
			tid2 = secondChromatid
		}
		else{
			tid1 = secondChromatid
			tid2 = firstChromatid
		}
		
		for(i <- 0  until gamete.size){
			if(i==index)
				if(tid1(i).alleles(plant)!=1.0)
					throw new ChromasomeException("Can only select when allele is present with probability on one of the chromatids")
				else
					gamete(i) = new Centimorgan(plant)		
			else{
				cMa = tid1(i); cMb = tid2(i)
				gamete(i) = cMa.combinedWith(cMb, probInAtDist(index-i))
			}
		}
		gamete
	}
	
	private def probInAtDist(distance:Int):Double = {
		val dist = Math.abs(distance)
		if(dist > 1){
			val probIN = probInAtDist(dist-1)
			0.99*probIN+0.01*(1-probIN)
		}
		else
			0.99	
	}
	
	def makeGameteNoSelection:Chromatid = {
		val gameteArray = new Array[Centimorgan](firstChromatid.size)
		for(i<-0 until firstChromatid.size){
			gameteArray(i) = firstChromatid(i).gameteify(secondChromatid(i), 0.5)
		}
		
		new Chromatid(gameteArray)
	}
	
	def proportionOf(plant:Plant):Double = {
		val totalNumCentimorgans:Double = 2*size
		
		(firstChromatid.sumProbabilitiesOf(plant)+secondChromatid.sumProbabilitiesOf(plant))/totalNumCentimorgans
	}
}
