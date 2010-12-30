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

class Centimorgan(val alleles:Map[Plant, Double]){
  def this(p:Plant) = this(Map(p->1.0))
  def this() = this(Map[Plant, Double]())

  val tolerance = 1e-10

  def combinedWith(cM: Centimorgan, proportionThis: Double): Centimorgan = {
    val proportionThat = 1.0 - proportionThis

    var resultAlleles = Map[Plant, Double]()
    for (a <- alleles.keySet)
      resultAlleles += (a ->(alleles(a) * proportionThis))
 
    for (a <- cM.alleles.keySet) {
      if (resultAlleles.contains(a))
        resultAlleles += (a->(cM.alleles(a) * proportionThat + resultAlleles(a)))
      else
        resultAlleles += (a->(cM.alleles(a) * proportionThat))
    }

    new Centimorgan(normalise(resultAlleles))
  }

  private def normalise(map:Map[Plant, Double]) = {
    val total = map.values.sum

    if(total != 1.0)
      if (scala.math.abs(total-1) > tolerance)
        throw new CentimorganException(
          "Cannot combine since this Centimorgan's values don't sum to 1.0 : Prob = " + total)
      else{
        val correction = 1-total
        val kvp = map.iterator.next
        map + (kvp._1 -> (kvp._2+correction))
      }
    else map
  }

  def probabilityOf(plant: Plant) = {
    if (alleles.contains(plant))
      alleles(plant)
    else
      0.0
  }

  def gameteify(that: Centimorgan, thisProb: Double) = {
    var sum = 0.0;
    for (i <- alleles.values)
      sum += i
    if (sum != 1.0)
      throw new CentimorganException("Probabilities don't add up to one")

    sum = 0.0;
    for (i <- that.alleles.values)
      sum += i
    if (sum != 1.0)
      throw new CentimorganException("Probabilities don't add up to one")

    var resultAlleles = Map[Plant, Double]()
    for (a <- this.alleles.keySet) {
      resultAlleles += (a->(this.alleles(a) * thisProb))
    }
    for (a <- that.alleles.keySet) {
      if (resultAlleles.contains(a))
        resultAlleles += (a->(resultAlleles(a) + that.alleles(a) * (1 - thisProb)))
      else
        resultAlleles += (a->(that.alleles(a) * (1 - thisProb)) )
    }

    new Centimorgan(resultAlleles)
  }
}
