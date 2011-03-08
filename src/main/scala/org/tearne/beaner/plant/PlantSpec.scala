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

trait PlantSpec {
  val chromosomeLengths: Array[Int]

  def apply(name: String) = new ParentPlant(this, Option(name))

	override def equals(other: Any): Boolean = {
		other match {
			case that: PlantSpec => {
				val a = (that canEqual this)
				val b = chromosomeLengths.sameElements(that.chromosomeLengths)
				a && b
			}
			case _ => false
		}
	}
	
	def canEqual(other:Any):Boolean = other.isInstanceOf[PlantSpec]
	
	override def hashCode: Int = {
		chromosomeLengths.foldLeft(41)((b,a) => 41*(a)+b)
	}
}