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
import scala.collection.immutable._

class Criterion(val plant:Plant, val chromasomeIndex:Int, val cMIndex:Int){
  def +(that:Criterion):Set[Criterion] = {
    val result1:Set[Criterion] = new HashSet[Criterion]()+this
    val result2:Set[Criterion] = result1 + that
    result2
  }
}
