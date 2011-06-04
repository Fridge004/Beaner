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

package org.tearne.beaner.criteria

import org.tearne.beaner.plant._

sealed abstract class SelectionCriterion

case class Criterion(val plant:Plant, val chromosomeIndex:Int, val cMIndex:Int) extends SelectionCriterion {
  def +(that:Criterion):Criteria = new Criteria(that, this)
}
case class DoubleCriterion(val criterion1: Criterion, val criterion2: Criterion) extends SelectionCriterion {
  if(criterion1.plant != criterion2.plant)
    throw new CriterionException("Donor plants must be equals for double selection on same chromosome")
  else if(criterion1.chromosomeIndex != criterion2.chromosomeIndex)
    throw new CriterionException("Chromasome index must be consistent for double selection")
  else if(criterion1.cMIndex == criterion2.cMIndex)
    throw new CriterionException("Centimorgans cannot be equal for double selection")
}