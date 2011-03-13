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

sealed trait ParentPair{
  def selectHet(criteria: Set[Criterion]): Cross = new Cross(this, criteria, Option(SelectionType.Heterozygous))
  def selectHom(criteria: Set[Criterion]): Cross = new Cross(this, criteria, Option(SelectionType.Homozygous))

}

case class CrossPair(val first: Cross, val second: Cross) extends ParentPair

case class MixedPair(val first: Cross, val second: Plant) extends ParentPair

case class PlantPair(val first:Plant, val second:Plant) extends ParentPair{
  def bothNamed() = first.name != None && second.name != None
}

object Parents{
  def apply(first: Plant, second: Plant) = new PlantPair(first, second)
  def apply(first: Cross, second: Plant) = new MixedPair(first, second)
  def apply(first: Cross, second: Cross) = new CrossPair(first, second)
  def apply(first: Plant, second: Cross): MixedPair = apply(second, first)
}
