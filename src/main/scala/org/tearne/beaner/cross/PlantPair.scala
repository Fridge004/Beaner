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
import org.tearne.beaner.cross._

class PlantPair(val first:Plant, val second:Plant) { 
  import PlantPair.plantCrosser

  def selectHet(criteria:Set[Criterion]):OffspringPlant = {
    plantCrosser.selectHeterozygousOffspring(this, criteria).getOrElse{
	      throw new OffspringPlantException()
      }
  }
  
  def selectHom(criteria:Set[Criterion]):OffspringPlant = {
      plantCrosser.selectHomozygousOffspring(this, criteria).getOrElse{
	      throw new OffspringPlantException()
      }
  }
}
object PlantPair{
  private var plantCrosser:PlantCrosser = null
  
  def apply(first:Plant, second:Plant) = new PlantPair(first, second)
  def setPlantCrosser(crosser:PlantCrosser) = plantCrosser = crosser
}
