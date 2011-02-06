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

package org.tearne.beaner.report

import processing.core.PApplet
import org.tearne.beaner.plant.{OffspringPlant, Plant}

class StatsPrinter(position: (Int, Int), plant: Plant, parent: Reporter){

  def display(){
    parent.textFont(parent.f,12)
    parent.fill(0)

    if(plant.isInstanceOf[OffspringPlant]){

      parent.pushMatrix()
      parent.translate(position._1, position._2)
      plotStats()
      parent.popMatrix()
    }
  }

  private def plotStats(){
    parent.text("Hello Bean")
  }
}
