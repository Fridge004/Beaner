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

import org.tearne.beaner.plant._
import processing.pdf._
import processing.core._
import org.tearne.beaner.cross.Criterion

class PlantPrinter(plant: Plant, criteria: Set[Criterion], colour: Colour, position: (Int, Int), size: (Int,Int), parent: Reporter){
  val margin = 0.05
  val headerSpace = 20

  def display() {
    val (xPos, yPos) = position
    parent.textFont(parent.f,12)

    //parent.pushMatrix
    parent.pushMatrix
    parent.translate(xPos, yPos)

    printPlant(plant, size)

    parent.popMatrix
    //parent.popMatrix
  }

  private def printPlant(plant:Plant, size: (Int,Int)){
    val (width, height) = size
    val numChromasomes = plant.chromasomes.size

    val widthWithoutMargin = (width*(1-margin)).asInstanceOf[Int]
    val marginWidth = width - widthWithoutMargin
    val marginHeight = (height * margin /2.0).asInstanceOf[Int]

    //pushMatrix
    //translate(0,marginHeight)
    parent.fill(0)
    parent.textAlign(processing.core.PConstants.LEFT)
    parent.text(plant.name.getOrElse(""),10f,10f)
    if(plant.isInstanceOf[OffspringPlant])
      parent.text("%.1f".format(100*plant.asInstanceOf[OffspringPlant].proportionOf(colour.prefVar))+"%",10f,22f)
    parent.translate(0,headerSpace)
    //text

    plant.chromasomes.zipWithIndex.foreach {
      case (chromasome, index) => {
        parent.pushMatrix
        val xTrans = (marginWidth/2.0+(widthWithoutMargin*index) / numChromasomes.asInstanceOf[Double]).asInstanceOf[Int]
        parent.translate(xTrans, headerSpace)
        new ChromasomeView(chromasome, colour, parent).display
        parent.popMatrix
      }
    }

    //popMatrix
  }
}
