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
import com.lowagie.text.PageSize
import org.tearne.beaner.cross.Criterion

class PlantPrinter(plants: List[NamedPlant], criteria: Set[Criterion], colour: Colour) extends processing.core.PApplet {
  val margin = 0.05
  val headerSpace = 20
  private val f:PFont = createFont("GillSans-Bold", 10);

  def makePdf(){
    this.init
  }
  
  override def setup() {
    val page = PageSize.A5
    size(page.getWidth.asInstanceOf[Int], page.getHeight().asInstanceOf[Int], processing.core.PConstants.PDF, "output.pdf")
    background(255)

    stroke(0);
    strokeWeight(0.1f);

    val pGraphics = g.asInstanceOf[PGraphicsPDF]
   
    val plantIterator = plants.iterator
    while(plantIterator.hasNext){
      printPlant(plantIterator.next)
      if(plantIterator.hasNext)
	      pGraphics.nextPage
    }
  }

  private def printPlant(namedPlant:NamedPlant){
    val numChromasomes = namedPlant.plant.chromasomes.size

    val widthWithoutMargin = (width*(1-margin)).asInstanceOf[Int]
    val marginWidth = width - widthWithoutMargin
    val marginHeight = (height * margin /2.0).asInstanceOf[Int]

    //pushMatrix
    //translate(0,marginHeight)
    textFont(f,12)
    fill(0)
    textAlign(processing.core.PConstants.LEFT)
    text(namedPlant.name,10f,10f)
    if(namedPlant.plant.isInstanceOf[OffspringPlant])
      text("%.1f".format(100*namedPlant.plant.asInstanceOf[OffspringPlant].proportionOf(colour.prefVar))+"%",10f,20f)
    translate(0,headerSpace)
    //text

    namedPlant.plant.chromasomes.zipWithIndex.foreach {
      case (chromasome, index) => {
        pushMatrix
        val xTrans = (marginWidth/2.0+(widthWithoutMargin*index) / numChromasomes.asInstanceOf[Double]).asInstanceOf[Int]
        translate(xTrans, headerSpace)
        new ChromasomeView(chromasome, colour, this).display
        popMatrix
      }
    }

    //popMatrix
  }
}

object PlantPrinter {
  //For testing
  def main(args: Array[String]) {
    import org.tearne.beaner.plant._
    import org.tearne.beaner.cross._

    //
    // Setup
    //
    PlantPair.setPlantCrosser(new PlantCrosser(new ChromosomeCrosser()))

    val p1 = PhaseolusVulgaris()
    val p2 = PhaseolusVulgaris()
    val p3 = PhaseolusVulgaris()
    val p4 = PhaseolusVulgaris()
    val pV = PhaseolusVulgaris()

    val c1 = new Criterion(p1, 1, 9)
    val c2 = new Criterion(p2, 3, 50)
    val c3 = new Criterion(p3, 4, 24)
    val c4 = new Criterion(p4, 7, 36)
    val cAll = c1 + c2 + c3 + c4

    val f1_p1p2 = p1 x p2 selectHet c1 + c2
    val f1_p3p4 = p3 x p4 selectHet c3 + c4
    val f1_p1p2p3p4 = f1_p1p2 x f1_p3p4 selectHet cAll

    val bc1 = f1_p1p2p3p4 x pV selectHet cAll
    val bc2 = bc1 x pV selectHet cAll
    val bc3 = bc2 x pV selectHet cAll
    val bc4 = bc3 x pV selectHet cAll

    val fin = bc4 x bc4 selectHom cAll

    //Make colour object
    val colours = new Colour(cAll, pV)

    val plantsList = List(
      NamedPlant(pV, "Pref Var"),
      NamedPlant(p1, "First Donor"),
      NamedPlant(p2, "Second Donor"),
      NamedPlant(p3, "Third Donor"),
      NamedPlant(p4, "Fourth Donor"),
      NamedPlant(f1_p1p2, "F1 (p1 x p2)"),
      NamedPlant(f1_p3p4, "F1 (p3 x p4)"),
      NamedPlant(f1_p1p2p3p4, "F1 ((p1 x p2) x (p3 x p4))"),
      NamedPlant(bc1, "Backcross 1"),
      NamedPlant(bc2, "Backcross 2"),
      NamedPlant(bc3, "Backcross 3"),
      NamedPlant(bc4, "Backcross 4"),
      NamedPlant(fin, "Selfed")
    )

    new PlantPrinter(plantsList, cAll, colours).makePdf()
  }
}
