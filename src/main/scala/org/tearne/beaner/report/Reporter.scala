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

import com.lowagie.text.PageSize
import org.tearne.beaner.cross.Criterion
import org.tearne.beaner.plant.Plant
import processing.core.{PApplet, PFont}
import processing.pdf.PGraphicsPDF

class Reporter(plants: List[Plant], criteria: Set[Criterion], colour: Colour) extends PApplet {

  private val fontName = PGraphicsPDF.listFonts().toList.find(f=>f.contains("Arial"))
  println("Auto selected font: "+fontName.getOrElse("[Error, not font found]"))
  val f:PFont = createFont(fontName.getOrElse("Arial Black"), 10);

  private val pageSplit = 0.3

  def makePDF(){
    this.init
  }

  override def setup(){
    textFont(f,12)
    val page = PageSize.A5
    size(
      page.getHeight.asInstanceOf[Int],
      page.getWidth.asInstanceOf[Int],
      processing.core.PConstants.PDF,
      "Output.pdf"
    )
    background(255)

    stroke(0)
    strokeWeight(0.1f)
    noLoop()

    val pGraphics = g.asInstanceOf[PGraphicsPDF]

    val plantIterator = plants.iterator
    while(plantIterator.hasNext){
      val plant = plantIterator.next
      new PlantPrinter(plant, criteria, colour, (200,10), (400,0), this).display
      new StatsPrinter((2,50), plant, this).display
      if(plantIterator.hasNext)
	      pGraphics.nextPage
    }
  }
}

object Reporter {
   //For testing
  def main(args: Array[String]) {
//    // --- Preamble ---
//    import org.tearne.beaner.plant._
//    import org.tearne.beaner.cross._
//    PlantPair.setPlantCrosser(new PlantCrosser(new ChromosomeCrosser()))
//
//    // --- Setup ---
//    // Starting plants
//    val pV = PhaseolusVulgaris("Pref Var")
//    val p1 = PhaseolusVulgaris("Donor1")
//    val p2 = PhaseolusVulgaris("Donor2")
//    val p3 = PhaseolusVulgaris("Donor3")
//    val p4 = PhaseolusVulgaris("Donor4")
//    // Selection criteria
//    val c1 = new Criterion(p1, 1, 9)
//    val c2 = new Criterion(p2, 3, 50)
//    val c3 = new Criterion(p3, 4, 24)
//    val c4 = new Criterion(p4, 7, 36)
//    val cAll = c1 + c2 + c3 + c4
//
//
//    // --- Crossings ---
//    // F1s
//    val f1_p1p2 = (p1 x p2) selectHet (c1 + c2) named "F1_p1p2"
//    val f1_p3p4 = (p3 x p4) selectHet (c3 + c4) named "F1_p3p4"
//    val f1_p1p2p3p4 = (f1_p1p2 x f1_p3p4) selectHet cAll named "F1_p1p2p3p4"
//    // Backcrossing
//    val bc1 = f1_p1p2p3p4 x pV selectHet cAll named "BC1"
//    val bc2 = bc1 x pV selectHet cAll named "BC2"
//    val bc3 = bc2 x pV selectHet cAll named "BC3"
//    val bc4 = bc3 x pV selectHet cAll named "BC4"
//    // Selfing
//    val fin = bc4 x bc4 selectHom cAll named "Final"
//
//    // --- Reporting ---
//    // Prepare for reporting
//    val colours = new Colour(cAll, pV)
//    val plantsList = List(
//      pV, p1, p2, p3, p4,
//      f1_p1p2, f1_p3p4, f1_p1p2p3p4,
//      bc1, bc2, bc3, bc4,
//      fin
//    )
//    // Produce PDF
//    new Reporter(plantsList, cAll, colours).makePDF()
  }
}