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

import org.tearne.beaner._
import plant._
import criteria._
import cross._
import model._

import com.itextpdf.text.{Document, Paragraph, Font, Element, PageSize}
import com.itextpdf.text.pdf._
import java.io.FileOutputStream

class Reporter(filePath: String, plants: List[Plant], criteria: Criteria, colour: Colour){
  val document: Document = new Document()
  document.setPageSize(PageSize.A5.rotate)
  document.setMargins(10,10,10,10)

  val writer: PdfWriter = PdfWriter.getInstance(document, new FileOutputStream(filePath));
  var canvas: PdfContentByte = null
  val helvetica: Font = new Font(Font.FontFamily.HELVETICA, 12)
  val font: BaseFont = helvetica.getCalculatedBaseFont(false)

  def makePDF(){
    document.open()
    canvas = writer.getDirectContentUnder()

//    canvas.setFontAndSize(font, 12)
//    canvas.beginText
//    canvas.showTextAligned(Element.ALIGN_TOP, "Hello world", 10, document.top, 0)
//    canvas.endText

    val plantIterator = plants.iterator
    while(plantIterator.hasNext){
      val plant = plantIterator.next

      var templateRight = canvas.createTemplate(document.right-200, document.top)
      templateRight = PlantPrinter(plant, criteria, colour, templateRight)
      canvas.addTemplate(templateRight, 200, 0)

      var templateLeft = canvas.createTemplate(200, document.top)
      templateLeft = StatsPrinter(plant, templateLeft)
      canvas.addTemplate(templateLeft, 0, 0)

      if(plantIterator.hasNext)
	      document.newPage
    }


    canvas.fill()
    document.close()
  }

  def createTemplate(w:Float, h:Float):PdfTemplate = canvas.createTemplate(w,h)

}

object Reporter {
  def main(args: Array[String]) {
    // --- Preamble ---
    import org.tearne.beaner.plant._
    import org.tearne.beaner.cross._
    import org.tearne.beaner.model._
    import org.tearne.beaner.chroma._

    val plantCrosser1 = new PlantCrosser(new ChromosomeCrosser(new Gameter(new HaldaneRecombinationModel)))
    val plantCrosser2 = new PlantCrosser(new ChromosomeCrosser(new Gameter(new SingleRecombinationModel)))
    val plantCrosser3 = new PlantCrosser(new ChromosomeCrosser(new Gameter(new NoDragModel)))


    // --- Setup ---
    // Starting plants
    val pV = PhaseolusVulgaris("Pref Var")
    val p1 = PhaseolusVulgaris("Donor1")
    val p2 = PhaseolusVulgaris("Donor2")
    val p3 = PhaseolusVulgaris("Donor3")
    val p4 = PhaseolusVulgaris("Donor4")
    // Selection criteria
    val c1 = new Criterion(p1, 1, 9)
    val c2 = new Criterion(p2, 3, 50)
    val c3 = new Criterion(p3, 4, 24)
    val c4 = new Criterion(p4, 7, 36)
    val cAll = c1 + c2 + c3 + c4


    // --- Crossings ---
    // F1s
    val f1_p1p2 = (p1 x p2) selectHet (c1 + c2) named "F1_p1p2"
    val f1_p3p4 = (p3 x p4) selectHet (c3 + c4) named "F1_p3p4"
    val f1_p1p2p3p4 = (f1_p1p2 x f1_p3p4) selectHet cAll named "F1_p1p2p3p4"
    // Backcrossing
    val bc1 = f1_p1p2p3p4 x pV selectHet cAll named "BC1"
    val bc2 = bc1 x pV selectHet cAll named "BC2"
    val bc3 = bc2 x pV selectHet cAll named "BC3"
    val bc4 = bc3 x pV selectHet cAll named "BC4"
    // Selfing
    val fin = bc4 x bc4 selectHom cAll named "Final"

    // --- Reporting ---
    // Prepare for reporting
    val colours = new Colour(cAll, pV)
    val plantsList = List(
      fin.evaluateUsing(plantCrosser1),
      fin.evaluateUsing(plantCrosser2),
      fin.evaluateUsing(plantCrosser3)
    )
    // Produce PDF
    val path = "output.pdf"
    new Reporter(path, plantsList, cAll, colours).makePDF()
  }
}