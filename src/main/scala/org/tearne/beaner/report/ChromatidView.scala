package org.tearne.beaner.report

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

import org.tearne.beaner.chroma._
import collection.mutable.ListBuffer
import com.itextpdf.text.pdf._

object ChromatidView {

  val cMHeight = 2
  private class Data(val chromatid:Chromatid, val colour:Colour, val canvas:PdfTemplate)

  def apply(chromatid:Chromatid, colour:Colour, canvas:PdfTemplate) = {
    val length = chromatid.size

    val data = new Data(chromatid, colour, canvas)

    drawCMColours(data)
    drawOutline(data)

    canvas
  }

  private def drawCMColours(data:Data) {
    import data._

    val nonTrivial = isNonTrivialChromatid(data, 0)
    var highlights = new ListBuffer[Int]()
    val canvHeight = canvas.getHeight

    chromatid.cMArray.zipWithIndex.foreach{
      case (cM,index) => {
        val c = colour(cM)
	      canvas.setColorFill(c)
        if(nonTrivial && cM.alleles.size == 1)
          highlights += index
        canvas.rectangle(0, canvHeight-cMHeight*(index+1), canvas.getWidth, cMHeight)
        canvas.fill
    }}

    canvas.setLineWidth(1)
    highlights.foreach{index =>
      canvas.rectangle(0, canvHeight-cMHeight*(index+1), canvas.getWidth, cMHeight)
      canvas.stroke
    }
  }

  private def drawOutline(data:Data) {
    import data._

    val canvHeight = canvas.getHeight
    val numCentimorgans = chromatid.size
    val height = numCentimorgans*cMHeight

    canvas.setLineWidth(1)
    canvas.rectangle(0, canvHeight-height, canvas.getWidth, height)
    canvas.stroke
  }

  private def isNonTrivialChromatid(data:Data, startIndex: Int): Boolean = {
    import data._

    if(chromatid.cMArray(startIndex).alleles.size > 1) true
    else if(startIndex+1 == chromatid.cMArray.size ) false
    else isNonTrivialChromatid(data, startIndex+1)
  }
}