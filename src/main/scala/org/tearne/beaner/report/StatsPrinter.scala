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

import com.itextpdf.text._
import com.itextpdf.text.pdf._
import org.tearne.beaner.plant.{OffspringPlant, Plant}

object StatsPrinter{

  val helvetica: Font = new Font(Font.FontFamily.HELVETICA, 12)
  val font: BaseFont = helvetica.getCalculatedBaseFont(false)

  def apply(plant: Plant, canvas: PdfTemplate): PdfTemplate = {
    //TODO do this with patten matching
    if(plant.isInstanceOf[OffspringPlant]){
      plotStats(plant.asInstanceOf[OffspringPlant], canvas)
    }

    canvas
  }

  private def plotStats(p: OffspringPlant, canvas: PdfTemplate){
    val str = "Test "+p.numPlantsForConfidence(0.95, 5)+" for a 95% chance\nof selecting 5"

    val doc = canvas.getPdfDocument

    doc.add(new Paragraph(str))

//    canvas.beginText
//    canvas.setFontAndSize(font, 8)
//    canvas.showTextAligned(
//        Element.ALIGN_LEFT,
//        str,
//        0,
//        canvas.getHeight-20,
//        0
//    )
//    canvas.endText
  }
}
