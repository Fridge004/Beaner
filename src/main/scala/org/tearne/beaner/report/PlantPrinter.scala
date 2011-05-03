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
import org.tearne.beaner.cross.{Criterion, Criteria}

import com.itextpdf.text._
import com.itextpdf.text.pdf._

object PlantPrinter{

  val helvetica: Font = new Font(Font.FontFamily.HELVETICA, 12)
  val font: BaseFont = helvetica.getCalculatedBaseFont(false)

  def apply(plant:Plant, criteria:Criteria, colour:Colour, canvas:PdfTemplate):PdfTemplate = {
    val numChromasomes = plant.chromosomes.size

    var title = plant.name.getOrElse("")

    if(plant.isInstanceOf[OffspringPlant])
      title += ": %.1f".format(100*plant.asInstanceOf[OffspringPlant].proportionOf(colour.prefVar))+"%"

    canvas.beginText
    canvas.setFontAndSize(font, 12)
    canvas.showTextAligned(Element.ALIGN_LEFT, title, 0, canvas.getHeight-10, 0)
    canvas.endText()

    val cWidth = canvas.getWidth / numChromasomes

    plant.chromosomes.zipWithIndex.foreach {
      case (chromasome, index) => {
        val xTrans = (index * cWidth).asInstanceOf[Int]
        var template = canvas.createTemplate(cWidth, canvas.getHeight)
        template = ChromosomeView(chromasome, colour, template)
        canvas.addTemplate(template, xTrans, 0)
      }
    }

    canvas
  }
}
