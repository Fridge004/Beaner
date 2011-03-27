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
import com.itextpdf.text._
import com.itextpdf.text.pdf._

object ChromosomeView {

  val helvetica: Font = new Font(Font.FontFamily.HELVETICA, 12)
  val font: BaseFont = helvetica.getCalculatedBaseFont(false)

  def apply(chromosome:Chromosome, colour: Colour, canvas:PdfTemplate):PdfTemplate = {
    val percent = chromosome.proportionOf(colour.prefVar)*100

    canvas.beginText
    canvas.setFontAndSize(font, 8)
    canvas.showTextAligned(
        Element.ALIGN_LEFT,
        "%.1f".format(percent)+"%",
        0,
        canvas.getHeight-20,
        0
    )
    canvas.endText

    val halfWidth = canvas.getWidth/2
    var leftTemplate = canvas.createTemplate(halfWidth-5, canvas.getHeight-25)
    var rightTemplate = canvas.createTemplate(halfWidth-5, canvas.getHeight-25)

    canvas.addTemplate(ChromatidView(chromosome.firstChromatid, colour, leftTemplate), 0, 0)
    canvas.addTemplate(ChromatidView(chromosome.firstChromatid, colour, rightTemplate), halfWidth-5, 0)

    canvas
  }
}