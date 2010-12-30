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

import processing.core.{PApplet, PFont}
import org.tearne.beaner.chroma._

class ChromasomeView(chromasome:Chromosome, colour: Colour, pApplet:PApplet) {
  private val left = new ChromatidView(chromasome.firstChromatid, true, colour, pApplet)
  private val right = new ChromatidView(chromasome.secondChromatid, false, colour, pApplet)
  private val f:PFont = pApplet.createFont("GillSans-Bold", 10);
  
  def display(){
    pApplet.textFont(f,8)
    pApplet.textAlign(processing.core.PConstants.CENTER)
    
    val percent = chromasome.proportionOf(colour.prefVar)*100
    pApplet.fill(0)
    pApplet.text("%.1f".format(percent)+"%",10, -6)

    left.display
    right.display
  }
}
