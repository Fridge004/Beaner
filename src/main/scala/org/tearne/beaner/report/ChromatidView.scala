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
import org.tearne.beaner.chroma._
import collection.mutable.ListBuffer

class ChromatidView(chromatid:Chromatid, isLeft:Boolean, colour:Colour, pApplet:PApplet) {
  private val length = chromatid.size
  private val cMHeight = 2

  def display(){
    pApplet.pushMatrix
    if(!isLeft){
      pApplet.translate(10,0)
    }

    drawCMColours()
    drawOutline()

    pApplet.popMatrix
  }
  
  private def drawCMColours() {
    val nonTrivial = detectIfNonTrivialChromatid(0)
    var highlights = new ListBuffer[Int]()

    chromatid.cMArray.zipWithIndex.foreach{
      case (cM,index) => {
        val c = colour(cM)
	      pApplet.fill(c)
        pApplet.stroke(c)
        if(nonTrivial && cM.alleles.size == 1)
          highlights += index
        pApplet.rect(0, cMHeight*index, 10, cMHeight)
   }}

    pApplet.strokeWeight(1f)
    highlights.foreach{index =>
      pApplet.noFill
      pApplet.stroke(0)
      pApplet.rect(0, cMHeight*index, 10, cMHeight)
    }
    pApplet.strokeWeight(0.1f)

  }

  private def drawOutline() {
    pApplet.stroke(0,0,0)
    pApplet.noFill()
    pApplet.rect(0, 0, 10, chromatid.size*cMHeight)
  }

  private def detectIfNonTrivialChromatid(startIndex: Int): Boolean = {
    if(chromatid.cMArray(startIndex).alleles.size > 1) true
    else if(startIndex+1==chromatid.cMArray.size ) false
    else detectIfNonTrivialChromatid(startIndex+1)
  }
}
