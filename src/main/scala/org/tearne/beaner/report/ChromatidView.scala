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

class ChromatidView(chromatid:Chromatid, isLeft:Boolean, colour:Colour, pApplet:PApplet) {
  private val length = chromatid.size
  val cMHeight = 3

  def display(){
    pApplet.pushMatrix
    if(!isLeft){
      pApplet.translate(10,0)
    } 
    drawChromatids()
    
    pApplet.popMatrix
  }
  
  private def drawChromatids() {
    chromatid.cMArray.zipWithIndex.foreach{
      case (cM,index) => {
	      pApplet.fill(colour(cM))
        pApplet.stroke(150,150,150)
        pApplet.rect(0, cMHeight*index, 10, cMHeight)
   }}
  }
}
