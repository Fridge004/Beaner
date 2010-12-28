package org.tearne.beaner.report

import processing.core.{PApplet,PFont}
import org.tearne.beaner.chroma._
import org.tearne.beaner.plant._

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
        pApplet.rect(0, cMHeight*index, 10, cMHeight)
   }}
  }
}
