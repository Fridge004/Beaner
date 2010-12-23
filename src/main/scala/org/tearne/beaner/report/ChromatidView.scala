package org.tearne.beaner.report

import processing.core.PApplet
import org.tearne.beaner.chroma._
import org.tearne.beaner.plant._

class ChromatidView(chromatid:Chromatid, xPos:Int, yPos:Int, isLeft:Boolean, plant:Plant, pApplet:PApplet) {
  private val length = chromatid.size 

  def display(){
    pApplet.pushMatrix
    if(isLeft){
      pApplet.fill(pApplet.color(255,110,110))
      pApplet.translate(-5,0)
    }
    else{
      pApplet.fill(pApplet.color(110,110,255))
      pApplet.translate(5,0)
    } 

    drawChromatids
    
    pApplet.popMatrix
  }
  
  def drawChromatids {
    chromatid.cMArray.zipWithIndex.foreach{
      case (cM,index) => {
	pApplet.fill(getColour(cM))
        pApplet.rect(xPos, yPos+3*index, 10, 3)
    }}
  }

  private def getColour(cM:Centimorgan):Int={
    val p = cM.probabilityOf(plant)
    val r:Int = (p*255).asInstanceOf[Int]
    val b:Int = 0.0.asInstanceOf[Int]
    val g:Int = ((1-p)*255).asInstanceOf[Int]
    pApplet.color(r,g,b)
  }
}
