package org.tearne.beaner.report

import processing.core.{PApplet, PFont}
import org.tearne.beaner.chroma._
import org.tearne.beaner.plant._

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
