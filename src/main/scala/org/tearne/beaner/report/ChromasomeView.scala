package org.tearne.beaner.report

import processing.core.PApplet
import org.tearne.beaner.chroma._
import org.tearne.beaner.plant._

class ChromasomeView(chromasome:Chromosome, position:Double, plant:Plant, canvas:PApplet) {
  private val myX = (position*canvas.width).asInstanceOf[Int]
  
  //println("  Position "+position)
  
  private val left = new ChromatidView(chromasome.firstChromatid, myX, 10, true, plant, canvas)
  private val right = new ChromatidView(chromasome.secondChromatid, myX, 10, false, plant, canvas)
  
  def display(){
    left.display
    right.display
  }
}
