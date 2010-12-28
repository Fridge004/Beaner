package org.tearne.beaner.report

import org.tearne.beaner.plant._
import java.awt.event._
import processing.pdf._
import processing.core._
import com.lowagie.text.PageSize

class PlantPrinter(plants: List[Plant], colour: Colour) extends processing.core.PApplet {
  def this(plant: Plant, colour: Colour) = this(List(plant), colour)
  val margin = 0.05
  val headerSpace = 20
  private val f:PFont = createFont("GillSans-Bold", 10);

  def makePdf(){
    this.init
  }
  
  override def setup() {
    val page = PageSize.A5
    size(page.getWidth.asInstanceOf[Int], page.getHeight().asInstanceOf[Int], processing.core.PConstants.PDF, "output.pdf")
    background(255)

    stroke(0);
    strokeWeight(0.1f);

    val pGraphics = g.asInstanceOf[PGraphicsPDF]
   
    val plantIterator = plants.iterator
    while(plantIterator.hasNext){
      printPlant(plantIterator.next)
      if(plantIterator.hasNext)
	      pGraphics.nextPage
    }
  }

  private def printPlant(plant:Plant){
    val numChromasomes = plant.chromasomes.size

    val widthWithoutMargin = (width*(1-margin)).asInstanceOf[Int]
    val marginWidth = width - widthWithoutMargin
    val marginHeight = (height * margin /2.0).asInstanceOf[Int]

    //pushMatrix
    //translate(0,marginHeight)
    textFont(f,12)
    fill(0)
    textAlign(processing.core.PConstants.LEFT)
    text(plant.friendlyName,10f,10f)
    if(plant.isInstanceOf[OffspringPlant])
      text("%.1f".format(100*plant.asInstanceOf[OffspringPlant].proportionOf(colour.prefVar))+"%",10f,20f)
    translate(0,headerSpace)
    //text

    plant.chromasomes.zipWithIndex.foreach {
      case (chromasome, index) => {
        pushMatrix
        val xTrans = (marginWidth/2.0+(widthWithoutMargin*index) / numChromasomes.asInstanceOf[Double]).asInstanceOf[Int]
        translate(xTrans, headerSpace)
        new ChromasomeView(chromasome, colour, this).display
        popMatrix
      }
    }

    //popMatrix
  }
}

object PlantPrinter {
  //For testing
  def main(args: Array[String]) {
    import org.tearne.beaner.plant._
    import org.tearne.beaner.cross._
    import org.tearne.beaner.chroma._

    //
    // Setup
    //
    PlantPair.setPlantCrosser(new PlantCrosser(new ChromosomeCrosser()))
    ParentPlant.setPlantType(PlantSpec.phaseolusVulgaris)

    // Plants
    val parent1 = new ParentPlant()
    val parent2 = new ParentPlant()
    val prefVar = new ParentPlant()

    // Criteria
    val marker1 = new Criterion(parent1, 0, 9)
    val marker2 = new Criterion(parent2, 1, 39)
    val criteria = marker1 + marker2

    //
    // Do crossings
    //
    //Heterozygous selection
    var f1 = parent1 x parent2 selectHet criteria
    var bc1 = f1 x prefVar selectHet criteria
    var bc2 = bc1 x prefVar selectHet criteria
    var bc3 = bc2 x prefVar selectHet criteria
    var bc4 = bc3 x prefVar selectHet criteria
    var bc5 = bc4 x prefVar selectHet criteria
    var bc6 = bc5 x prefVar selectHet criteria

    //Homozygous selection
    var fin = bc6 x bc6 selectHom criteria

    //Make colour object
    val colour = new Colour(criteria, prefVar)

    new PlantPrinter(List(prefVar, parent1, parent2, f1, bc1, bc2, bc3, bc4, bc5, bc6, fin), colour).makePdf()
  }
}
