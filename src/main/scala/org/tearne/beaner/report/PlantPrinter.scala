package org.tearne.beaner.report

import org.tearne.beaner.plant._
import java.awt.event._
import processing.pdf._
import processing.core._

class PlantPrinter(plant: Plant, prefVar: Plant) extends processing.core.PApplet {
  override def setup() {
    size(400, 300, processing.core.PConstants.PDF, "s.pdf")
    background(255)

    //stroke(0, 20);
    //strokeWeight(20.0f);
    //line(0, 0, width / 2, height);

    val numChromasomes = plant.chromasomes.size

    plant.chromasomes.zipWithIndex.foreach {
      case (chromasome, index) => {
        new ChromasomeView(chromasome, (index + 0.5) / numChromasomes.asInstanceOf[Double], prefVar, this).display
      }
    }
  }

  def makePdf() {
    PApplet.main(Array("org.tearne.beaner.report.PlantPrinter"));
  }
}

object PlantPrinter {
  def apply(p:Plant, pV:Plant) = {
    var frame = new javax.swing.JFrame("Test")
    var applet = new PlantPrinter(p, pV)
    frame.getContentPane().add(applet)
    applet.init
    frame.pack
    frame.setVisible(true)

    frame.addWindowListener(new WindowAdapter() {
      override def windowClosing(e: WindowEvent) {
        applet.destroy()
        frame.dispose()
      }
    })
  }

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
    val aphid = new Criterion(parent1, 0, 9)
    val blight = new Criterion(parent2, 1, 39)
    val criteria = aphid + blight

    //
    // Do crossings
    //
    //Heterozygous selection
    var f1 = parent1 x parent2 selectHet criteria
    var bc1 = f1 x prefVar selectHet criteria
    var bc2 = bc1 x prefVar selectHet criteria

    //Homozygous selection
    var fin = bc2 x bc2 selectHom criteria

    apply(fin, prefVar)
  }
}
