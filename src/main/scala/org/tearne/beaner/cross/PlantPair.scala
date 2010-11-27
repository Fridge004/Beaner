package org.tearne.beaner.cross

import org.tearne.beaner.plant._
import org.tearne.beaner.plant.selection._

class PlantPair(val first:Plant, val second:Plant) { 
  import PlantPair.plantCrosser

  def selectHet(criteria:List[Criteria]):Option[OffspringPlant] = {
      plantCrosser.selectHeterozygousOffspring(this, criteria)
  }
  
  def selectHom(criteria:List[Criteria]):Option[OffspringPlant] = {
      plantCrosser.selectHomozygousOffspring(this, criteria)
  }
}
object PlantPair{
  private var plantCrosser:PlantCrosser = null
  
  def apply(first:Plant, second:Plant) = new PlantPair(first, second)
  def setPlantCrosser(crosser:PlantCrosser) = plantCrosser = crosser
}