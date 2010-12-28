package org.tearne.beaner.report

import processing.core._
import org.tearne.beaner.plant._
import org.tearne.beaner.cross._
import collection._
import collection.immutable.HashMap
import org.tearne.beaner.chroma.Centimorgan

class Colour(criteria: mutable.Set[Criterion], val prefVar: Plant){
  val plantsMap: HashMap[Plant, Int] = {
    var map = new HashMap[Plant, Int]
    map += (prefVar -> Colour.prefVar)

    val plants = new mutable.HashSet[Plant]
    criteria.foreach{criterion => plants + criterion.plant}

    //TODO Do this a functional way
    var counter = 0;
    for(criterion <- criteria){
      if(criterion.plant != prefVar && !map.contains(criterion.plant)){
        map += (criterion.plant -> Colour.donorColours(counter))
        counter += 1
      }
    }

    map
   }

  def apply(plant: Plant) = {
    plantsMap(plant)
  }

  def apply(cM: Centimorgan) = {
    def red(c: Int) = c >> 16 & 0xFF
    def green(c: Int) = c >> 8 & 0xFF
    def blue(c: Int) = c & 0xFF

    var r,g,b = 0.0
    cM.alleles.foreach{
      case(plant, prob) => {
        val c = plantsMap(plant)
        r += red(c)*prob
        g += green(c)*prob
        b += blue(c)*prob
      }
    }

    new PApplet().color(r.asInstanceOf[Int],g.asInstanceOf[Int],b.asInstanceOf[Int])
  }
}
object Colour {
  val p = new PApplet()
  val prefVar = p.color(127,127,127)

  val donorColours = Array(
    p.color(161,218,0),
    p.color(4,78,226),
    p.color(213,59,129),
    p.color(233,228,28),
    p.color(253,199,3),
    p.color(0,51,102),
    p.color(153,51, 102),
    p.color(0,204,153),
    p.color(255,124,128),
    p.color(228,33,243))
}
