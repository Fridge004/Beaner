package org.tearne.beaner.report

import processing.core._
import org.tearne.beaner.plant._
import org.tearne.beaner.cross._
import org.tearne.beaner.chroma.Centimorgan

class Colour(criteria: Set[Criterion], val prefVar: Plant){
  val plantsMap: Map[Plant, Int] = {
    val donorsExcludingPrefVar = criteria.toList.map(_.plant).filter(_!=prefVar).distinct

    if(donorsExcludingPrefVar.size > Colour.donorColours.size)
      throw new ColourException("There are "+donorsExcludingPrefVar.size+" plants and only "+Colour.donorColours.size+" colours available")

    donorsExcludingPrefVar
      .zip(Colour.donorColours)
      .toMap
      .+(prefVar -> Colour.prefVar)
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
