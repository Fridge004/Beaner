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

import com.itextpdf.text.BaseColor
import org.tearne.beaner.plant._
import org.tearne.beaner.cross._
import org.tearne.beaner.chroma.Centimorgan

class Colour(criteria: Set[Criterion], val prefVar: Plant){
  val plantsMap: Map[Plant, BaseColor] = {
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

  def apply(cM: Centimorgan): BaseColor = {
    var r,g,b = 0.0
    cM.alleles.foreach{
      case(plant, prob) => {
        val c = plantsMap(plant)
        r += c.getRed * prob
        g += c.getGreen * prob
        b += c.getBlue * prob
      }
    }

    new BaseColor(r.asInstanceOf[Int],g.asInstanceOf[Int],b.asInstanceOf[Int])
  }
}
object Colour {
  val prefVar = new BaseColor(255,255,255)

  val donorColours = Array(
    new BaseColor(161,218,0),
    new BaseColor(4,78,226),
    new BaseColor(213,59,129),
    new BaseColor(233,228,28),
    new BaseColor(253,199,3),
    new BaseColor(0,51,102),
    new BaseColor(153,51, 102),
    new BaseColor(0,204,153),
    new BaseColor(255,124,128),
    new BaseColor(228,33,243))
}
