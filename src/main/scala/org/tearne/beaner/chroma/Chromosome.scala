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

package org.tearne.beaner.chroma

import org.tearne.beaner.plant._

class Chromosome(val firstChromatid: Chromatid, val secondChromatid: Chromatid, val selectionProbability: Option[Double]) {

  def this(firstChromatid: Chromatid, secondChromatid: Chromatid, selectionProbability: Double) 
    = this(firstChromatid, secondChromatid, Some(selectionProbability))
  def this(firstChromatid: Chromatid, secondChromatid: Chromatid) = this(firstChromatid, secondChromatid, None)
  def this(plant: Plant, size: Int) = this(new Chromatid(plant, size), new Chromatid(plant, size))

  if (firstChromatid.size != secondChromatid.size)
    throw new ChromosomeException("Chromatids are not of the same size")

  val size = firstChromatid.size


  def proportionOf(plant: Plant): Double = {
    val totalNumCentimorgans: Double = 2 * size

    (firstChromatid.sumProbabilitiesOf(plant) + secondChromatid.sumProbabilitiesOf(plant)) / totalNumCentimorgans
  }
}
