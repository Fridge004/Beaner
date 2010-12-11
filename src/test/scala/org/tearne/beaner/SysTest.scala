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

package org.tearne.beaner
import org.junit.{Test, Before}
import org.junit.Assert._
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.Assertions._
import org.scalatest.junit.JUnitSuite
import org.scalatest.mock.MockitoSugar

import org.tearne.beaner.chroma._
import org.tearne.beaner.plant._
import org.tearne.beaner.cross._

class SysTest extends JUnitSuite with MockitoSugar {
  val tolerance = 1e-16
  val lowTolerance = 1e-4

  val spec = mock[PlantSpec]; when(spec.chromasomeLengths).thenReturn(Array[Int](50, 70, 90))

  ParentPlant.setPlantType(spec)
  val p1 = new ParentPlant()
  val p2 = new ParentPlant()
  val pV = new ParentPlant()
  var f1, bc1, bc2, fin: OffspringPlant = null
  val crit1 = new Criterion(p1, 0, 9)
  val crit2 = new Criterion(p2, 1, 39)

  @Before
  def setup {
    PlantPair.setPlantCrosser(new PlantCrosser(new ChromosomeCrosser()))
    
    //Heterozygous selection
    f1 = p1 x p2 selectHet crit1 + crit1
    bc1 = f1 x pV selectHet crit1 + crit2
    bc2 = bc1 x pV selectHet crit1 + crit2

    //Homozygous selection
    fin = bc2 x bc2 selectHom crit1 + crit2
  }

  @Test
  def crossingTest1 {
    assertEquals(1.0, f1.selectionProbability.get, tolerance)
    assertEquals(0.0, f1.chromasomes(2).proportionOf(pV), tolerance)

    assertEquals(0.25, bc1.selectionProbability.get, tolerance)
    assertEquals(0.5, bc1.chromasomes(2).proportionOf(pV), tolerance)

    assertEquals(0.25, bc2.selectionProbability.get, tolerance)
    assertEquals(0.75, bc2.chromasomes(2).proportionOf(pV), tolerance)

    assertEquals(1.0 / 16.0, fin.selectionProbability.get, tolerance)
    assertEquals(0.75, fin.chromasomes(2).proportionOf(pV), tolerance)
  }

  @Test
  def crossingTest2 {
    assertEquals(0.772698, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 0), lowTolerance)
    assertEquals(0.794207, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 1), lowTolerance)
    assertEquals(0.816559, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 2), lowTolerance)
    assertEquals(0.839790, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 3), lowTolerance)
    assertEquals(0.863940, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 4), lowTolerance)
    assertEquals(0.889051, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 5), lowTolerance)
    assertEquals(0.915165, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 6), lowTolerance)
    assertEquals(0.942329, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 7), lowTolerance)
    assertEquals(0.970591, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 8), lowTolerance)
    assertEquals(1.000000, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 9), lowTolerance)
    assertEquals(0.970591, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 10), lowTolerance)
    assertEquals(0.942329, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 11), lowTolerance)
    assertEquals(0.915165, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 12), lowTolerance)
    assertEquals(0.889051, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 13), lowTolerance)
    assertEquals(0.863940, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 14), lowTolerance)
    assertEquals(0.839790, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 15), lowTolerance)
    assertEquals(0.816559, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 16), lowTolerance)
    assertEquals(0.794207, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 17), lowTolerance)
    assertEquals(0.772698, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 18), lowTolerance)
  }
}
