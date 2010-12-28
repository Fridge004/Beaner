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
import org.junit.Assert._
import org.mockito.Mockito._
import org.scalatest.junit.JUnitSuite
import org.scalatest.mock.MockitoSugar

import org.tearne.beaner.chroma._
import org.tearne.beaner.plant._
import org.tearne.beaner.cross._
import org.junit.{Before, Test}

class SysTest extends JUnitSuite with MockitoSugar {
  val tolerance = 1e-16
  val lowTolerance = 1e-4

  @Before def setup {
    PlantPair.setPlantCrosser(new PlantCrosser(new ChromosomeCrosser()))
  }

  @Test def megansSecondExcelSheetTest_4Markers {
    //TODO fix spec
    val spec = mock[PlantSpec]
    when(spec.chromasomeLengths).thenReturn(Array[Int](5, 6, 3, 7, 5))
    ParentPlant.setPlantType(spec)

    val p1 = new ParentPlant()
    val p2 = new ParentPlant()
    val p3 = new ParentPlant()
    val p4 = new ParentPlant()
    val pV = new ParentPlant()

    val c1 = new Criterion(p1, 0, 1)
    val c2 = new Criterion(p2, 1, 5)
    val c3 = new Criterion(p3, 2, 0)
    val c4 = new Criterion(p4, 3, 3)
    val cAll = c1 + c2 + c3 + c4

    val f1_p1p2 = p1 x p2 selectHet c1 + c2
    val f1_p3p4 = p3 x p4 selectHet c3 + c4
    val f1_p1p2p3p4 = f1_p1p2 x f1_p3p4 selectHet cAll

    val bc1 = f1_p1p2p3p4 x pV selectHet cAll
    val bc2 = bc1 x pV selectHet cAll
    val bc3 = bc2 x pV selectHet cAll
    val bc4 = bc3 x pV selectHet cAll

    val fin = bc4 x bc4 selectHom cAll

    assertEquals(0.0, f1_p1p2.proportionOf(pV), lowTolerance)
    assertEquals(0.0, f1_p3p4.proportionOf(pV), lowTolerance)
    assertEquals(0.0, f1_p1p2p3p4.proportionOf(pV), lowTolerance)

    assert(1.0 === f1_p1p2p3p4.chromasomes(0).firstChromatid(1).probabilityOf(p1))

    assertEquals(0.5, bc1.chromasomes(0)proportionOf(pV), lowTolerance)
    assertEquals(0.5, bc1.proportionOf(pV), lowTolerance)

    assertEquals(0.506852, bc2.chromasomes(0)proportionOf(pV), lowTolerance)
    assertEquals(0.508375, bc2.chromasomes(3)proportionOf(pV), lowTolerance)
    assertEquals(0.554999, bc2.proportionOf(pV), lowTolerance)

    assertEquals(0.516560, bc3.chromasomes(3)proportionOf(pV), lowTolerance)
    assertEquals(0.585773, bc3.proportionOf(pV), lowTolerance)

    assertEquals(0.520132, bc4.chromasomes(0)proportionOf(pV), lowTolerance)
    assertEquals(0.534910, bc4.chromasomes(1)proportionOf(pV), lowTolerance)
    assertEquals(0.514513, bc4.chromasomes(2)proportionOf(pV), lowTolerance)
    assertEquals(0.524559, bc4.chromasomes(3)proportionOf(pV), lowTolerance)
    assertEquals(0.937500, bc4.chromasomes(4)proportionOf(pV), lowTolerance)
    assertEquals(0.604349, bc4.proportionOf(pV), lowTolerance)

    assertEquals(0.053133, fin.chromasomes(0)proportionOf(pV), lowTolerance)
    assertEquals(0.091490, fin.chromasomes(1)proportionOf(pV), lowTolerance)
    assertEquals(0.038388, fin.chromasomes(2)proportionOf(pV), lowTolerance)
    assertEquals(0.064756, fin.chromasomes(3)proportionOf(pV), lowTolerance)
    assertEquals(0.937500, fin.chromasomes(4)proportionOf(pV), lowTolerance)
    assertEquals(0.233483, fin.proportionOf(pV), lowTolerance)
  }

  @Test def setupForMegansFirstExcelSheetTest {
    val spec = mock[PlantSpec]; when(spec.chromasomeLengths).thenReturn(Array[Int](50, 70, 90))
    ParentPlant.setPlantType(spec)

    val p1 = new ParentPlant()
    val p2 = new ParentPlant()
    val pV = new ParentPlant()

    val crit1 = new Criterion(p1, 0, 9)
    val crit2 = new Criterion(p2, 1, 39)

    PlantPair.setPlantCrosser(new PlantCrosser(new ChromosomeCrosser()))

    //Heterozygous selection
    val f1 = p1 x p2 selectHet crit1 + crit1
    val bc1 = f1 x pV selectHet crit1 + crit2
    val bc2 = bc1 x pV selectHet crit1 + crit2

    //Homozygous selection
    val fin = bc2 x bc2 selectHom crit1 + crit2

    assertEquals(1.0, f1.selectionProbability.get, tolerance)
    assertEquals(0.0, f1.chromasomes(2).proportionOf(pV), tolerance)

    assertEquals(0.25, bc1.selectionProbability.get, tolerance)
    assertEquals(0.5, bc1.chromasomes(2).proportionOf(pV), tolerance)

    assertEquals(0.25, bc2.selectionProbability.get, tolerance)
    assertEquals(0.75, bc2.chromasomes(2).proportionOf(pV), tolerance)

    assertEquals(1.0 / 16.0, fin.selectionProbability.get, tolerance)
    assertEquals(0.75, fin.chromasomes(2).proportionOf(pV), tolerance)

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
