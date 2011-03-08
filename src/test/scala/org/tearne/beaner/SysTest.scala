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
  val plantCrosser = new PlantCrosser(new ChromosomeCrosser(new Gameter(new RecombinationModel)))

//  @Before def setup {
//    PlantPair.setPlantCrosser(new PlantCrosser(new ChromosomeCrosser(null)))
//  }

  @Test def megansSecondExcelSheetTest_4Markers {
    object MyPlantSpec extends PlantSpec{
      val chromosomeLengths = Array(5, 6, 3, 7, 5)
    }
    val p1 = MyPlantSpec("donor1")
    val p2 = MyPlantSpec("donor2")
    val p3 = MyPlantSpec("donor3")
    val p4 = MyPlantSpec("donor4")
    val pV = MyPlantSpec("donor5")

    val c1 = new Criterion(p1, 0, 1)
    val c2 = new Criterion(p2, 1, 5)
    val c3 = new Criterion(p3, 2, 0)
    val c4 = new Criterion(p4, 3, 3)
    val cAll = c1 + c2 + c3 + c4

    val f1_p1p2 = p1 x p2 selectHet c1 + c2 named "f1_p1p2"
    val f1_p3p4 = p3 x p4 selectHet c3 + c4 named "f1_p3p4"
    val f1_p1p2p3p4 = f1_p1p2 x f1_p3p4 selectHet cAll named "f1_p1p2p3p4"

    val bc1 = f1_p1p2p3p4 x pV selectHet cAll named "bc1"
    val bc2 = bc1 x pV selectHet cAll named "bc2"
    val bc3 = bc2 x pV selectHet cAll named "bc3"
    val bc4 = bc3 x pV selectHet cAll named "bc4"

    val fin = bc4 x bc4 selectHom cAll named "fin"

    assertEquals(0.0, f1_p1p2.evaluateUsing(plantCrosser).proportionOf(pV), lowTolerance)
    assertEquals(0.0, f1_p3p4.evaluateUsing(plantCrosser).proportionOf(pV), lowTolerance)
    assertEquals(0.0, f1_p1p2p3p4.evaluateUsing(plantCrosser).proportionOf(pV), lowTolerance)

    assert(1.0 === f1_p1p2p3p4.evaluateUsing(plantCrosser).chromosomes(0).firstChromatid(1).probabilityOf(p1))

    assertEquals(0.5, bc1.evaluateUsing(plantCrosser).chromosomes(0)proportionOf(pV), lowTolerance)
    assertEquals(0.5, bc1.evaluateUsing(plantCrosser).proportionOf(pV), lowTolerance)

    assertEquals(0.506852, bc2.evaluateUsing(plantCrosser).chromosomes(0)proportionOf(pV), lowTolerance)
    assertEquals(0.508375, bc2.evaluateUsing(plantCrosser).chromosomes(3)proportionOf(pV), lowTolerance)
    assertEquals(0.554999, bc2.evaluateUsing(plantCrosser).proportionOf(pV), lowTolerance)

    assertEquals(0.516560, bc3.evaluateUsing(plantCrosser).chromosomes(3)proportionOf(pV), lowTolerance)
    assertEquals(0.585773, bc3.evaluateUsing(plantCrosser).proportionOf(pV), lowTolerance)

    assertEquals(0.520132, bc4.evaluateUsing(plantCrosser).chromosomes(0)proportionOf(pV), lowTolerance)
    assertEquals(0.534910, bc4.evaluateUsing(plantCrosser).chromosomes(1)proportionOf(pV), lowTolerance)
    assertEquals(0.514513, bc4.evaluateUsing(plantCrosser).chromosomes(2)proportionOf(pV), lowTolerance)
    assertEquals(0.524559, bc4.evaluateUsing(plantCrosser).chromosomes(3)proportionOf(pV), lowTolerance)
    assertEquals(0.937500, bc4.evaluateUsing(plantCrosser).chromosomes(4)proportionOf(pV), lowTolerance)
    assertEquals(0.604349, bc4.evaluateUsing(plantCrosser).proportionOf(pV), lowTolerance)

    assertEquals(0.053133, fin.evaluateUsing(plantCrosser).chromosomes(0)proportionOf(pV), lowTolerance)
    assertEquals(0.091490, fin.evaluateUsing(plantCrosser).chromosomes(1)proportionOf(pV), lowTolerance)
    assertEquals(0.038388, fin.evaluateUsing(plantCrosser).chromosomes(2)proportionOf(pV), lowTolerance)
    assertEquals(0.064756, fin.evaluateUsing(plantCrosser).chromosomes(3)proportionOf(pV), lowTolerance)
    assertEquals(0.937500, fin.evaluateUsing(plantCrosser).chromosomes(4)proportionOf(pV), lowTolerance)
    assertEquals(0.233483, fin.evaluateUsing(plantCrosser).proportionOf(pV), lowTolerance)
  }

  @Test def setupForMegansFirstExcelSheetTest {
    object MyPlantSpec extends PlantSpec{
      val chromosomeLengths = Array[Int](50, 70, 90)
    }

    val p1 = MyPlantSpec("p1")
    val p2 = MyPlantSpec("p2")
    val pV = MyPlantSpec("p3")

    val crit1 = new Criterion(p1, 0, 9)
    val crit2 = new Criterion(p2, 1, 39)

    //Heterozygous selection
    val f1 = p1 x p2 selectHet crit1 + crit1 named "f1"
    val bc1 = f1 x pV selectHet crit1 + crit2 named "bc1"
    val bc2 = bc1 x pV selectHet crit1 + crit2  named "bc2"

    //Homozygous selection
    val fin = bc2 x bc2 selectHom crit1 + crit2 named "final"

    assertEquals(1.0, f1.evaluateUsing(plantCrosser).selectionProbability.get, tolerance)
    assertEquals(0.0, f1.evaluateUsing(plantCrosser).chromosomes(2).proportionOf(pV), tolerance)

    assertEquals(0.25, bc1.evaluateUsing(plantCrosser).selectionProbability.get, tolerance)
    assertEquals(0.5, bc1.evaluateUsing(plantCrosser).chromosomes(2).proportionOf(pV), tolerance)

    assertEquals(0.25, bc2.evaluateUsing(plantCrosser).selectionProbability.get, tolerance)
    assertEquals(0.75, bc2.evaluateUsing(plantCrosser).chromosomes(2).proportionOf(pV), tolerance)

    assertEquals(1.0 / 16.0, fin.evaluateUsing(plantCrosser).selectionProbability.get, tolerance)
    assertEquals(0.75, fin.evaluateUsing(plantCrosser).chromosomes(2).proportionOf(pV), tolerance)

    assertEquals(0.772698, fin.evaluateUsing(plantCrosser).chromosomes(0).firstChromatid.probabilityOf(p1, 0), lowTolerance)
    assertEquals(0.794207, fin.evaluateUsing(plantCrosser).chromosomes(0).firstChromatid.probabilityOf(p1, 1), lowTolerance)
    assertEquals(0.816559, fin.evaluateUsing(plantCrosser).chromosomes(0).firstChromatid.probabilityOf(p1, 2), lowTolerance)
    assertEquals(0.839790, fin.evaluateUsing(plantCrosser).chromosomes(0).firstChromatid.probabilityOf(p1, 3), lowTolerance)
    assertEquals(0.863940, fin.evaluateUsing(plantCrosser).chromosomes(0).firstChromatid.probabilityOf(p1, 4), lowTolerance)
    assertEquals(0.889051, fin.evaluateUsing(plantCrosser).chromosomes(0).firstChromatid.probabilityOf(p1, 5), lowTolerance)
    assertEquals(0.915165, fin.evaluateUsing(plantCrosser).chromosomes(0).firstChromatid.probabilityOf(p1, 6), lowTolerance)
    assertEquals(0.942329, fin.evaluateUsing(plantCrosser).chromosomes(0).firstChromatid.probabilityOf(p1, 7), lowTolerance)
    assertEquals(0.970591, fin.evaluateUsing(plantCrosser).chromosomes(0).firstChromatid.probabilityOf(p1, 8), lowTolerance)
    assertEquals(1.000000, fin.evaluateUsing(plantCrosser).chromosomes(0).firstChromatid.probabilityOf(p1, 9), lowTolerance)
    assertEquals(0.970591, fin.evaluateUsing(plantCrosser).chromosomes(0).firstChromatid.probabilityOf(p1, 10), lowTolerance)
    assertEquals(0.942329, fin.evaluateUsing(plantCrosser).chromosomes(0).firstChromatid.probabilityOf(p1, 11), lowTolerance)
    assertEquals(0.915165, fin.evaluateUsing(plantCrosser).chromosomes(0).firstChromatid.probabilityOf(p1, 12), lowTolerance)
    assertEquals(0.889051, fin.evaluateUsing(plantCrosser).chromosomes(0).firstChromatid.probabilityOf(p1, 13), lowTolerance)
    assertEquals(0.863940, fin.evaluateUsing(plantCrosser).chromosomes(0).firstChromatid.probabilityOf(p1, 14), lowTolerance)
    assertEquals(0.839790, fin.evaluateUsing(plantCrosser).chromosomes(0).firstChromatid.probabilityOf(p1, 15), lowTolerance)
    assertEquals(0.816559, fin.evaluateUsing(plantCrosser).chromosomes(0).firstChromatid.probabilityOf(p1, 16), lowTolerance)
    assertEquals(0.794207, fin.evaluateUsing(plantCrosser).chromosomes(0).firstChromatid.probabilityOf(p1, 17), lowTolerance)
    assertEquals(0.772698, fin.evaluateUsing(plantCrosser).chromosomes(0).firstChromatid.probabilityOf(p1, 18), lowTolerance)
  }
}
