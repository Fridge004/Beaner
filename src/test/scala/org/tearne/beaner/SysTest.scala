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

import org.tearne.beaner._
import plant._
import chroma._
import criteria._
import cross._
import model._

import org.junit.{Before, Test}

class SysTest extends JUnitSuite with MockitoSugar {
  val tolerance = 1e-16
  val lowTolerance = 1e-4
  val plantCrosser = new PlantCrosser(new ChromosomeCrosser(new Gameter(new HaldaneRecombinationModel)))

  /**
   * verifying against previous figures (not analytically verified)
   */
  @Test def doubleSelectionTest{
    object ThreeChromoPlant extends PlantSpec{
      val chromosomeLengths = Array(100,100,100)
    }
    //Todo takes v long time to work out combinations
    val p1 = ThreeChromoPlant("donor1")
    val p2 = ThreeChromoPlant("donor2")
    val pV = ThreeChromoPlant("prefVar")
    
    val c1_1 = new Criterion(p1, 0, 10)	//Gene1
    val c1_2 = new Criterion(p1, 0, 15) //Marker for Gene1
    val c2_1 = new Criterion(p2, 1, 30)	//Gene2
    val c2_2 = new Criterion(p2, 1, 40)	//Marker for Gene2
    
    val cAll = c1_1 + c1_2 + c2_1 + c2_2
    
    val f1_p1p2 = p1 x p2 selectHet cAll named "f1_p1p2"
    val complex = f1_p1p2 x pV selectHet cAll named "complex"

    val bc1 = complex x pV selectHet cAll named "bc1"
    val bc2 = bc1 x pV selectHet cAll named "bc2"
    val bc3 = bc2 x pV selectHet cAll named "bc3"
    val bc4 = bc3 x pV selectHet cAll named "bc4"

    val fin = bc4 x bc4 selectHom cAll named "fin"

    fail("Forcing test to fail since too slow at present")
    
    assertEquals( 0.62147, bc1.evaluateUsing(plantCrosser).chromosomes(0).proportionOf(pV), lowTolerance )
    assertEquals( 0.59658,  bc1.evaluateUsing(plantCrosser).chromosomes(1).proportionOf(pV), lowTolerance )
    assert( 0.75 === bc1.evaluateUsing(plantCrosser).chromosomes(2).proportionOf(pV) )
    
    assertEquals( 0.66748, fin.evaluateUsing(plantCrosser).chromosomes(0).proportionOf(pV), lowTolerance)
    assertEquals( 0.58603, fin.evaluateUsing(plantCrosser).chromosomes(1).proportionOf(pV), lowTolerance)
    assertEquals( 0.96875, fin.evaluateUsing(plantCrosser).chromosomes(2).proportionOf(pV), lowTolerance)
    
  }
  
  @Test def megansSecondExcelSheetTest_4Markers {
    object FiveChromoPlant extends PlantSpec{
      val chromosomeLengths = Array(5, 6, 3, 7, 5)
    }
    val p1 = FiveChromoPlant("donor1")
    val p2 = FiveChromoPlant("donor2")
    val p3 = FiveChromoPlant("donor3")
    val p4 = FiveChromoPlant("donor4")
    val pV = FiveChromoPlant("donor5")

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

    assertEquals(0.5, bc1.evaluateUsing(plantCrosser).chromosomes(0).proportionOf(pV), lowTolerance)
    assertEquals(0.5, bc1.evaluateUsing(plantCrosser).proportionOf(pV), lowTolerance)

    assertEquals(0.506852, bc2.evaluateUsing(plantCrosser).chromosomes(0).proportionOf(pV), lowTolerance)
    assertEquals(0.508375, bc2.evaluateUsing(plantCrosser).chromosomes(3).proportionOf(pV), lowTolerance)
    assertEquals(0.554999, bc2.evaluateUsing(plantCrosser).proportionOf(pV), lowTolerance)

    assertEquals(0.516560, bc3.evaluateUsing(plantCrosser).chromosomes(3).proportionOf(pV), lowTolerance)
    assertEquals(0.585773, bc3.evaluateUsing(plantCrosser).proportionOf(pV), lowTolerance)

    assertEquals(0.520132, bc4.evaluateUsing(plantCrosser).chromosomes(0).proportionOf(pV), lowTolerance)
    assertEquals(0.534910, bc4.evaluateUsing(plantCrosser).chromosomes(1).proportionOf(pV), lowTolerance)
    assertEquals(0.514513, bc4.evaluateUsing(plantCrosser).chromosomes(2).proportionOf(pV), lowTolerance)
    assertEquals(0.524559, bc4.evaluateUsing(plantCrosser).chromosomes(3).proportionOf(pV), lowTolerance)
    assertEquals(0.937500, bc4.evaluateUsing(plantCrosser).chromosomes(4).proportionOf(pV), lowTolerance)
    assertEquals(0.604349, bc4.evaluateUsing(plantCrosser).proportionOf(pV), lowTolerance)

    assertEquals(0.053133, fin.evaluateUsing(plantCrosser).chromosomes(0).proportionOf(pV), lowTolerance)
    assertEquals(0.091490, fin.evaluateUsing(plantCrosser).chromosomes(1).proportionOf(pV), lowTolerance)
    assertEquals(0.038388, fin.evaluateUsing(plantCrosser).chromosomes(2).proportionOf(pV), lowTolerance)
    assertEquals(0.064756, fin.evaluateUsing(plantCrosser).chromosomes(3).proportionOf(pV), lowTolerance)
    assertEquals(0.937500, fin.evaluateUsing(plantCrosser).chromosomes(4).proportionOf(pV), lowTolerance)
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
    val f1 = p1 x p2 selectHet crit1 + crit2 named "f1"
    val bc1 = f1 x pV selectHet crit1 + crit2 named "bc1"
    val bc2 = bc1 x pV selectHet crit1 + crit2  named "bc2"

    //Homozygous selection
    val fin = bc2 x bc2 selectHom crit1 + crit2 named "final"

    val f1Result = f1.evaluateUsing(plantCrosser)
    assertEquals(1.0, f1Result.selectionProbability.get, tolerance)
    assertEquals(0.0, f1Result.chromosomes(2).proportionOf(pV), tolerance)

    val bc1Result = bc1.evaluateUsing(plantCrosser)
    assertEquals(0.25, bc1Result.selectionProbability.get, tolerance)
    assertEquals(0.5, bc1Result.chromosomes(2).proportionOf(pV), tolerance)

    val bc2Result = bc2.evaluateUsing(plantCrosser)
    assertEquals(0.25, bc2Result.selectionProbability.get, tolerance)
    assertEquals(0.75, bc2Result.chromosomes(2).proportionOf(pV), tolerance)

    val finResult = fin.evaluateUsing(plantCrosser)
    assertEquals(1.0 / 16.0, finResult.selectionProbability.get, tolerance)
    assertEquals(0.75, finResult.chromosomes(2).proportionOf(pV), tolerance)

    assertEquals(0.772698, finResult.chromosomes(0).firstChromatid.probabilityOf(p1, 0), lowTolerance)
    assertEquals(0.794207, finResult.chromosomes(0).firstChromatid.probabilityOf(p1, 1), lowTolerance)
    assertEquals(0.816559, finResult.chromosomes(0).firstChromatid.probabilityOf(p1, 2), lowTolerance)
    assertEquals(0.839790, finResult.chromosomes(0).firstChromatid.probabilityOf(p1, 3), lowTolerance)
    assertEquals(0.863940, finResult.chromosomes(0).firstChromatid.probabilityOf(p1, 4), lowTolerance)
    assertEquals(0.889051, finResult.chromosomes(0).firstChromatid.probabilityOf(p1, 5), lowTolerance)
    assertEquals(0.915165, finResult.chromosomes(0).firstChromatid.probabilityOf(p1, 6), lowTolerance)
    assertEquals(0.942329, finResult.chromosomes(0).firstChromatid.probabilityOf(p1, 7), lowTolerance)
    assertEquals(0.970591, finResult.chromosomes(0).firstChromatid.probabilityOf(p1, 8), lowTolerance)
    assertEquals(1.000000, finResult.chromosomes(0).firstChromatid.probabilityOf(p1, 9), lowTolerance)
    assertEquals(0.970591, finResult.chromosomes(0).firstChromatid.probabilityOf(p1, 10), lowTolerance)
    assertEquals(0.942329, finResult.chromosomes(0).firstChromatid.probabilityOf(p1, 11), lowTolerance)
    assertEquals(0.915165, finResult.chromosomes(0).firstChromatid.probabilityOf(p1, 12), lowTolerance)
    assertEquals(0.889051, finResult.chromosomes(0).firstChromatid.probabilityOf(p1, 13), lowTolerance)
    assertEquals(0.863940, finResult.chromosomes(0).firstChromatid.probabilityOf(p1, 14), lowTolerance)
    assertEquals(0.839790, finResult.chromosomes(0).firstChromatid.probabilityOf(p1, 15), lowTolerance)
    assertEquals(0.816559, finResult.chromosomes(0).firstChromatid.probabilityOf(p1, 16), lowTolerance)
    assertEquals(0.794207, finResult.chromosomes(0).firstChromatid.probabilityOf(p1, 17), lowTolerance)
    assertEquals(0.772698, finResult.chromosomes(0).firstChromatid.probabilityOf(p1, 18), lowTolerance)
  }
}
