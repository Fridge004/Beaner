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

import org.junit.{ Before, Test }
import org.junit.Assert._
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.Assertions._
import org.scalatest.mock.MockitoSugar
import org.scalatest.junit.JUnitSuite
import org.tearne.beaner.chroma
import scala.math._

import org.tearne.beaner.plant._

class ChromosomeTest extends JUnitSuite with MockitoSugar {
  val tolerance = 1e-16
  val p1 = mock[Plant]
  val p2 = mock[Plant]
  val p3 = mock[Plant]
  val p4 = mock[Plant]

  var tidA, tidB, tidC, tidD: Chromatid = null

  @Before
  def setup {
    tidA = new Chromatid(p1, 100)
    tidB = new Chromatid(p2, 100)
    tidC = new Chromatid(p1, 100)
    tidC(50) = new Centimorgan(Map(p1->0.5,p2->0.5))
    tidD = new Chromatid(p1, 99)
  }

  @Test
  def defaultSelectionProbabilityIsNone {
    assertEquals(None, new Chromosome(tidA, tidB).selectionProbability)
  }

  @Test
  def nonNoneSelectionProbability {
    assertEquals(0.3, new Chromosome(tidA, tidB, 0.3).selectionProbability.get, tolerance)
  }

  @Test
  def size {
    assertEquals(100, new Chromosome(tidA, tidB).size)
    assertEquals(99, new Chromosome(tidD, tidD).size)
  }

  @Test
  def probabilityGameteContains {
    assertEquals(0.5, new Chromosome(tidA, tidB).probabilityGameteContains(p1, 50), tolerance)
    assertEquals(0.75, new Chromosome(tidA, tidC).probabilityGameteContains(p1, 50), tolerance)
    assertEquals(0.5, new Chromosome(tidC, tidC).probabilityGameteContains(p1, 50), tolerance)
    assertEquals(0.25, new Chromosome(tidB, tidC).probabilityGameteContains(p1, 50), tolerance)
  }

  @Test
  def exceptionIfTryToSelectWhenAlleleNotPresentWithProbOne {
    tidA(50) = new Centimorgan(Map(p1->0.5, p2->0.5))

    intercept[ChromasomeException] {
      new Chromosome(tidA, tidB).makeGameteSelectingFor(p1, 50)
    }

    intercept[ChromasomeException] {
      new Chromosome(tidA, tidB).makeGameteSelectingFor(p2, 50)
    }
  }

  @Test
  def exceptionIfChromatidsDiffLengths {
    intercept[ChromasomeException] { new Chromosome(tidA, tidD) }
  }

  @Test
  def gameteSelectingFor_doubleChromatid {
    tidB(50) = new Centimorgan(p1)
    val chromasome = new Chromosome(tidA, tidB)

    val gamete = chromasome.makeGameteSelectingFor(p1, 50)

    assertEquals(1.0, gamete.probabilityOf(p1, 50), tolerance)
    var probP1 = 0.5  //All should be prob = 1/2
    for (index <- 51 until 100) {
      assertEquals("At " + index + ":", probP1, gamete.probabilityOf(p1, index), tolerance)
      assertEquals("At " + index + ":", 1 - probP1, gamete.probabilityOf(p2, index), tolerance)
      assertEquals("At " + index + ":", probP1, gamete.probabilityOf(p1, 100 - index), tolerance)
      assertEquals("At " + index + ":", 1 - probP1, gamete.probabilityOf(p2, 100 - index), tolerance)
    }

    assertEquals("At " + 0 + ":", probP1, gamete.probabilityOf(p1, 0), tolerance)
    assertEquals("At " + 0 + ":", 1 - probP1, gamete.probabilityOf(p2, 0), tolerance)
  }

  @Test
  def gameteSelectingFor_singleChromatid {
    val chromasome = new Chromosome(tidA, tidB)
    val gamete = chromasome.makeGameteSelectingFor(p1, 50)

    assertEquals(1.0, gamete.probabilityOf(p1, 50), tolerance)
    var probP1 = 0.0
    for (index <- 51 until 100) {
      probP1 = 1.0-0.5*(1.0-exp(-2.0*(index-50)/100.0))
      assertEquals("At " + index + ":", probP1, gamete.probabilityOf(p1, index), tolerance)
      assertEquals("At " + index + ":", 1 - probP1, gamete.probabilityOf(p2, index), tolerance)
      assertEquals("At " + index + ":", probP1, gamete.probabilityOf(p1, 100 - index), tolerance)
      assertEquals("At " + index + ":", 1 - probP1, gamete.probabilityOf(p2, 100 - index), tolerance)
    }

    probP1 = 1.0-0.5*(1.0-exp(-2.0*(50.0)/100.0))
    assertEquals("At " + 0 + ":", probP1, gamete.probabilityOf(p1, 0), tolerance)
    assertEquals("At " + 0 + ":", 1 - probP1, gamete.probabilityOf(p2, 0), tolerance)
  }

  @Test
  def proportionOf {
    assertEquals(0.5, new Chromosome(tidA, tidB).proportionOf(p1), tolerance)
    assertEquals(0.5, new Chromosome(tidA, tidB).proportionOf(p2), tolerance)
    assertEquals(0, new Chromosome(tidA, tidB).proportionOf(p3), tolerance)
    tidA(0) = new Centimorgan(p3)
    tidA(1) = new Centimorgan(p3)
    assertEquals(0.01, new Chromosome(tidA, tidB).proportionOf(p3), tolerance)

    val cTid1 = mock[Chromatid]
    when(cTid1.sumProbabilitiesOf(p1)).thenReturn(2.0)
    when(cTid1.size).thenReturn(4)

    val cTid2 = mock[Chromatid]
    when(cTid2.sumProbabilitiesOf(p1)).thenReturn(3.0)
    when(cTid2.size).thenReturn(4)

    val chromosome = new Chromosome(cTid1, cTid2)
    val expectedExpectedProportion = (2.0 + 3.0) / 8.0

    assertEquals(expectedExpectedProportion, chromosome.proportionOf(p1), tolerance)
  }

  @Test
  def makingGameteSubcallsOnEachCentimorgan {
    val cM1a = mock[Centimorgan]
    val cM1b = mock[Centimorgan]
    val cTidArray1 = new Array[Centimorgan](2)
    cTidArray1(0) = cM1a
    cTidArray1(1) = cM1b

    val cM2a = new Centimorgan()
    val cM2b = new Centimorgan()
    val cTidArray2 = new Array[Centimorgan](2)
    cTidArray2(0) = cM2a
    cTidArray2(1) = cM2b

    val chromosome = new Chromosome(new Chromatid(cTidArray1), new Chromatid(cTidArray2))
    chromosome.makeGameteNoSelection

    verify(cM1a).gameteify(cM2a, 0.5)
    verify(cM1b).gameteify(cM2b, 0.5)
  }

  @Test
  def makeGameteNoSelection() {
    val cTidArray1 = new Array[Centimorgan](5)
    cTidArray1(0) = new Centimorgan(p1)
    cTidArray1(1) = new Centimorgan(p1)
    cTidArray1(2) = new Centimorgan(p2)
    cTidArray1(3) = new Centimorgan(p1)
    cTidArray1(4) = new Centimorgan(p1)

    val cTidArray2 = new Array[Centimorgan](5)
    cTidArray2(0) = new Centimorgan(p3)
    cTidArray2(1) = new Centimorgan(p3)
    cTidArray2(2) = new Centimorgan(p3)
    cTidArray2(3) = new Centimorgan(p3)
    cTidArray2(4) = new Centimorgan(p4)

    val chromosome = new Chromosome(new Chromatid(cTidArray1), new Chromatid(cTidArray2))

    val gamete = chromosome.makeGameteNoSelection

    var expectedProbability = 0.5

    assertProbEquals(expectedProbability, gamete(0).alleles(p1))
    assertProbEquals(expectedProbability, gamete(0).alleles(p3))
    assertFalse(gamete(0).alleles.contains(p2))
    assertFalse(gamete(0).alleles.contains(p4))

    assertProbEquals(expectedProbability, gamete(1).alleles(p1))
    assertProbEquals(expectedProbability, gamete(1).alleles(p3))
    assertFalse(gamete(1).alleles.contains(p2))
    assertFalse(gamete(1).alleles.contains(p4))

    assertProbEquals(expectedProbability, gamete(2).alleles(p2))
    assertProbEquals(expectedProbability, gamete(2).alleles(p3))
    assertFalse(gamete(2).alleles.contains(p1))
    assertFalse(gamete(2).alleles.contains(p4))

    assertProbEquals(expectedProbability, gamete(3).alleles(p1))
    assertProbEquals(expectedProbability, gamete(3).alleles(p3))
    assertFalse(gamete(3).alleles.contains(p2))
    assertFalse(gamete(3).alleles.contains(p4))

    assertProbEquals(expectedProbability, gamete(4).alleles(p1))
    assertProbEquals(expectedProbability, gamete(4).alleles(p4))
    assertFalse(gamete(4).alleles.contains(p2))
    assertFalse(gamete(4).alleles.contains(p3))
  }

  private def assertProbEquals(a: Double, b: Double) = assertEquals(a, b, tolerance)
}
