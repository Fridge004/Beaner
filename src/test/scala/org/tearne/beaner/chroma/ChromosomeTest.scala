

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
  def exceptionIfChromatidsDiffLengths {
    intercept[ChromasomeException] { new Chromosome(tidA, tidD) }
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
}
