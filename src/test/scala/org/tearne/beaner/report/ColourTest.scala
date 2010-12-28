package org.tearne.beaner.report

import org.scalatest.junit.JUnitSuite
import org.scalatest.mock.MockitoSugar
import org.junit.Test
import org.tearne.beaner.plant.Plant
import org.tearne.beaner.cross.Criterion
import org.tearne.beaner.chroma.Centimorgan
import processing.core.PApplet
import collection.mutable.LinkedHashSet
import collection._

class ColourTest extends JUnitSuite with MockitoSugar{

  @Test def blendedCentiMorganColour {
    val prefVar = mock[Plant]
    val p0 = mock[Plant]
    val p1 = mock[Plant]

    val alleles = mutable.Map(prefVar -> 0.2, p0 -> 0.3, p1 -> 0.4)
    val cM = new Centimorgan(alleles)

    def red(c: Int) = c >> 16 & 0xFF
    def green(c: Int) = c >> 8 & 0xFF
    def blue(c: Int) = c & 0xFF

    val expectedRed   = red(Colour.prefVar)*0.2 + red(Colour.donorColours(0))*0.3 + red(Colour.donorColours(1))*0.4
    val expectedGreen = green(Colour.prefVar)*0.2 + green(Colour.donorColours(0))*0.3 + green(Colour.donorColours(1))*0.4
    val expectedBlue  = blue(Colour.prefVar)*0.2 + blue(Colour.donorColours(0))*0.3 + blue(Colour.donorColours(1))*0.4

    val expectedColour = new PApplet().color(
      expectedRed.asInstanceOf[Float],
      expectedGreen.asInstanceOf[Float],
      expectedBlue.asInstanceOf[Float]
    )

    val criteria: LinkedHashSet[Criterion] = new Criterion(p0, 0, 1) + new Criterion(p1, 2, 3)
    val colour = new Colour(criteria,prefVar)

    assert(expectedColour === colour(cM))
  }

  @Test def gettingPlantColours {
    val prefVar = mock[Plant]
    val p0 = mock[Plant]
    val p1 = mock[Plant]
    val p2 = mock[Plant]
    val p3 = mock[Plant]
    val p4 = mock[Plant]
    val p5 = mock[Plant]
    val p6 = mock[Plant]
    val p7 = mock[Plant]
    val p8 = mock[Plant]
    val p9 = mock[Plant]

    val criteria = new Criterion(p0, 0, 1) +
      new Criterion(p1, 2, 3) +
      new Criterion(p2, 2, 3) +
      new Criterion(p3, 4, 5) +
      new Criterion(p4, 0, 1) +
      new Criterion(p5, 2, 3) +
      new Criterion(p6, 4, 5) +
      new Criterion(p7, 0, 1) +
      new Criterion(p8, 2, 3) +
      new Criterion(p9, 4, 5) +
      new Criterion(p1, 0, 1) +
      new Criterion(p2, 2, 3) +
      new Criterion(p3, 4, 5)

    val colour = new Colour(criteria, prefVar)

    assert(Colour.prefVar === colour(prefVar))

    assert(Colour.donorColours(0) === colour(p0))
    assert(Colour.donorColours(1) === colour(p1))
    assert(Colour.donorColours(2) === colour(p2))
    assert(Colour.donorColours(3) === colour(p3))
    assert(Colour.donorColours(4) === colour(p4))
    assert(Colour.donorColours(5) === colour(p5))
    assert(Colour.donorColours(6) === colour(p6))
    assert(Colour.donorColours(7) === colour(p7))
    assert(Colour.donorColours(8) === colour(p8))
    assert(Colour.donorColours(9) === colour(p9))
  }
}