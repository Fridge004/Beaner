package org.tearne.beaner.report

import org.scalatest.junit.JUnitSuite
import org.scalatest.mock.MockitoSugar
import org.junit.Test
import org.tearne.beaner.plant.Plant
import org.tearne.beaner.cross.Criterion
import org.tearne.beaner.chroma.Centimorgan
import processing.core.PApplet

class ColourTest extends JUnitSuite with MockitoSugar{

  @Test def exceptionIfMorePlantsThanAvailableColours {
    val elevenPlants = 1.to(11).map(i => mock[Plant])
    val criteria = elevenPlants.map(p => new Criterion(p,0,0)).toSet

    intercept[ColourException]{
      new Colour(criteria, mock[Plant])
    }
  }


  @Test def blendedCentiMorganColour {
    val prefVar = mock[Plant]
    val p0 = mock[Plant]
    val p1 = mock[Plant]

    val alleles = Map(prefVar -> 0.2, p0 -> 0.3, p1 -> 0.4)
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

    val criteria: Set[Criterion] = new Criterion(p0, 0, 1) + new Criterion(p1, 2, 3)
    val colour = new Colour(criteria,prefVar)

    assert(expectedColour === colour(cM))
  }

  @Test def gettingPlantColours {
    val prefVar = mock[Plant]
    val plants = List(
      mock[Plant],
      mock[Plant],
      mock[Plant],
      mock[Plant],
      mock[Plant],
      mock[Plant],
      mock[Plant],
      mock[Plant],
      mock[Plant],
      mock[Plant]
    )

    val criteria = new Criterion(plants(0), 0, 1) +
                  new Criterion(plants(1), 2, 3) +
                  new Criterion(plants(2), 2, 3) +
                  new Criterion(plants(3), 4, 5) +
                  new Criterion(plants(4), 0, 1) +
                  new Criterion(plants(5), 2, 3) +
                  new Criterion(plants(6), 4, 5) +
                  new Criterion(plants(7), 0, 1) +
                  new Criterion(plants(8), 2, 3) +
                  new Criterion(plants(9), 4, 5) +
                  new Criterion(plants(1), 0, 1) +
                  new Criterion(plants(2), 2, 3) +
                  new Criterion(plants(3), 4, 5)

    val colour = new Colour(criteria, prefVar)

    assert(Colour.prefVar === colour(prefVar))

    var colours = Set[Int]()
    for(i <- 0 to 9){
      colours += colour(plants(i))
    }

    assert(10 === colours.size)
    0.to(9).foreach{
      i => assert(colours.contains(Colour.donorColours(i)), "Result doesn't contain "+i+"th colour: "+Colour.donorColours(i)+colours)
    }
  }
}