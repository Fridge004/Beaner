package org.tearne.beaner.report

import org.scalatest.junit.JUnitSuite
import org.scalatest.mock.MockitoSugar
import org.junit.Test
import org.tearne.beaner.plant.Plant
import org.tearne.beaner.cross.{Criterion,Criteria}
import org.tearne.beaner.chroma.Centimorgan

import com.itextpdf.text.BaseColor

class ColourTest extends JUnitSuite with MockitoSugar{

  @Test def exceptionIfMorePlantsThanAvailableColours {
    val elevenPlants = 1.to(11).map(i => mock[Plant])
    val temp = elevenPlants.map(p => new Criterion(p,0,0))
    val criteria = new Criteria() ++ temp

    intercept[ColourException]{
      new Colour(criteria, mock[Plant])
    }
  }


  @Test def blendedCentiMorganColour {
    //
    // TODO: Success of this test is dependent on the order of the plants stored in the criteria
    //
    val prefVar = mock[Plant]
    val p0 = mock[Plant]
    val p1 = mock[Plant]

    val alleles = Map(prefVar -> 0.2, p0 -> 0.3, p1 -> 0.4)
    val cM = new Centimorgan(alleles)

    val criteria = new Criterion(p0, 0, 1) + new Criterion(p1, 2, 3)
    val colour = new Colour(criteria,prefVar)

    val colour1 = colour(prefVar)
    val colour2 = colour(p0)
    val colour3 = colour(p1)

    val expectedRed   = red(colour1)*0.2 + red(colour2)*0.3 + red(colour3)*0.4
    val expectedGreen = green(colour1)*0.2 + green(colour2)*0.3 + green(colour3)*0.4
    val expectedBlue  = blue(colour1)*0.2 + blue(colour2)*0.3 + blue(colour3)*0.4

    val expectedColour = new BaseColor(
      expectedRed.asInstanceOf[Int],
      expectedGreen.asInstanceOf[Int],
      expectedBlue.asInstanceOf[Int]
    )

    assert( expectedColour === colour(cM) )
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

    var colours = Set[BaseColor]()
    for(i <- 0 to 9){
      colours += colour(plants(i))
    }

    assert(10 === colours.size)
    0.to(9).foreach{
      i => assert(colours.contains(Colour.donorColours(i)), "Result doesn't contain "+i+"th colour: "+Colour.donorColours(i)+colours)
    }
  }

  def red(c: BaseColor) = c.getRed
  def green(c: BaseColor) = c.getGreen
  def blue(c: BaseColor) = c.getBlue
}