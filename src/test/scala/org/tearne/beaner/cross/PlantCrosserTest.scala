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

package org.tearne.beaner.cross

import org.tearne.beaner._
import plant._
import chroma._
import criteria._
import cross._

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.junit.Assert.{ assertEquals, assertTrue }
import org.scalatest.junit.JUnitSuite
import org.scalatest.mock.MockitoSugar
import org.scalatest.Assertions._
import org.mockito.Mockito._
import org.mockito.Matchers._
import org.mockito.stubbing.Answer
import org.mockito.invocation.InvocationOnMock

class PlantCrosserTest extends JUnitSuite with MockitoSugar {
  val tolerance = 1e-16
  val plantName = "myPlant"
  val positive1 = Option(0.1)
  val positive2 = Option(0.1)
  val positive3 = Option(0.1)

  //Create simple namedPlant spec (3 chromasomes of lengths 3, 2, 20 and 1)
  val spec = mock[PlantSpec]; when(spec.chromosomeLengths).thenReturn(Array[Int](3, 2, 20, 1))

  val p1 = new ParentPlant(spec, "donor1")
  val p2 = new ParentPlant(spec, "donor2")
  
  val plantPair = PlantPair(p1, p2)

  //Create criteria to select for 
  // p1 on the first chromosome, third cM
  // p2 on the second chromosome, second cM
  // p1 on third chromosome, first AND tenth cM
  // No selection on the fourth chromasome
  
  //val criteria = new Criterion(p1, 0, 2) + new Criterion(p2, 1, 1)
  val criteria = mock[CriteriaProvider]
  when(criteria.getGatheredSelectionCriterion).thenReturn(
    Set[SelectionCriterion](
        new Criterion(p1, 0, 2),
        new Criterion(p2, 1, 1),
        new DoubleCriterion(new Criterion(p2,2,1),new Criterion(p2,2,10)) )
  )
  
  //Result Chromosomes
  val chromosome0 = mock[Chromosome];
  when(chromosome0.size).thenReturn(3)
  when(chromosome0.selectionProbability).thenReturn(positive1)

  val chromosome1 = mock[Chromosome];
  when(chromosome1.size).thenReturn(2)
  when(chromosome1.selectionProbability).thenReturn(positive2)

  val chromosome2 = mock[Chromosome];
  when(chromosome2.size).thenReturn(20)
  when(chromosome2.selectionProbability).thenReturn(positive3)
  
  val chromosome3 = mock[Chromosome];
  when(chromosome3.size).thenReturn(1)
  when(chromosome3.selectionProbability).thenReturn(None)

  @Test def parentsAreSetOnOffspringPlant_Heterozygous {
	val chromosomeCrosser = mock[ChromosomeCrosser]
    when(chromosomeCrosser.selectHeterozygousOffspring(p1.chromosomes(0), p2.chromosomes(0), p1, 2)).thenReturn(chromosome0)
    when(chromosomeCrosser.selectHeterozygousOffspring(p1.chromosomes(1), p2.chromosomes(1), p2, 1)).thenReturn(chromosome1)
    when(chromosomeCrosser.selectHeterozygousOffspring(p1.chromosomes(2), p2.chromosomes(2), p1, 1, 10)).thenReturn(chromosome2)
    when(chromosomeCrosser.getOffspringWithoutSelection(p1.chromosomes(2), p2.chromosomes(2))).thenReturn(chromosome3)
    
    val offspring = new PlantCrosser(chromosomeCrosser).selectHeterozygousOffspring(PlantPair(p1, p2), criteria)
    assertEquals(PlantPair(p1, p2), offspring.parents.get)
  }
  
  @Test def parentsAreSetOnOffspringPlant_Homozygous {
    val chromosomeCrosser = mock[ChromosomeCrosser]
    when(chromosomeCrosser.selectHomozygousOffspring(p1.chromosomes(0), p2.chromosomes(0), p1, 2)).thenReturn(chromosome0)
    when(chromosomeCrosser.selectHomozygousOffspring(p1.chromosomes(1), p2.chromosomes(1), p2, 1)).thenReturn(chromosome1)
    when(chromosomeCrosser.selectHomozygousOffspring(p1.chromosomes(1), p2.chromosomes(1), p2, 1, 10)).thenReturn(chromosome2)
    when(chromosomeCrosser.getOffspringWithoutSelection(p1.chromosomes(2), p2.chromosomes(2))).thenReturn(chromosome3)
     
    val offspring = new PlantCrosser(chromosomeCrosser).selectHomozygousOffspring(PlantPair(p1, p2), criteria)
    assertEquals(PlantPair(p1, p2), offspring.parents.get)
  }

  @Test
  def cantCrossUnnamedOffspringPlants {
	val chromas = Array(chromosome0, chromosome1, chromosome2, chromosome3)

    val plantCrosser = new PlantCrosser(mock[ChromosomeCrosser])

    val p1 = new OffspringPlant(chromas, spec, Some(0.1)) //Has no name
    val p2 = new ParentPlant(spec, plantName)

    intercept[UnnamedPlantException]{
      plantCrosser.selectHeterozygousOffspring(p1 x p2, criteria)
    }
  }

  @Test
  def exceptionIfTryToBreedPlantsWithDifferentNumberOfChromosomes {
    var altSpec = mock[PlantSpec]
    when(altSpec.chromosomeLengths).thenReturn(Array(100))

    val p1 = PhaseolusVulgaris("myPlant")
    val p2 = new ParentPlant(altSpec, "myOtherPlant")

    val plantCrosser = new PlantCrosser(mock[ChromosomeCrosser])
    intercept[PlantCrosserException] { plantCrosser.selectHeterozygousOffspring(PlantPair(p1, p2), null) }
    intercept[PlantCrosserException] { plantCrosser.selectHomozygousOffspring(PlantPair(p1, p2), null) }
  }

  @Test
  def heterozygousCrossing {
    val chromasomeCrosser = mock[ChromosomeCrosser]
    when(chromasomeCrosser.selectHeterozygousOffspring(p1.chromosomes(0), p2.chromosomes(0), p1, 2)).thenReturn(chromosome0)
    when(chromasomeCrosser.selectHeterozygousOffspring(p1.chromosomes(1), p2.chromosomes(1), p2, 1)).thenReturn(chromosome1)
    when(chromasomeCrosser.selectHeterozygousOffspring(p1.chromosomes(2), p2.chromosomes(2), p1, 1, 10)).thenReturn(chromosome2)
    when(chromasomeCrosser.getOffspringWithoutSelection(p1.chromosomes(3), p2.chromosomes(3))).thenReturn(chromosome3)

    val offspring = new PlantCrosser(chromasomeCrosser).selectHeterozygousOffspring(plantPair, criteria)

    assertEquals( chromosome0, offspring.chromosomes(0) )
    assertEquals( chromosome1, offspring.chromosomes(1) )
    assertEquals( chromosome2, offspring.chromosomes(2) )
    assertEquals( chromosome3, offspring.chromosomes(3) )
    assert(offspring.parents.get === plantPair)

    //No selection probability on third chromasome since pretending there is no selection on it
    val expectedSelectionProb = chromosome0.selectionProbability.get *
      							chromosome1.selectionProbability.get *
      							chromosome2.selectionProbability.get
    assertEquals(expectedSelectionProb, offspring.selectionProbability.get, tolerance)
  }

  @Test
  def homozygousCrossing {
    val chromasomeCrosser = mock[ChromosomeCrosser]
    when(chromasomeCrosser.selectHomozygousOffspring(p1.chromosomes(0), p2.chromosomes(0), p1, 2)).thenReturn(chromosome0)
    when(chromasomeCrosser.selectHomozygousOffspring(p1.chromosomes(1), p2.chromosomes(1), p2, 1)).thenReturn(chromosome1)
    when(chromasomeCrosser.selectHomozygousOffspring(p1.chromosomes(2), p2.chromosomes(2), p1, 1, 10)).thenReturn(chromosome2)
    when(chromasomeCrosser.getOffspringWithoutSelection(p1.chromosomes(3), p2.chromosomes(3))).thenReturn(chromosome3)

    val offspring = new PlantCrosser(chromasomeCrosser).selectHomozygousOffspring(plantPair, criteria)

    assertEquals( chromosome0, offspring.chromosomes(0) )
    assertEquals( chromosome1, offspring.chromosomes(1) )
    assertEquals( chromosome2, offspring.chromosomes(2) )
    assertEquals( chromosome3, offspring.chromosomes(3) )
    assert( offspring.parents.get === plantPair )

    val expectedSelectionProb = chromosome0.selectionProbability.get * 
    							chromosome1.selectionProbability.get *
    							chromosome3.selectionProbability.get
    //No selection probability on third chromasome since pretending there is no selection on it
    assertEquals( expectedSelectionProb, offspring.selectionProbability.get, tolerance )
  }
}
