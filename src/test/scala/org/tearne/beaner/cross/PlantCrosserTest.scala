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

import org.tearne.beaner.chroma._
import org.tearne.beaner.plant._
import org.tearne.beaner.cross._
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.junit.Assert.{ assertEquals, assertTrue }
import org.scalatest.junit.JUnitSuite
import org.scalatest.mock.MockitoSugar
import org.scalatest.Assertions._
import org.mockito.Mockito._
import org.mockito.Matchers._

class PlantCrosserTest extends JUnitSuite with MockitoSugar {
  val tolerance = 1e-16

  //Create simple namedPlant spec (3 chromasomes of lengths 3, 2 and 1)
  val spec = mock[PlantSpec]; when(spec.chromasomeLengths).thenReturn(Array[Int](3, 2, 1))

  val p1 = new ParentPlant(spec)
  val p2 = new ParentPlant(spec)
  val p3 = new ParentPlant(spec)

  //Create criteria to select for 
  // p1 on the first chromasome, third cM
  // p2 on the second chromasome, second cM
  //No selection on the third chromasome
  val criteria = new Criterion(p1, 0, 2) + new Criterion(p2, 1, 1)

  val chromasome0 = mock[Chromosome];
  when(chromasome0.size).thenReturn(3)
  when(chromasome0.selectionProbability).thenReturn(Option(0.1))

  val chromasome1 = mock[Chromosome];
  when(chromasome1.size).thenReturn(2)
  when(chromasome1.selectionProbability).thenReturn(Option(0.2))

  val chromasome2 = mock[Chromosome];
  when(chromasome2.size).thenReturn(1)
  when(chromasome2.selectionProbability).thenReturn(None)

  @Test
  def exceptionIfTryToBreedPlantsWithDifferentNumberOfChromosomes {
    var mockPlantSpec = mock[PlantSpec]
    when(mockPlantSpec.chromasomeLengths).thenReturn(Array(100))

    val p1 = PhaseolusVulgaris()
    val p2 = new ParentPlant(mockPlantSpec)

    val plantCrosser = new PlantCrosser(mock[ChromosomeCrosser])
    intercept[PlantCrosserException] { plantCrosser.selectHeterozygousOffspring(PlantPair(p1, p2), null) }
    intercept[PlantCrosserException] { plantCrosser.selectHomozygousOffspring(PlantPair(p1, p2), null) }
  }

  @Test
  def heterozygousCrossing {
    val chromasomeCrosser = mock[ChromosomeCrosser]
    when(chromasomeCrosser.selectHeterozygousOffspring(p1.chromasomes(0), p2.chromasomes(0), p1, 2)).thenReturn(Some(chromasome0))
    when(chromasomeCrosser.selectHeterozygousOffspring(p1.chromasomes(1), p2.chromasomes(1), p2, 1)).thenReturn(Some(chromasome1))
    when(chromasomeCrosser.getOffspringWithoutSelection(p1.chromasomes(2), p2.chromasomes(2))).thenReturn(Some(chromasome2))

    val plantCrosser = new PlantCrosser(chromasomeCrosser)
    val offspring = plantCrosser.selectHeterozygousOffspring(PlantPair(p1, p2), criteria).get

    assertEquals(chromasome0, offspring.chromasomes(0))
    assertEquals(chromasome1, offspring.chromasomes(1))
    assertEquals(chromasome2, offspring.chromasomes(2))

    val expectedSelectionProb = chromasome0.selectionProbability.get *
      chromasome1.selectionProbability.get
    //No selection probability on third chromasome since pretending there is no selection on it
    assertEquals(expectedSelectionProb, offspring.selectionProbability.get, tolerance)
  }

  @Test
  def failedHeteroxygousCrossing {
    val chromasomeCrosser = mock[ChromosomeCrosser]
    when(chromasomeCrosser.selectHeterozygousOffspring(p1.chromasomes(0), p2.chromasomes(0), p1, 2)).thenReturn(Some(chromasome0))
    when(chromasomeCrosser.selectHeterozygousOffspring(p1.chromasomes(1), p2.chromasomes(1), p2, 1)).thenReturn(None)
    when(chromasomeCrosser.getOffspringWithoutSelection(p1.chromasomes(2), p2.chromasomes(2))).thenReturn(Some(chromasome2))

    val plantCrosser = new PlantCrosser(chromasomeCrosser)
    val offspring = plantCrosser.selectHeterozygousOffspring(PlantPair(p1, p2), criteria)

    assertTrue(offspring == None)
  }

  @Test
  def homozygousCrossing {
    val chromasomeCrosser = mock[ChromosomeCrosser]
    when(chromasomeCrosser.selectHomozygousOffspring(p1.chromasomes(0), p2.chromasomes(0), p1, 2)).thenReturn(Some(chromasome0))
    when(chromasomeCrosser.selectHomozygousOffspring(p1.chromasomes(1), p2.chromasomes(1), p2, 1)).thenReturn(Some(chromasome1))
    when(chromasomeCrosser.getOffspringWithoutSelection(p1.chromasomes(2), p2.chromasomes(2))).thenReturn(Some(chromasome2))

    val plantCrosser = new PlantCrosser(chromasomeCrosser)
    val offspring = plantCrosser.selectHomozygousOffspring(PlantPair(p1, p2), criteria).get

    assertEquals(chromasome0, offspring.chromasomes(0))
    assertEquals(chromasome1, offspring.chromasomes(1))
    assertEquals(chromasome2, offspring.chromasomes(2))

    val expectedSelectionProb = chromasome0.selectionProbability.get *
      chromasome1.selectionProbability.get
    //No selection probability on third chromasome since pretending there is no selection on it
    assertEquals(expectedSelectionProb, offspring.selectionProbability.get, tolerance)
  }

  @Test
  def failedHomozygousCrossing {
    val chromasomeCrosser = mock[ChromosomeCrosser]
    when(chromasomeCrosser.selectHomozygousOffspring(p1.chromasomes(0), p2.chromasomes(0), p1, 2)).thenReturn(None)
    when(chromasomeCrosser.selectHomozygousOffspring(p1.chromasomes(1), p2.chromasomes(1), p2, 1)).thenReturn(Some(chromasome1))
    when(chromasomeCrosser.getOffspringWithoutSelection(p1.chromasomes(2), p2.chromasomes(2))).thenReturn(Some(chromasome2))

    val plantCrosser = new PlantCrosser(chromasomeCrosser)
    val offspring = plantCrosser.selectHomozygousOffspring(PlantPair(p1, p2), criteria)

    assertTrue(offspring == None)
  }
}
