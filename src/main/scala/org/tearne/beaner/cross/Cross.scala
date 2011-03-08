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

import org.tearne.beaner.plant._
import org.tearne.beaner.chroma._

case class Cross(
    val pair:ParentPair,
    val criteria: Set[Criterion],
    name: Option[String] = None,
    val selectionType: Option[SelectionType.Value] = None) {

  type Self = Cross

  def this(pair:ParentPair,
           criteria: Set[Criterion],
           selectionType: SelectionType.Value) = this(pair, criteria, None, Option(selectionType))

  def x(plant: Plant) = Parents(this, plant)
  def x(that: Cross) = Parents(this, that)

  def named(newName:String) = copy(name=Option(newName))

  def evaluateUsing(plantCrosser: PlantCrosser): OffspringPlant = {
    if(name == None)
      throw new UnnamedPlantException()

    val plantPair: PlantPair = pair match{
      case CrossPair(first, second) =>
        PlantPair(first.evaluateUsing(plantCrosser), second.evaluateUsing(plantCrosser))
      case MixedPair(first, second) =>
        PlantPair(first.evaluateUsing(plantCrosser), second)
      case p:PlantPair => p
    }


    val result = selectionType match{
      case Some(SelectionType.Heterozygous) =>
        plantCrosser.selectHeterozygousOffspring(plantPair, criteria)
      case Some(SelectionType.Homozygous) =>
        plantCrosser.selectHomozygousOffspring(plantPair, criteria)
      //TODO test
      case None => throw new RuntimeException("No selection type specified")
    }

    result.get named name.get
  }
}