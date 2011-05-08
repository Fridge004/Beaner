package org.tearne.beaner.cross

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

import collection._
//import Iterator
import generic.{CanBuildFrom, GenericTraversableTemplate, GenericCompanion, SeqFactory}
import generic.CanBuildFrom
//import SetLike
import mutable.{ Builder, ArrayBuffer }

class Criteria(buf: ArrayBuffer[Criterion])
  extends IndexedSeq[Criterion]
  with IndexedSeqLike[Criterion, Criteria]{

  def this(criterion: Criterion*) = this(new ArrayBuffer[Criterion]() ++= criterion)

  override def newBuilder = Criteria.newBuilder
  private var size0 = 0

  override def size = buf.size
  override def stringPrefix = "Criteria"

  def contains(key: Criterion) = buf.contains(key)
  def apply(idx: Int) = buf.apply(idx)
  def length = buf.length
  override def iterator = buf.iterator
  def +(elem: Criterion) = if(buf contains elem) this else new Criteria(buf += elem)

  //
  // Non-collection functionality below
  //

  def getGatheredSelectionCriterion() = {
    val gatherByChromosomeIndex = mutable.Map[Int, Set[SelectionCriterion]]()
    foreach{c => {
      val index = c.chromosomeIndex
      if(!gatherByChromosomeIndex.contains(index))
        gatherByChromosomeIndex.update(index, Set[SelectionCriterion]())
      gatherByChromosomeIndex.update(index, gatherByChromosomeIndex(index)+c)
    }}

    gatherByChromosomeIndex.values.map{set => {
      if(set.size == 1) set.iterator.next
      else if(set.size == 2) {
        val it = set.iterator
        new DoubleCriterion(it.next.asInstanceOf[Criterion], it.next.asInstanceOf[Criterion])
      }
      else throw new RuntimeException("TODO - fix me")
    }}.toSet
  }
}

object Criteria{
  implicit def criteriaCanBuildFrom = new CanBuildFrom[Criteria, Criterion, Criteria](){
    def apply(from: Criteria) = newBuilder
    def apply() = newBuilder
  }

  def newBuilder: Builder[Criterion, Criteria] =
    new ArrayBuffer[Criterion] mapResult (buf => new Criteria(buf))

  def empty: Criteria = new Criteria(new ArrayBuffer[Criterion])
}