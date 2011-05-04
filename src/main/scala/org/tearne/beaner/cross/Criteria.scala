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

import scala.collection.Iterator
import scala.collection.generic.{ CanBuildFrom, GenericCompanion, MutableSetFactory }
import scala.collection.SetLike
import scala.collection.mutable.{ Builder, SetBuilder }

class Criteria(elems: Criterion*) extends Set[Criterion] with SetLike[Criterion, Criteria] {
  private val rep = Set[Criterion]()
  private var size0 = 0

  override def size = elems.size
  override def empty: Criteria = new Criteria()
  override def stringPrefix = "Criteria"

  def contains(key: Criterion) = elems.contains(key)
  def iterator = elems.iterator
  override def +(elem: Criterion) = if(elems contains elem) this else new Criteria( elem +: elems: _*)
  override def -(elem: Criterion) = if(!(elems contains elem)) this else new Criteria( elems filterNot(elem == _): _* )

  // Non-collection functionality below
  def getChromosomeCriteria() = Set[ChromosomeCriteria]()
}

object Criteria { //Was mutable set factory
  implicit def criteriaCanBuildFrom = new CanBuildFrom[Criteria, Criterion, Criteria](){
    def apply(from: Criteria) = newBuilder
    def apply() = newBuilder
  }
  def newBuilder: Builder[Criterion, Criteria] = new SetBuilder[Criterion, Criteria](empty)

  def empty: Criteria = new Criteria()
}