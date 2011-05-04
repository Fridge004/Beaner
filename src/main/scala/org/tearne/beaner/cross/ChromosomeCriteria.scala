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

sealed abstract class ChromosomeCriteria
case class SingleChromosomeCriteria(val criterion: Criterion) extends ChromosomeCriteria
case class DoubleChromosomeCriteria(val criterion1: Criterion, val criterion2: Criterion) extends ChromosomeCriteria

object ChromosomeCriteria{
  def apply(c: Criterion) = new SingleChromosomeCriteria(c)
  def apply(c1: Criterion, c2: Criterion) = {
    if(c1.plant != c2.plant)
      throw new ChromosomeCriteriaException("Donor plants must be equals for double selection on same chromosome")
    else if(c1.chromosomeIndex != c2.chromosomeIndex)
      throw new ChromosomeCriteriaException("Chromasome index must be consistent for double selection")
    else if(c1.cMIndex == c2.cMIndex)
      throw new ChromosomeCriteriaException("Centimorgans cannot be equal for double selection")

    new DoubleChromosomeCriteria(c1,c2)
  }
}