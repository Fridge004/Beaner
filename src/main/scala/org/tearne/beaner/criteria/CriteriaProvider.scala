package org.tearne.beaner.criteria

trait CriteriaProvider {
	def getGatheredSelectionCriterion(): Set[SelectionCriterion]
}