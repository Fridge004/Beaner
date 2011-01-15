pSplit = 0.01
range = 1:200

#
# Only one recombination
#
probIn = function(cmNo){
  (1-pSplit)^cmNo
}

probOut = function(cmNo){
  terms = function(j){ (1-pSplit)^(j-1) }
  pSplit*sum(terms(1:cmNo))
}

probSum = function(i){
  probIn(i) + probOut(i)
}

maxOneProbs = sapply(range, probIn)

#
# Independent recombinations
#
prob <- rep(1,100)
prob[1] = 1-pSplit
for(i in 2:100){
	prob[i] = prob[i-1]*0.99+(1-prob[i-1])*0.01	
}

indepProbs = prob

##
# Poisson approximation
##

probInPoisson = function(d){
  1-0.5*(1-exp(-2*d/100))
}

poissonProbs = sapply(range, probInPoisson)
#
# Plot
#

line = c(seq(1-pSplit, 0.5, by=-pSplit), rep(0.5,150))

pdf("plot.pdf")
plot(range, maxOneProbs, col='red', ylim=c(0,1), xlab="Distance (cM)", ylab="Prob.")
points(range, poissonProbs, col='green')
points(range, indepProbs, col='blue')
points(range, line, type='l')
title("Probability of drag at cM")
dev.off()
