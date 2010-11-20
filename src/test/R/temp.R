## Probability of an odd number of crossovers
pOddCross = function(d){
  0.5*(1-exp(-2*d/100))
}

pOddCross(1:20)


prob <- rep(1,100)
for(i in 2:100){
prob[i] = prob[i-1]*0.99+(1-prob[i-1])*0.01
}
plot(1:100, prob)

## mean percentage in
a=0.01
1/3*(a*(1-a)^2+a^3+a^2*(1-a))+
2/3*(a*(1-a)^2+2*a^2*(1-a))+
(1-a)^3

(0.99+0.9802+0.970596)/3
