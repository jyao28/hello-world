
library('MASS')
library('nlme')
library('RHmm') #Load HMM package
#Code based upon http://systematicinvestor.wordpress.com/2012/11/01/regime-detection/
bullMarketOne = rnorm( 100, 0.1/365, 0.05/sqrt(365) )
bearMarket  = rnorm( 100, -0.2/365, 0.15/sqrt(365))
bullMarketTwo = rnorm( 100, 0.15/365, 0.07/sqrt(365) )
true.states = c(rep(0,100),rep(0.5,100),rep(1,100))
# returns = c( bullMarketOne, bearMarket, bullMarketTwo )
returns = c( bearMarket, bullMarketOne, bullMarketOne, bearMarket )

y=returns
ResFit = HMMFit(y, nStates=2) #Fit a HMM with 2 states to the data
VitPath = viterbi(ResFit, y) #Use the viterbi algorithm to find the most likely state path (of the training data)
fb = forwardBackward(ResFit, y) #Forward-backward procedure, compute probabilities
 
 
# Plot probabilities and implied states
layout(1:3)
#plot(cumsum(returns),ylab="Cumulative Market Return",type="l", main="Fake Market Data")
plot(returns,ylab="Cumulative Market Return",type="l", main="Fake Market Data")
plot(VitPath$states, type='s', main='Implied States', xlab='', ylab='State')
matplot(fb$Gamma, type='l', main='Smoothed Probabilities', ylab='Probability')
legend(x='topright', c('Bear Market - State 2','Bull Market - State 1'),  fill=1:2, bty='n')

