library('MASS')
library('nlme')
library('RHmm') #Load HMM package
library('zoo')
 
#HMM model 1 (high vol and low vol upwards trend)
model1ReturnsFunc <- function(isHighVol){
  return(rnorm( 100, 0.1,if(isHighVol){0.15}else{0.02}))
}
bullLowVol = model1ReturnsFunc(F)
bullHighVol  = model1ReturnsFunc(T)
model1TrainingReturns = c(bullLowVol, bullHighVol)
Model1Fit = HMMFit(model1TrainingReturns, nStates=2) #Fit a HMM with 2 states to the data
 
#HMM model 2 (high vol and low vol downwards trend)
model2ReturnsFunc <- function(isHighVol){
  return(rnorm( 100, -0.1,if(isHighVol){0.15}else{0.02}))
}
bearLowVol = model2ReturnsFunc(F)
bearHighVol  = model2ReturnsFunc(T)
model2TrainingReturns = c(bearLowVol, bearHighVol)
Model2Fit = HMMFit(model2TrainingReturns, nStates=2) #Fit a HMM with 2 states to the data
 
#HMM model 3 (sideways market)
model3ReturnsFunc <- function(isHighVol){
  return(rnorm( 100, 0.0,if(isHighVol){0.16}else{0.08}))
}
sidewaysLowVol = model3ReturnsFunc(F)
sidewaysHighVol  = model3ReturnsFunc(T)
model3TrainingReturns = c(sidewaysLowVol, sidewaysHighVol)
Model3Fit = HMMFit(model3TrainingReturns, nStates=2) #Fit a HMM with 2 states to the data
 
generateDataFunc <- function(modelSequence,highVolSequence){
  results <- c()
  if(length(modelSequence) != length(highVolSequence)){ print("Model Sequence and Vol Sequence must be the same length"); return(NULL)}
  for(i in 1:length(modelSequence)){
    #Bit rubish having all these IFs here but its easy to understand for novice R users
    if(modelSequence[i] == 1){
       results <- c(results,model1ReturnsFunc(highVolSequence[i]))
    }
    if(modelSequence[i] == 2){
       results <- c(results,model2ReturnsFunc(highVolSequence[i]))
    }
    if(modelSequence[i] == 3){
       results <- c(results,model3ReturnsFunc(highVolSequence[i]))
    }
  }
  return(results)
}
 
#Create some out of sample data
actualModelSequence <- c(1,1,1,3,2,2,1)
actualVolRegime <- c(T,T,T,T,T,T,T)
outOfSampleData <- generateDataFunc(actualModelSequence,actualVolRegime)
#Will take 50 days of data and calculate the rolling log likelihood for each HMM model
model1Likelihood <- rollapply(outOfSampleData,50,align="right",na.pad=T,function(x) {forwardBackward(Model1Fit,x)$LLH})
model2Likelihood <- rollapply(outOfSampleData,50,align="right",na.pad=T,function(x) {forwardBackward(Model2Fit,x)$LLH})
model3Likelihood <- rollapply(outOfSampleData,50,align="right",na.pad=T,function(x) {forwardBackward(Model3Fit,x)$LLH})
layout(1:3)
plot(cumsum(outOfSampleData),main="Fake Market Data",ylab="Cumulative Returns",type="l")
plot(model1Likelihood,type="l",ylab="Log Likelihood of Each Model",main="Log Likelihood for each HMM Model")
lines(model2Likelihood,type="l",col="red")
lines(model3Likelihood,type="l",col="blue")
plot(rep((actualModelSequence==3)*3,each=100),col="blue",type="o",ylim=c(0.8,3.1),ylab="Actual MODEL Number",main="Actual MODEL Sequence")
lines(rep((actualModelSequence==2)*2,each=100),col="red",type="o")
lines(rep((actualModelSequence==1)*1,each=100),col="black",type="o")
legend(x='topright', c('Model 1 - Bull Mkt','Model 2 - Bear Mkt','Model 3 - Side ways Mkt'), col=c("black","red","blue"), bty='n',lty=c(1,1,1))
