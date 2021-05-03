library(bnutil)
library(pgloess)
library(pgspline)
library(dplyr)
library(ggplot2)


getdata = function() {
  aData = AnnotatedData$new(data = pgloess::ddf ,pgloess::mdf)
}

setResult = function(annotatedResult){
  print(annotatedResult)
  result = annotatedResult$data
}

getProperties = function(){
  props = list(Interactive = "Yes")
}

bnMessageHandler = bnshiny::BNMessageHandler$new()
bnMessageHandler$getDataHandler = getdata
bnMessageHandler$setResultHandler = setResult
bnMessageHandler$getPropertiesAsMapHandler = getProperties


bnshiny::startBNTestShiny('pgspline', sessionType='run', bnMessageHandler=bnMessageHandler)
