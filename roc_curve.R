#ROC curve function
  #author: Matthew Sigurdso (msigurds@stanford.edu), based on original calculation by Paul Riviere
  #requires pROC package

#Three required arguments: takes in a model from glm(), a data frame, and the name of the outcome column

#Three optional arguments (select at most one; default value is FALSE for each):
  #returnValues outputs a data frame containing the sensitivity and specificity for each possible threshold
  #returnPred outputs the predicted probabilities from the model
  #returnROC outputs the full roc object

#Example usage: roc.curve(glm.fit, data, "outcomeName")

roc.curve = function(glm.fit,data,outcome,returnValues=FALSE,returnPred=FALSE,returnROC=FALSE) {
  #Check input
  if (!is.character(outcome)) {stop("outcome must be a string")}
  if (sum(returnValues,returnPred,returnROC)>1) {stop("Select one output at a time.")}
  
  #Define outcome column by renaming it
  outcomeIndex = which(colnames(data)==outcome)
  names(data)[outcomeIndex] = "outcome"
  
  #Make prediction based on the fit
  prediction = predict(glm.fit,type=c("response"))
  
  #Calculate ROC curve
  data = data.frame(data,prediction)
  curve = roc(outcome~prediction,data=data)
  plot(curve)    
  print(curve)   #Summarizes the result, including the area under the curve
    #By default, the plot function labels the x-axis as "Specificity" (from 1 to 0) instead of "1-specificity" from 0 to 1
  
  #Output, if requested
  if (returnValues==TRUE) {
    output = data.frame(curve$thresholds,curve$sensitivities,curve$specificities)
    names(output) = c("threshold","sensitivity","specificity")
    return(round(output,digits=10))   #rounding is to prevent display as scientific notation
  }

  if (returnPred==TRUE) {
    #For confidence intervals: calculate the predictions on the linear ("link") scale
    linkPred = predict(glm.fit, type=c("link"), se.fit=TRUE)
    
    #Calculate the 95% confidence intervals, then convert back to the logistic scale using the inverse logit function
    lower.limit = linkPred$fit - 1.96 * linkPred$se.fit
    lower.limit = exp(lower.limit)/(1+exp(lower.limit))
    upper.limit = linkPred$fit + 1.96 * linkPred$se.fit
    upper.limit = exp(upper.limit)/(1+exp(upper.limit))
    
    output = data.frame(prediction, lower.limit, upper.limit)
    return(round(output,digits=10))   #rounding is to prevent display as scientific notation
  }

  if (returnROC==TRUE) {return(curve)}
}

#ryan edited this
