HSImodelFit <- function(modLayers,
                        IKLayer, subsetForModel = FALSE){
  
  if (isTRUE(subsetForModel)){
    subsetForModel <- 50
  } else {
    if (any(!is(subsetForModel, "numeric"), isFALSE(subsetForModel))){
      subsetForModel <- NULL
    }
  }
  stk <- raster::stack(modLayers, IKLayer)
  names(stk)[names(stk) == names(IKLayer)] <- "IKranks"
  modelData <- data.table(getValues(stk))
  modelData <- na.omit(modelData)
  if (!is.null(subsetForModel)){
    modelData <- suppressMessages(LandR::subsetDT(modelData, 
                                                  by = "IKranks", 
                                                  doSubset = subsetForModel))
  }
  modelData[, IKranks := as.factor(IKranks)]
  
  # With subsetForModel = 50, takes about 1.5 minutes
  # With the full dataset it might be several hours.
  HSImodel <- ordfor(depvar = "IKranks", data = modelData, nsets = 1000, ntreeperdiv = 100,
                      ntreefinal = 5000, perffunction = "equal")
  return(HSImodel)
}

