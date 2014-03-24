
fitEVD <- function(data, maxima, conditional = "NULL", ...){
  fit <- fevd(maxima, data = data, ...)
  if(conditional != "NULL"){
    fit.location <- fevd(maxima, data = data, location.fun = ~ conditional, ...)
    lrt.location <- lr.test(fit, fit.location)
    fit.scale <- fevd(maxima, data = data, scale.fun = ~ contional, ...)
    lrt.scale <- lr.test(fit, fit.scale)
    if(lrt.location$p.value > 0.05 & lrt.scale$p.value > 0.05){
      
    } else if(lrt.location$p.value <= 0.05 & lrt.scale$p.value > 0.05){
      fit <- fit.location
    } else if(lrt.location$p.value > 0.05 & lrt.scale$p.value <= 0.05){
      fit <- fit.scale
    } else if(lrt.location$p.value <= 0.05 & lrt.scale$p.value <= 0.05){
      fit.location.scale <- fevd(maxima, data = data, location.fun = ~ conditional,
                                 scale.fun = ~ conditional, ...)
      lrt.location.scale <- lr.test(fit, fit.location.scale)
      if(lrt.location.scale$p.value <= 0.05){
        fit.list <- list(fit.location, fit.scale, fit.location.scale)
        nllhs <- c(summary(fit.location)$nllh, summary(fit.scale)$nllh, 
                   summary(fit.location.scale)$nllh)
        fit <- fit.list[[match(min(nllhs), nllhs)]]
      } else {
        fit.list <- list(fit.location, fit.scale)
        nllhs <- c(summary(fit.location)$nllh, summary(fit.scale)$nllh)
        fit <- fit.list[[match(min(nllhs), nllhs)]]
      }
    }
  }
  return(fit)
}

predict <- function(...){
  
}

