##########################################################################
# Workflows
# define several workflows for applying the different resampling strategies for regression tasks
##########################################################################


WFMetaUtil <- function(form, train, test, method, npts, control.parms, ymin, ymax, tloss, epsilon, 
                   strat.parms, learner, learner.pars){ # workflow for using Metautil strategy
  pc <- list()
  pc$method <- method
  pc$npts <- npts
  pc$control.pts <- control.parms
  
  loss.parms <- list()
  loss.parms$ymin <- ymin
  loss.parms$ymax <- ymax
  loss.parms$tloss <- tloss
  loss.parms$epsilon <- epsilon
  
    
  predictor.pars <- list()
  predictor <- "predict"
  if(learner == "svm"){
    predictor.pars <- list(probability=TRUE)
  } else if(learner == "randomForest"){
    predictor.pars <- list(type="prob")
  }
  
  mystrat.parms <- list(p=strat.parms$p, control.parms=pc, loss.parms=loss.parms)

  preds <- MetaUtilRegress(form, train, test, type = "util", strat = "auto", 
                     strat.parms=mystrat.parms,
                     minds=NULL, maxds=NULL, eps=0.1, m=20, n=NULL, q=TRUE, 
                     learner, learner.pars, predictor, predictor.pars)
  res <- list(trues=responseValues(form,test),preds=preds)
  return(res)
}



# define the learn/test functions for the systems

cv.lm<- function(form, train,test,...) {
  m <- lm(form, train,...)
  li <- m$xlevels
  for(i in 1:length(names(li))){
    m$xlevels[[names(li)[i]]] <- union(m$xlevels[[names(li)[i]]], levels(test[,names(li)[i]]))
  }
  predict(m,test, interval="none")
}

cv.svm <- function(form,train,test,...) {
  m <- svm(form,train,...)
  predict(m,test)
}
cv.randomForest <- function(form,train,test,...) {
  m <- randomForest(form,train,...)
  predict(m,test)
}

cv.earth <- function(form,train,test,...) {
  m <- earth(form,train,...)
  predict(m,test)[,1]
}

cv.nnet <- function(form, train, test,...){
  m <- nnet(form,train,...)
  predict(m, test)
}


# ============================================================
# EVALUATION STATISTICS
# metrics definition for the estimation task
# ============================================================

eval.stats <- function(trues, preds, train, metrics,
                       thr.rel, method,npts,control.pts,
                       ymin,ymax,tloss,epsilon,p) {
  pc <- list()
  pc$method <- method
  pc$npts <- npts
  pc$control.pts <- control.pts
  lossF.args <- list()
  lossF.args$ymin <- ymin
  lossF.args$ymax <- ymax
  lossF.args$tloss <- tloss
  lossF.args$epsilon <- epsilon
  
  MU <- util(preds, trues, pc, lossF.args, util.control(umetric="MU",p=p))
  NMU <- util(preds, trues, pc, lossF.args, util.control(umetric="NMU",p=p))
  ubaprec <- util(preds,trues,pc,lossF.args,util.control(umetric="P", event.thr=thr.rel, p=p))
  ubarec  <- util(preds,trues,pc,lossF.args,util.control(umetric="R", event.thr=thr.rel, p=p))
  ubaF   <- util(preds,trues,pc,lossF.args,util.control(umetric="Fm",beta=1, event.thr=thr.rel, p=p))
  
  c(mad = mean(abs(trues-preds)),
    mse = mean((trues-preds)^2),
    mae_phi = mean(phi(trues,phi.parms=pc)*(abs(trues-preds))),
    mse_phi = mean(phi(trues,phi.parms=pc)*(trues-preds)^2),
    rmse_phi = sqrt(mean(phi(trues,phi.parms=pc)*(trues-preds)^2)),
    ubaF = ubaF,
    ubaprec = ubaprec,
    ubarec = ubarec,
    MU = MU,
    NMU = NMU)
}