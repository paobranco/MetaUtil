## ================================================================
## Metacost strategy for regression problems.
## P.Branco Jan 2018
## ================================================================
# # Examples:
# library(e1071)
# data(Boston, package = "MASS")
#
# form <- as.formula(medv~.)
#
# tgt <- which(colnames(Boston) == "medv")
# sp <- sample(1:nrow(Boston), as.integer(0.7*nrow(Boston)))
# train <- Boston[sp,]
# test <- Boston[-sp,]
#
# control.parms <- phi.control(Boston[,tgt], method="extremes", extr.type="both")
# # the boundaries of the domain considered
# minds <- min(train[,tgt])
# maxds <- max(train[,tgt])
#
# # build m.pts to include at least (minds, maxds) and (maxds, minds) points
# m.pts <- matrix(c(minds, maxds, -1, maxds, minds, -1),
#                 byrow=TRUE, ncol=3)
# 
# res <- MetacostRegress(form, train, test, type="util",
#                        strat="interpol", strat.parms=list(method="bilinear"),
#                        control.parms, m.pts, minds, maxds, m=2,
#                        learner="svm")
#
# # obtain the utility of the predictions
# MetaUtil <- UtilInterpol(test$medv, res, type="util", control.parms = control.parms,
#                            minds, maxds, m.pts, method=method)
# # train a normal model
# model <- svm(form,train)
# normal.preds <- predict(model, test)
#
# #obtain the utility of the normal predcitions
# NormalUtil <- UtilInterpol(test$medv, normal.preds, type="util",
#                            control.parms = control.parms,
#                            minds, maxds, m.pts, method=method)
# #check the performance
# eval.normal <- EvalRegressMetrics(test$medv, normal.preds, NormalUtil,
#                                   thr=0.8, control.parms = control.parms)
#
# eval.util <- EvalRegressMetrics(test$medv, res, MetaUtil,
#                                 thr=0.8, control.parms = control.parms)

MetaUtilRegress <- function(form, train, test, 
                            type = "util", strat = "interpol", 
                            strat.parms = list(method = "bilinear", control.parms, m.pts),
                            # strat= "automatic",
                            # strat.parms = list(p=0.5, control.parms, loss.parms)
                            minds=NULL, maxds=NULL, eps = 0.1,
                            m, n = NULL, q = TRUE,
                            learner = NULL, learner.pars = NULL, 
                            predictor = "predict", predictor.pars = NULL,
                            full.output = FALSE){
  # Args:
  #   form           a model formula
  #   train          the train data
  #   test           the test data
  #   type           the type of surface provided. Can be: "util"(default), "cost" 
  #                  or "ben".
  #   strat          the type of strategy used to obtain the surface
  #   strat.parms    the parameters required for the strategy selected
  #   control.parms  the control.parms defined through the function phi.control
  #                  these parameters stablish the diagonal of the surface provided.
  #   m.pts          a 3-column matrix with interpolating points for the cases 
  #                  where y != \hat{y}, provided by the user. The first column
  #                  has the y value, the second column the \hat{y} value and the
  #                  third column has the corresponding utility value. The domain
  #                  boundaries of (y, \hat{y}) must be provided.
  #   minds          the lower bound of the target variable considered
  #   maxds          the upper bound of the target variable considered
  #   eps            a value for the precision considered during the pdf estimation. 
  #   m              number of resamples to generate
  #   n              number of examples in each resample. If set to NULL 
  #                  (the default) n is set to the training size
  #   q              logical indicating if all resamples are to be used for 
  #                  each example (defaults to TRUE)
  #   learner        the learning algorithm to use
  #   learner.pars   the parameters passed through the learning algorithm
  #   predictor      the predictor used
  #   predictor.pars the predictor parameters 
  #   full.output    if FALSE only the predictions are returned. When set to TRUE
  #                  returns a named list with the predictions (res), 
  #                  the model (model) and the modified training set (newTrain)
  #
  # Returns: the predictions on the test set using the model obtained through
  #          metacost algorithm (when full.output is FALSE). Returns the predictions,
  #          the model and the modified training set when full.output is TRUE.
  
  
  tgt <- which(names(train) == as.character(form[[2]]))
  
  if(!is.numeric(train[1,tgt])) stop("This function only deals with regression tasks.",
                                     call. = FALSE)
  
  type <- match.arg(type, c("utility", "cost", "benefit"))
  strat <- match.arg(strat, c("interpol", "auto"))
  if(is.null(m)) stop("The parameter m must be set.", call. = FALSE)
  if(is.null(n)) n <- nrow(train)
  
  if(is.null(minds)) minds <- min(train[,tgt])
  if(is.null(maxds)) maxds <- max(train[,tgt])
  if(!q){
    spUsed <- matrix(NA, nrow=n, ncol=m)
  }
  predsM <- list()
  if (strat == "interpol"){
    if (length(strat.parms) != 3){
      stop("strat.parms should provide 3 parameters:
          the method selected for interpolation, the control.parms and the m.pts.",
           call. = FALSE)
    }
    method <- match.arg(strat.parms$method, c("bilinear", "splines", "idw", "krige"))
    control.parms <- strat.parms$control.parms
    m.pts <- strat.parms$m.pts
    # UtilRes is a lxl matrix with the true utility values on the rows and the
    # predictions on the columns, i.e., resUtil[a,b] provides the utility of
    # predicting b for a true value a.
    
    UtilRes <- UtilInterpol(NULL, NULL, type, control.parms, 
                            minds, maxds, m.pts, 
                            method = method, visual = FALSE, eps = eps,
                            full.output = TRUE)
    
    
    y.true <- seq(minds-0.01, maxds+0.01, by=eps)
    if(y.true[length(y.true)]!=maxds) y.true <- c(y.true, maxds)
    
    # generate m models and obtain the m set of predictions
    for(i in 1:m){
      sp <- sample(1:nrow(train), n-2, replace=TRUE)
      # need to ensure that at least the domain extremes are in the training sample
      sp <- c(sp, which.min(train[,tgt]), which.max(train[,tgt]))
      
      # learn with the bootstrap sample drawn
      if(q){
        predsM[[i]] <- getPDFinRange(y.true, train, train[sp,], form)
      } else {
        spUsed[,i] <- sp
        predsM[[i]] <- getPDFinRange(y.true, train, train[sp,], form)
      }
    }
    } else if (strat == "auto"){
      
      if (length(strat.parms) != 3){
        stop("strat.parms should provide 3 parameters:
              the p value selected for penalizing FP and FN, the control.parms and the loss.parms.",
             call. = FALSE)
      }
      y.true <- seq(minds-0.01, maxds+0.01, by=eps)
      if(y.true[length(y.true)]!=maxds) y.true <- c(y.true, maxds)
      
      # UtilRes is a lxl matrix with the true utility values on the rows and the
      # predictions on the columns, i.e., resUtil[a,b] provides the utility of
      # predicting b for a true value a.

      
      UtilRes <- matrix(nrow=length(y.true), ncol=length(y.true))
      
      control.parms <- strat.parms$control.parms
      loss.parms <- strat.parms$loss.parms
      util.parms <- util.control(p=strat.parms$p)
      
      for(i in 1:length(y.true)){
        UtilRes[i,] <- util(y.true,
                            rep(y.true[i], length(y.true)),
                            control.parms,loss.parms,util.parms,return.uv = TRUE)
      }
      
      
      # generate m models and obtain the m set of predictions
      for(i in 1:m){
        sp <- sample(1:nrow(train), n-2, replace=TRUE)
        # need to ensure that at least the domain extremes are in the training sample
        sp <- c(sp, which.min(train[,tgt]), which.max(train[,tgt]))
        
        # learn with the bootstrap sample drawn
        if(q){
          predsM[[i]] <- getPDFinRange(y.true, train, train[sp,], form,
                                       learner, learner.pars,
                                       predictor, predictor.pars)
        } else {
          spUsed[,i] <- sp
          predsM[[i]] <- getPDFinRange(y.true, train, train[sp,], form,
                                       learner, learner.pars,
                                       predictor, predictor.pars)
        }
      }
      
    }
  
  if(!q){
    # auxSP is TRUE when the example in the row can be used for the model in the column,
    # i.e., that example was not used for training the model in that column
    auxSP <- matrix(NA, nrow = nrow(train), ncol = m)
    
    auxSP <- t(sapply(1:nrow(train), 
                      function(x) {
                        apply(spUsed,2,function(a) !(x %in% a))
                      }
    ))
    new.m <- apply(auxSP, 1, function(x) length(which(x))) 
  }
  
  trainProbs <- matrix(0, nrow=nrow(train), ncol=length(y.true))
  if(q){
    trainProbs <- Reduce("+", predsM)/m
  } else { # select only the models that did not used the example for training 
    searchInd <- sapply(1:nrow(train), function(x) {which(auxSP[x,])})
    for(ex in 1:nrow(train)){
      for(se in searchInd[[ex]]){
        trainProbs[ex,] <- trainProbs[ex,]+predsM[[se]][ex,]/new.m[ex]
      }
    }
    ind.NAN <- which(new.m == 0)
    if(length(ind.NAN)){
      trainProbs[ind.NAN,] <- (Reduce("+", predsM)/m)[ind.NAN,]
    }
  } 
  
  # change the target variable values of the training set using the previously
  # estimated probabilities
  
  optim <- vector("numeric", length=nrow(train))
  for (ex in 1:nrow(train)){
    areas <- vector("numeric",length=length(y.true))
    for (case in 1:length(y.true)){
      prod <- trainProbs[ex,]*UtilRes[,case]
      idx <- 2:length(y.true)
      areas[case] <- as.double((y.true[idx] - y.true[idx-1]) %*% (prod[idx] + prod[idx-1])) / 2
    }
    if(type == "utility" || type == "benefit"){
      optim[ex] <- y.true[which.max(areas)]
    } else {
      optim[ex] <- y.true[which.min(areas)]
    }
  }
  
  
  newTrain <- train
  newTrain[,tgt] <- optim
  
  # training set is now pre-processed
  # learn and predict with the new training set
  model <- do.call(eval(parse(text = learner)),
                   c(list(form, data = newTrain), learner.pars))
  
  predictorR <- "predict"
  predictor.parsR <- NULL
  res <- do.call(eval(parse(text = predictorR)),
                 c(list(model, test), predictor.parsR))
  
  list(res=res, model = model, newTrain=newTrain)
  if(full.output){
    return(list(res=res, model = model, newTrain=newTrain)) 
  } else {
    return(res)
  }
  
}