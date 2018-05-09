# function for obtaining the optim prediction of a regression task
source("AuxsClassPdf.R")
source("classPdf.R")

UtilOptim <- function(form, train, test,
                      type = "util", strat = "automatic", 
                      strat.parms = list(p=0.5), # p sets which error are more penalized (FP or FN)
                      control.parms, 
                      minds=NULL, maxds=NULL,
                      learner="randomForest", 
                      learner.pars=NULL,
                      predictor="predict",
                      predictor.pars= list(type="prob"),
                      eps = 0.1){
  #   inputs:
  #   form           a formula
  #   train          the train data
  #   test           the test data
  #   type           the type of surface provided. Can be: "util"(default), "cost" 
  #                  or "ben".
  #   strat
  #   strat.parms
  #   control.parms  the control.parms defined through the function phi.control
  #                  these parameters stablish the diagonal of the surface provided.
  #   m.pts         a 3-column matrix with interpolating points for the cases 
  #                 where y != \hat{y}, provided by the user. The first column
  #                 has the y value, the second column the \hat{y} value and the
  #                 third column has the corresponding utility value. The domain
  #                 boundaries of (y, \hat{y}) must be provided.
  #   minds         the lower bound of the target variable considered
  #   maxds         the upper bound of the target variable considered
  #   learner       the learning system to evaluate the conditional probability
  #                 estimation (must output probabilities)
  #   learner.pars  the learner parameters to use
  #   predictor      the prediction function to use
  #   predictor.pars the predictor parameters
  #   eps           a value for the precision considered during the pdf. 
  #
  #   output:
  #   the predictions for the test data optimized using the surface provided
  
  type <- match.arg(type, c("utility", "cost", "benefit"))
  strat <- match.arg(strat, c("interpol", "automatic")) # only interpol implemented for now
  tgt <- which(names(train) == as.character(form[[2]]))
  
  if (is.null(minds)){
    minds <- min(train[,tgt])
  }
  if(is.null(maxds)){
    maxds <- max(train[,tgt])
  }
  
  y.true <- seq(minds-0.01, maxds+0.01, by=eps)
  if(y.true[length(y.true)]!=maxds) y.true <- c(y.true, maxds)
  
  if(strat == "interpol"){
    # if (length(strat.parms) != 1){
    #   stop("strat.parms should only provide the method selected for interpolation. 
    #        No further arguments are necessary.", call. = FALSE)
    # }
    # method <- match.arg(strat.parms[[1]], c("bilinear", "splines", "idw", "krige"))
    # # UtilRes is a lxl matrix with the true utility values on the rows and the
    # # predictions on the columns, i.e., resUtil[a,b] provides the utility of
    # # predicting b for a true value a.
    # UtilRes <- UtilInterpol(NULL, NULL, type, control.parms, 
    #                         minds, maxds, m.pts, 
    #                         method = method, visual = FALSE, eps = eps,
    #                         full.output = TRUE)
    } else if(strat == "automatic"){
      if(!any(names(strat.parms) == "p")){
        stop("Automatic strategy for utility requires the setting 
             of parameter p in strat.parms argument.")
      }
      UtilRes <- matrix(nrow=length(y.true), ncol=length(y.true))
      loss.parms <- loss.control(train[,tgt])
      util.parms <- util.control(p=strat.parms$p)
      for(i in 1:length(y.true)){
        UtilRes[i,] <- util(y.true,
                            rep(y.true[i], length(y.true)),
                            control.parms,loss.parms,util.parms,return.uv = TRUE)
      }
    }
  
  resPDF <- getPDFinRange(y.true, test, train, form, learner, learner.pars,
                          predictor, predictor.pars)
  
  
  optim <- vector("numeric", length=nrow(test))
  for (ex in 1:nrow(test)){
    
    areas <- vector("numeric",length=length(y.true))
    for (case in 1:length(y.true)){
      prod <- resPDF[ex,]*UtilRes[,case]
      idx <- 2:length(y.true)
      areas[case] <- as.double((y.true[idx] - y.true[idx-1]) %*% (prod[idx] + prod[idx-1])) / 2
    }
    if(type == "utility" || type == "benefit"){
      optim[ex] <- y.true[which.max(areas)]
    } else {
      optim[ex] <- y.true[which.min(areas)]
    }
  }
  
  optim
}