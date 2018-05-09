library(UBL)
library(uba)
library(performanceEstimation)
library(e1071)         # for the svm
library(randomForest)  # randomForest

source("Auxs.R")
source("MetaUtil.R")
source("classPdf.R")
source("AuxsClassPdf.R")

##########################################################################
# load the datasets
##########################################################################

load("AllDataSets.RData")



#########################################################################
# to generate information about the data sets for a given threshold
#########################################################################
PCSall <- list()

for(d in 1:16)
{
  y <- resp(DSs[[d]]@formula,DSs[[d]]@data)
  pc <- phi.control(y, method="extremes")
  lossF.args <- loss.control(y)
  PCSall[[d]] <- list(pc, lossF.args)
}

thr.rel <- 0.8
for(d in 1:16){
  form <- DSs[[d]]@formula
  data <- DSs[[d]]@data
  y <- resp(form,data)
  pc <- list()
  pc$method <- PCSall[[d]][[1]][[1]]
  pc$npts <- PCSall[[d]][[1]][[2]]
  pc$control.pts <- PCSall[[d]][[1]][[3]]
  both <- all(pc$control.pts[c(2,8)] == c(1,1))
  y.relev <- phi(y,pc)
  total <- 0
  if (both) {  # we have both low and high extrs
    rare.low <- which(y.relev > thr.rel & y < pc$control.pts[4])
    rare.high <- which(y.relev > thr.rel & y > pc$control.pts[4])
    rare.cases <- c(rare.low,rare.high)
    total <- length(rare.cases)
  } else {
    # the indexes of the cases with rare target variable values
    rare.cases <- if (pc$control.pts[2] == 1)  which(y.relev > thr.rel & y < pc$control.pts[4]) else which(y.relev > thr.rel & y > pc$control.pts[4])
    total <- length(rare.cases)
  }
  
  if(both){
    b <- 2
  }else {b <- 1 } 
}

# data set 4 and 6 (a4 and a6) have a bad formed control.pts entry which must be corrected

PCSall[[4]][[1]][[2]] <- 2
PCSall[[6]][[1]][[2]] <- 2

PCSall[[4]][[1]][[3]] <- PCSall[[4]][[1]][[3]][4:9]
PCSall[[6]][[1]][[3]] <- PCSall[[6]][[1]][[3]][4:9]


PCS <- list(PCSall[[1]], PCSall[[2]],PCSall[[3]], PCSall[[4]],PCSall[[5]],
            PCSall[[6]],PCSall[[7]],PCSall[[8]], PCSall[[9]], PCSall[[10]], 
            PCSall[[11]], PCSall[[12]], PCSall[[13]], PCSall[[14]], PCSall[[15]],
            PCSall[[16]])

myDSs <- list(PredTask(a1~., DSs[[1]]@data, "a1"), PredTask(a2~., DSs[[2]]@data, "a2"),
              PredTask(a3~., DSs[[3]]@data, "a3"), PredTask(a4~., DSs[[4]]@data, "a4"),
              PredTask(a5~., DSs[[5]]@data, "a5"), PredTask(a6~., DSs[[6]]@data, "a6"),
              PredTask(a7~., DSs[[7]]@data, "a7"),
              PredTask(Rings~., DSs[[8]]@data, "Abalone"),
              PredTask(acceleration~., DSs[[9]]@data, "acceleration"),
              PredTask(available.power~., DSs[[10]]@data, "availPwr"),
              PredTask(rej~., DSs[[11]]@data, "bank8FM"), 
              PredTask(fuel.consumption.country~., DSs[[12]]@data, "fuelCons"),
              PredTask(HousValue~., DSs[[13]]@data, "boston"),
              PredTask(class~.,DSs[[14]]@data, "machineCpu"),
              PredTask(class~.,DSs[[15]]@data, "servo"),
              PredTask(ScaledSoundPressure~.,DSs[[16]]@data, "airfoild")
)

# weight for penalizing FP ot FN
p <- 1
##########################################################################
# learners and estimation procedure
##########################################################################

WFs <- list()

WFs$svm <- list(learner.pars=list(cost=c(10,150), gamma=c(0.01,0.001), probability=TRUE))

WFs$randomForest <- list(learner.pars=list(mtry=c(5,7),ntree=c(500,750,1500)))


# exps with 2 times 10 fold CV
source("CVestimation.R")
##########################################################################
# exps
##########################################################################

for(d in c(1:9, 11:13, 15:16)){
  for(w in names(WFs)) {
    resObj <- paste(myDSs[[d]]@taskName,w,'Res',sep='')
    assign(resObj,
           try(
             res <-performanceEstimation(
               myDSs[d],         
               c(do.call('workflowVariants',
                         c(list('WFMetaUtil',
                                method = PCSall[[d]][[1]][[1]],
                                npts = PCSall[[d]][[1]][[2]],
                                control.parms=PCSall[[d]][[1]][[3]],
                                #loss.parms=PCSall[[d]][[2]],
                                ymin=PCSall[[d]][[2]][[1]],
                                ymax=PCSall[[d]][[2]][[2]],
                                tloss=PCSall[[d]][[2]][[3]],
                                epsilon=PCSall[[d]][[2]][[4]],
                                strat.parms=list(p=1), learner=w),
                           WFs[[w]],
                           varsRootName=paste('WFMetaUtil',w,sep='.'),
                           as.is="control.parms"
                         ))
               ),
               CVsetts[[d]])
           )
    )
    if (class(get(resObj)) != 'try-error') save(list=resObj,file=paste(myDSs[[d]]@taskName,w,'Rdata',sep='.'))
  }
}


