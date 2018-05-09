# auxiliary functions for conditional density estimation
# assumes the first column in the data set contains the target variable

generate_dataset <- function(thresh, train_data, cv_data, form, learner, learner.pars,
                             predictor, predictor.pars){
  
  #generate a single new training dataset
  #thresh: threshold for lower bound also used
  #        to identify the corresponding model
  #cv_data: cross validation dataset
  #starts at the right border of the first bin
  
  # learner:       the used learner
  # learner.pars:  the learner parameters
  # predictor:     the predictor to use
  # predictor.pars:the parameters of the predictor
  

  train_data[train_data[,1]<=thresh,1] <- 0;
  train_data[train_data[,1]>thresh,1] <- 1;

  train_data[,1] <- factor(train_data[,1]);
  
  if(is.null(predictor)) { # no separate prediction phase

    pred_cv <- do.call(eval(parse(text = learner)),
                       c(list(form, train_data, cv_data), learner.pars))
    
  } else {
    
    model <- do.call(eval(parse(text = learner)),
                     c(list(form, data=train_data), learner.pars))
    pred_cv <- do.call(eval(parse(text = predictor)),
                       c(list(model, cv_data), predictor.pars))
    
  }

  #only return the "0" class because the other one is just 1 - p
  if(learner == "svm"){
    return(attr(pred_cv, "probabilities")[,which(colnames(attr(pred_cv, "probabilities"))=="0")])
  } else if (learner == "earth"){
    if(attr(pred_cv, "dimnames")[[2]]=="0"){
      return(pred_cv)
    } else {
      return(1-pred_cv)
    }
  } else{
    return(pred_cv[,1]) # this is ok for rpart and randomForest
  }
}




generate_bi_probs <- function(bin_vec, train_data, cv_data, form, learner, learner.pars,
                              predictor, predictor.pars){

  #generate all bipoint problems for the cv dataset
  #using the elements of thresh_vec generate all binary problems
  #bin_vec: vector of bin sequences
  #train_data: training data
  #cv_data: crossvalidation data
  # learner:       the used learner
  # learner.pars:  the learner parameters
  # predictor:     the predictor to use
  # predictor.pars:the parameters of the predictor
  
  #RETURNS:
  #        class conditional probabilities from the cv_data


  binary_probs <- matrix(ncol = length(bin_vec) - 2, nrow = length(cv_data[,1]));
print("Begin models training")
  for(i in 2:(length(bin_vec)-1)){
    binary_probs[,i-1] <- generate_dataset(bin_vec[i], train_data, cv_data,
                                           form, learner, learner.pars,
                                           predictor, predictor.pars);
  }
  print("End models training")

  #rbord is left border from 2 to len-1  so smaller has to increase
  #which leads to isotonic regression or chernochukov rearrangement

  xleft <- 2:(length(bin_vec) - 1);

  for(i in 1:length(binary_probs[,1])){
    mon_fnkt <- isoreg(xleft, binary_probs[i,]);
    binary_probs[i,] <- mon_fnkt$yf;
  }

  #translate into class probabilities easier if smaller than
  #i can do this collectively

  binary_probs <- 1.0 - binary_probs;

  ret_probs <- matrix(ncol = length(bin_vec) - 1, nrow = length(cv_data[,1]));
  ret_probs[,1] <- 1.0 - binary_probs[,1];
  for(i in 2:(length(bin_vec) - 2) ){
      ret_probs[,i] <- binary_probs[,i - 1] - binary_probs[,i];
    }

  ret_probs[,(length(bin_vec) - 1)] <- binary_probs[,(length(bin_vec) - 2)];
  #the last element stays the same
  return(ret_probs);
}



calc_weights <- function(cv_class, numel, train_el_class){
  train_weights <- vector(length = length(train_el_class));
  for(j in 1:length(numel)){
    train_weights[train_el_class == j] <- length(train_el_class) * cv_class[j] / numel[j];
  }
  return(train_weights);
}


