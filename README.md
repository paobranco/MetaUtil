## MetaUtil: Meta Learning for Utility Maximization in Regression Tasks

This repository has all the code used in the experiments carried out in the paper *"MetaUtil: Meta Learning for Utility Maximization in Regression"* [1].


This repository is organized as follows:

* **Code** folder - contains all the code for reproducing the experiments presented in the paper;
* **Data** folder - contains the 16 regression data sets used in the experiments carried out;


### Requirements

The experimental design was implemented in R language. Both code and data are in a format suitable for R environment.

In order to replicate these experiments you will need a working installation
  of R. Check [https://www.r-project.org/] if you need to download and install it.

In your R installation you also need to install the following additional R packages:

  - randomForest
  - e1071
  - performaceEstimation
  - UBL
  - uba


  All the above packages with the exception of uba, can be installed from CRAN Repository directly as any "normal" R package. Essentially you need to issue the following command within R:

```r
install.packages(c("randomForest", "e1071", "performanceEstimation", "UBL"))
```

The package uba needs to be installed from a tar.gz file that you
  can download from http://www.dcc.fc.up.pt/~rpribeiro/uba/.
  Download the tar.gz file into your folder and then issue:

```r
install.packages("uba_0.7.8.tar.gz",repos=NULL,dependencies=T)
```

Check the other README files in each folder to see more detailed instructions on how to run the experiments.

*****

### References
[1] Branco, P. and Torgo, L. and Ribeiro, R.P. (2018, October) "MetaUtil: Meta Learning for Utility Maximization in Regression" In *International Conference on Discovery Science* (pp. ---). Springer, Berlin, Heidelberg.
