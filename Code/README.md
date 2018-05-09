# Code

This folder contains the R code necessary for replicating the paper experiments.

## Contents

* **Orig** folder - includes all the code for the experiments regarding the baseline regression learners;
* **UtilOptim** folder - includes all the code for the experiments regarding the algorithm for maximizing utility proposed in Branco et al. (2017).
* **MetaUtil** folder - includes all the code for the experiments regarding the MetaUtil algorithm for maximizing utility proposed in this paper.


## Running the experiments

In order to run the experiments you must:

- have a working installation of R; 
- ensure the necessary R packages are installed;
- load the data sets available in the **data** folder. To do this you execute R in the folder with the data and then issue the command:

```r
source("AllDataSets.RData")
```

### Running the original learners

Change the working directory to the **Orig** folder issuing the command:

```r
setwd("<you-directory-tree>/Orig")
```

The experiences can be run for utility surfaces with different characteristics. The only parameter used in the paper for obtaining different utility surfaces was p. In the paper the value of p was selected in $\{0.2, 0.5, 0.8, 1\}$.

To run the experiments, it is only necessary to issue the following command:
```r
source("expsBaseLearners.R")
```
Alternatively, you may run the experiments directly from a Linux terminal as follows:
```bash
nohup R --vanilla --quiet < expsBaseLearners.R &

```

This allows to obtain the results for all the data sets on the original regression algorithms. The value of p used is set to 1 in this file. In order to change it to a different value you only need to change the line in the file setting this value to the new desired value.

Just change the line containing
```r
p <- 1
```

to, for instance,
```r
p <- 0.2
```

### Running the Utility maximization algorithm (UtilOptim)


Change the working directory to the **UtilOptim** folder issuing the command:

```r
setwd("<you-directory-tree>/UtilOptim")
```

The experiences can be run for utility surfaces with different characteristics. The only parameter used in the paper for obtaining different utility surfaces was p. In the paper the value of p was selected in $\{0.2, 0.5, 0.8, 1\}$.

To run the experiments, it is only necessary to issue the following command:
```r
source("expsUtilOptimR.R")
```
Alternatively, you may run the experiments directly from a Linux terminal as follows:
```bash
nohup R --vanilla --quiet < expsUtilOptimR.R &

```

This allows to obtain the results for all the data sets using the proposed algorithm for utility maximization using the randomForest and the svm as classifiers for obtaining the conditional density estimation.

The value of p used is set to 1 in this file. In order to change it to a different value you only need to change the line in the file setting this value to the new desired value.

Just change the line containing
```r
p <- 1
```

to, for instance,
```r
p <- 0.2
```

### Running the MetaUtil algorithm (MetaUtil)


Change the working directory to the **MetaUtil** folder issuing the command:

```r
setwd("<you-directory-tree>/MetaUtil")
```

The experiences can be run for utility surfaces with different characteristics. The only parameter used in the paper for obtaining different utility surfaces was p. In the paper the value of p was selected in $\{0.2, 0.5, 0.8, 1\}$.

To run the experiments, it is only necessary to issue the following command:
```r
source("expsMetaUtil.R")
```
Alternatively, you may run the experiments directly from a Linux terminal as follows:
```bash
nohup R --vanilla --quiet < expsMetaUtil.R &

```

This allows to obtain the results for all the data sets using the proposed algorithm for utility maximization using the randomForest and the svm as classifiers for obtaining the conditional density estimation.

The value of p used is set to 1 in this file. In order to change it to a different value you only need to change the line in the file setting this value to the new desired value.

Just change the line containing
```r
p <- 1
```

to, for instance,
```r
p <- 0.2
```

