# SPIDDOR
Modeling, simulation and analysis of Boolean networks applied to Systems Pharmacology.

## To install SPIDDOR:  
```{r}
install.packages("devtools")  
library(devtools)  
install_github("SPIDDOR/SPIDDOR") 
```
  
Additionally, Rtools is needed to use the simulation algorithm in C++.  
## To install Rtools:  
```{r}
install.packages("installr")  
library(installr)  
install.Rtools() 
```
  
If you have the most recent version of R, you should select the most recent Rtools download (at the top).  
Be aware of the path where Rtools or RBuildTools is installed and use the Connect2Rtools function each time you start a new R session in order to connect the package with Rtools gcc compiler (only for Windows users). Example:  
```{r}
library(SPIDDOR)  
Connect2Rtools(path="C:\\Rtools")
```

