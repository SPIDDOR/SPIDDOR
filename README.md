# SPIDDOR
Modeling, simulation and analysis of Boolean networks applied to Systems Pharmacology.

## To install SPIDDOR:  
```{r}
install.packages("devtools")  
library(devtools)  
install_github("SPIDDOR/SPIDDOR") 
```
If you use a proxy server you need to install httr package and then:
```{r}
library(httr)
#Set your proxy here:
set_config(use_proxy("10.10.10",8080))
install_github("SPIDDOR/SPIDDOR") 
```
Additionally, a C/C++ compiler is needed to perform faster simulations with SPIDDOR. Follow the instructions below depending on your operating system. 
## Windows:  
For Windows users, Rtools is needed  to compile the simulation algorithm in C++.
It can be downloaded from https://cran.r-project.org/bin/windows/Rtools/ .   
Select the Rtools download compatible with your R version. We recommend that users use the latest release of Rtools with the latest release of R. Be aware of the path where Rtools or RBuildTools is installed and use the Connect2Rtools function each time you start a new R session in order to connect the package with Rtools gcc compiler (only for Windows users). Example:  
```{r}
library(SPIDDOR)  
Connect2Rtools(path="C:\\Rtools")
```
## Mac OS X:
If you are using a Mac, you will need to start by making sure you have Xcode + developer tools installed. You can do this in one of two ways. Either:

1- Download and install XCode from the Mac AppStore: http://itunes.apple.com/us/app/xcode/id497799835?mt=12  
2- Within XCode go to Preferences : Downloads and install the Command Line Tools

However, XCode.app is a huge application that you do not need unless you are developing OS X or iOS applications. Alternatively (for a smaller download size) you can:

1- Register as an Apple Developer here (free): https://developer.apple.com/programs/register/  
2- Download the Command Line Tools for XCode appropriate for the version of OS X you are running from here: https://developer.apple.com/downloads/

Second, you need to tell R which compilers to use. To do that, you need to create a file in the folder where R is installed in your computer named Makevars: ~/.R/Makevars. Then write the following lines with your favourite text editor:  

CC=clang  
CXX=clang++  

Otherwise, you can do this with the following commands in the Terminal (writing your true directory to the R folder):  

cd ~/.R  
nano Makevars  

Add the text:  

CC=clang  
CXX=clang++  

And follow the directions at the bottom of the screen to "write out" and close the file. (Control-O Enter and Control-X.)  

Now you need to install Rcpp again from source so it is built with the same C++ compiler you are using. In R:
```{r}
install.packages("Rcpp", type = "source")
```
## Linux: 
For Debian/Ubuntu, you can install the core software development utilities required for R package development by executing:  

sudo apt-get install r-base-dev texlive-full  

Some packages may require installation of additional R build dependencies. To provide all components needed to build R itself from source you can execute:  

sudo apt-get build-dep r-base-core  

## Final Test
A good test to check that everything is set up correctly is to use the devtools package. In R:
```{r}
install.packages("devtools")
devtools::has_devel()
```
If returns TRUE, you are good to go with SPIDDOR.
