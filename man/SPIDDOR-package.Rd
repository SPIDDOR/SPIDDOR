\name{SPIDDOR-package}
\Rdversion{1.0}
\alias{SPIDDOR-package}
\alias{SPIDDOR}
\docType{package}
\title{
\packageTitle{SPIDDOR}
}
\description{
\packageDescription{SPIDDOR}
}
\details{
SPIDDOR is an R package which consist on a set of tools to perform Boolean modeling in the context of development therapies for complex diseases. SPIDDOR allows users to simulate synchronous and asynchronous Boolean networks and analyze the results in terms of the average dynamic evolution of the nodes or in terms of attractors.

From a methodological point of view the Boolean analysis performed by SPIDDOR involves certain novelties. Common Boolean modeling approaches only define direct activation-inhibition relationships between the components of the network. In our models, we incorporate the modulation interactions, which are used to modulate the intensity of the activations or inhibitions produced by the regulator nodes. The package also allows users to specify the activity level of their nodes as a percentage ON in order to perform mutational studies or evaluate the inputs of the network with background noises. Additionally, SPIDDOR incorporates new visualization techniques to evaluate the attractors of the system and the effects of perturbations.

%\packageDESCRIPTION{SPIDDOR}
%\packageIndices{SPIDDOR}
}
\author{
Authors: \packageAuthor{SPIDDOR}

Maintainer: \packageMaintainer{SPIDDOR}
}
\references{
Irurzun-Arana I, Pastor JM, Trocóniz IF, Gómez-Mantilla JD. (2017). Advanced Boolean modeling of biological networks applied to systems pharmacology. Bioinformatics. 33 (7) doi: 10.1093/bioinformatics/btw747

Kauffman, S. A. 1969. "Metabolic Stability and Epigenesis in Randomly Constructed Genetic Nets." J. Theor. Biol 22.

Saadatpour, A. and Albert, R. (2013). Boolean modeling of biological regulatory
networks: A methodology tutorial. Methods, 62(1).

Ruiz-Cerda, M. L., Irurzun-Arana, I., Gonzalez-Garcia, I., Hu, C., Zhou, H., Vermeulen,
A., Troconiz, I., and Gomez-Mantilla, J. D. 2016. "Towards patient
stratification and treatment in the autoimmune disease lupus erythematosus using
a systems pharmacology approach". Eur. J. Pharm. Sci.
}

\keyword{ package
Boolean network
attractor
Systems Pharmacology
Simulation}

\examples{

##################################################
# Average of the dynamic trajectory of a network #
##################################################

  data("cellcycle")
  BN<-read.Boolean.functions(Lines=BN_cellcycle$BooleanFunctions)
  
  #Normal average of the dynamic trajectory of the network with asynchronous updating method:
  AVG<-Average_simulations.f(BN,time.steps=49,repetitions=2500)
  
  #Plot the results with ggplot2:
   AVG2<-toggplot(AVG)
   
   ggplot(data=AVG2,aes(x=time,y=value)) +
   geom_line( colour="#336600",size = 1.5) + ylab(" \% of activation") + 
   xlab("Time steps") +   facet_wrap(~variable)
}
