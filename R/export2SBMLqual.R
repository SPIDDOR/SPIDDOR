
#' @export
export2SBMLqual<-function(inputfile=NULL,Lines=NULL,file="output.sbml"){
  SBMLcode<-NULL
  k<-NULL
  if(length(Lines)==0) Lines <- readLines(inputfile, -1)
  Lines <- gsub("#.*", "", Lines)
  Lines <- Lines[nchar(Lines) > 0]
#  header <- Lines[1]
#  Lines <- Lines[-1] #Eliminar el header
  
  #Get node names:
  nodes<-(lapply(Lines,function(x){
    constant<-c()
    c<-strsplit(x, split = "=")[[1]]
    output<-c[1]
    output<-gsub(" ","",output,perl=TRUE)
    output<-gsub("[-\\.\\:]","_",output,perl=TRUE)
    output<-gsub("[+=*]","",output,perl=TRUE)
    c[2]<-gsub(" ","",c[2],perl=TRUE)
    if(c[2]%in%c("1","0")) constant=c[2]
    c(output,constant)
  }))
  
  regulators<-unlist(lapply(Lines,function(x){
    c<-strsplit(x, split = "=")[[1]]
    c[2]<-gsub(" ","",c[2],perl=TRUE)
    reg<-strsplit(c[2], split = "[&\\!\\|\\(\\)]",perl=TRUE)[[1]]
    reg<-reg[!(reg%in%"")]
    reg<-gsub("(THR|MOD|ANY)_","",reg,perl=TRUE)#ignore.case=T)
    reg<-gsub("[-\\.\\:]","_",reg,perl=TRUE)
    reg<-gsub("[+=*]","",reg,perl=TRUE)
    reg<-gsub("\\[.*?]","",reg)
  }))
  regulators<-unique(regulators)
  
  #Model name:
  model <- sub(".sbml", "", basename(file), fixed = TRUE)
  model <- gsub("[^a-zA-Z0-9_]+", "_", model)
  
  t <- textConnection("SBMLcode", encoding = "UTF-8", open = "w", local = TRUE)
  
  cat(file = t, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
  cat(file = t, "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" level=\"3\" version=\"1\" xmlns:qual=\"http://www.sbml.org/sbml/level3/version1/qual/version1\" qual:required=\"true\">\n")
  cat(file = t, "\t<model id=\"", model, "\">\n", sep = "")
  cat(file = t, "\t\t<listOfCompartments>\n")
  cat(file = t, "\t\t\t<compartment id=\"default\" constant=\"true\"/>\n")
  cat(file = t, "\t\t</listOfCompartments>\n")
  cat(file = t, "\t\t<qual:listOfQualitativeSpecies>\n")
  
  nodes.names<-c()
  for (node in nodes) {
    node<-gsub(" ","",node,perl=TRUE)
    if(length(node)>1){
      cat(file = t, "\t\t\t<qual:qualitativeSpecies qual:compartment=\"default\"", 
          " qual:constant=\"true\" qual:id=\"", node[1], "\" qual:name=\"", 
          node[1],"\" qual:initialLevel=\"",node[2],"\" qual:maxLevel=\"1\"/>\n", sep = "")
      next()
    }
    cat(file = t, "\t\t\t<qual:qualitativeSpecies qual:compartment=\"default\"", 
        " qual:constant=\"false\" qual:id=\"", node, "\" qual:name=\"", 
        node, "\" qual:maxLevel=\"1\"/>\n", sep = "")
    nodes.names<-c(nodes.names,node)
  }
  
  if(length(setdiff(regulators,nodes.names))>0){
    for(node in setdiff(regulators,nodes.names)){
      cat(file = t, "\t\t\t<qual:qualitativeSpecies qual:compartment=\"default\"", 
          " qual:constant=\"false\" qual:id=\"", node, "\" qual:name=\"", 
          node, "\" qual:maxLevel=\"1\"/>\n", sep = "")
      Lines<-c(Lines,paste(node,"=",node))
      nodes.names<-c(nodes.names,node)
    }
  }
  
  cat(file = t, "\t\t</qual:listOfQualitativeSpecies>\n")
  cat(file = t, "\t\t<qual:listOfTransitions>\n")
  
  for(j in 1:length(Lines)){
    c<-strsplit(Lines[j], split = "=")[[1]]
    if(length(c)>2){
      stop(paste("BF",j,"incorrectly written"))
    } 
    output<-c[1]
    output<-gsub(" ","",output,perl=TRUE) #Eliminar espacios en blanco del output
    output<-gsub("[-\\.\\:]","_",output,perl=TRUE) #Si hay guiones o puntos que se cambien por barrabaja
    output<-gsub("[+=*]","",output,perl=TRUE)
    if(! output %in% nodes.names) next()
    cat(file = t, "\t\t\t<qual:transition qual:id=\"tr_", 
        output, "\" qual:name=\"Interactions targeting ", 
        output, "\">\n", sep = "")
    cat(file = t, "\t\t\t\t<qual:listOfInputs>\n")
    #Regulators:
    c[2]<-gsub(" ","",c[2],perl=TRUE)
    c[2]<-gsub("(THR|MOD|ANY)_","",c[2],perl=TRUE)#ignore.case=T)
    c[2]<-gsub("[-\\.\\:]","_",c[2],perl=TRUE)
    c[2]<-gsub("[+=*]","",c[2],perl=TRUE)
    c[2]<-gsub("\\[.*?]","",c[2])
    #inputs:
    reg<-strsplit(c[2], split = "[&\\!\\|\\(\\)]",perl=TRUE)[[1]]
    reg<-reg[!(reg%in%"")]
    reg<-unique(reg)
    for (input in reg) cat(file = t, "\t\t\t\t\t<qual:input qual:qualitativeSpecies=\"", 
                           input, "\" qual:transitionEffect=\"none\"/>\n", 
                           sep = "")
    #outputs:
    cat(file = t, "\t\t\t\t</qual:listOfInputs>\n")
    cat(file = t, "\t\t\t\t<qual:listOfOutputs>\n")
    cat(file = t, "\t\t\t\t\t<qual:output qual:qualitativeSpecies=\"", 
        output, "\" qual:transitionEffect=\"assignmentLevel\"/>\n", 
        sep = "")
    cat(file = t, "\t\t\t\t</qual:listOfOutputs>\n")
    
    #boolean functions
    cat(file = t, "\t\t\t\t<qual:listOfFunctionTerms>\n")
    cat(file = t, "\t\t\t\t\t<qual:functionTerm qual:resultLevel=\"1\">\n")
    cat(file = t, "\t\t\t\t\t\t<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n")
    tab<-"\t\t\t\t\t\t\t"
    #parsetoSBML
    k<<-0
    expression<-parser(c[2])
    parsedBF<-parsetoSBML(expression)
    parsedBF<-paste(tab,parsedBF,sep="")
    cat(file = t, parsedBF)
    
    cat(file = t, "\t\t\t\t\t\t</math>\n")
    cat(file = t, "\t\t\t\t\t</qual:functionTerm>\n")
    cat(file = t, "\t\t\t\t\t<qual:defaultTerm qual:resultLevel=\"0\"/>\n")
    cat(file = t, "\t\t\t\t</qual:listOfFunctionTerms>\n")
    cat(file = t, "\t\t\t</qual:transition>\n")
    
  }
  
  cat(file = t, "\t\t</qual:listOfTransitions>\n")
  cat(file = t, "\t</model>\n")
  cat(file = t, "</sbml>\n")
  close(t)
  t <- file(file, encoding = "UTF-8", open = "w")
  cat(file = t, SBMLcode, sep = "\n")
  close(t)
  
}

#Examples:
# inputfile<-"Example_networks/Cardiac_network.txt"

# outputfile<-"Cardiac_network.sbml"
# source('parser.R')
# export2SBMLqual(inputfile,outputfile)
