parser <- function(expr)
{
  
  # add extra whitespace to brackets and operators
  
  expr <- gsub("(\\(|\\)|\\[|\\]|\\||\\&|\\!|\\=|\\.\\.|[a-zA-Z0-9_]*)"," \\1 ", expr)
  
  # strip multiple whitespace characters
  expr <- gsub("[ ]+", " ", expr) #Elimina extra espacios en blanco
  expr <- gsub("^[ ]+", "", expr) #Elimina el primer espacio en blanco
  expr <- gsub("[ ]+$", "", expr) #Elimina el ultimo espacio en blanco
  
  # split up at whitespace positions
  res <- strsplit(expr, " ", fixed=TRUE)[[1]]
  return(res)
}

SBML_regulators<-function(regulator,NOT=FALSE){
 # regulators<-c()
  regulator<-gsub("(U|MOD|ANY)_","",regulator,perl=TRUE)
  regulator<-gsub("[-\\.]","_",regulator,perl=TRUE)
  regulator<-gsub("\\[.*?]","",regulator)
  v<-c("<apply>\n","\t<eq/>\n",paste("\t<ci>",regulator,"</ci>\n",sep=""))
  type<-'\t<cn type=\"integer\">1</cn>\n'
  if(NOT==T){
    type<-'\t<cn type=\"integer\">0</cn>\n'
  }
  e<-"</apply>\n"
  end<-c(v,type,e)
  return(end)
  #regulators<-c(regulators,end)
}

nextoperator<-function(){
  k<<-k+1
  return(k)
}

parseNOT<-function(expression){
  operNOT<-c("<apply>\n","\t<not/>\n")
  regulators<-parsetoSBML(expression)
  regulators<-paste("\t",regulators,sep="")
  regulators<-c(operNOT,regulators,"</apply>\n")
  return(regulators)
}

parsetoSBML<-function(expression){
  
  operOR<-c("<apply>\n","\t<or/>\n")
  operAND<-c("<apply>\n","\t<and/>\n")
 
  NOT=F
  operators<-c()
  regulators<-c()
  k<-nextoperator()

  while(k<=length(expression)){
    s<-expression[k]
    if(s%in%c(0,1)) break
    if(s=="("){
      if(NOT==T) regulators[[length(regulators)+1]]<-parseNOT(expression)
      else regulators[[length(regulators)+1]]<-parsetoSBML(expression)
    } 
    else
    if(s=="!") NOT=T
    else if(s%in%c("|","&")){
      operators<-c(operators,s)
    }else if(s==")") break
    else{ #Nombre de nodo
      reg<-SBML_regulators(s,NOT)
      regulators[[length(regulators)+1]]<-reg
      NOT=F
    }
    k<-nextoperator()
  }
  
  join<-c()
  if(length(regulators)==1){
    regulators<-unlist(regulators)
 #   regulators<-paste("\t",regulators,sep="")
    join<-regulators
  }else if(length(regulators)>1){
    
    if(length(unique(operators))==1){  #The same operator
      regulators<-unlist(regulators)
      regulators<-paste("\t",regulators,sep="")
      if(operators[1]=="&") join<-c(operAND,regulators,"</apply>\n")
      else  join<-c(operOR,regulators,"</apply>\n")
      
    }else{                            #Different operators
      operators<-c(operators,"|")
      AND=F
      for(i in 1:length(operators)){
        if(operators[i]=="&" & AND==F){
          AND=T
          pos<-i
        }else if(operators[i]=="|"){
          if(AND==T){ #Habia ANDs
            regAND<-unlist(regulators[pos:i])
            regAND<-paste("\t",regAND,sep="") #Tabulador a los reguladores
            regAND<-c(operAND,regAND,"</apply>\n")
            regAND<-paste("\t",regAND,sep="") #Tabulador porque hay OR
            join<-c(join,regAND)
            AND=F
            pos<-c()
          }else{
            regOR<-unlist(regulators[i])
            regOR<-paste("\t",regOR,sep="") 
            join<-c(join,regOR)
          }
        }
      }
      join<-c(operOR,join,"</apply>\n")
    }
  }
  
}

#k=0
#k=0
#join<-parsetoSBML(expression)
#join<-paste(join,collapse = "")


#Write the file:
# file<-"probando.txt"
# t <- textConnection("output", encoding = "UTF-8", open = "w", local = TRUE)
# cat(file = t,join)
# close(t)
# t <- file(file, encoding = "UTF-8", open = "w")
# cat(file = t, output, sep = "\n")
# close(t)