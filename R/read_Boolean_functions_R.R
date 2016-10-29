

read.Boolean.functions.R<-function(file=NULL,Lines=NULL){
  
  
  if(length(Lines)==0) Lines<- readLines(file, -1)
  Lines <- gsub("#.*", "", Lines) #remove comments
  Lines <- Lines[nchar(Lines) > 0]

  #Create .R:
  write("","dynamic_evolution.R")

  nodes<-unlist(lapply(Lines,function(x){
    c<-strsplit(x, split = "=")[[1]]
    output<-c[1]
    output<-gsub(" ","",output,perl=TRUE)
    output<-gsub("[-\\.\\:]","_",output,perl=TRUE)
    output<-gsub("[+=*]","",output,perl=TRUE)
  }))
  
  if(any(duplicated(nodes))) stop(paste("Node",nodes[duplicated(nodes)],"has more than one Boolean function"))
  
  regulators<-unlist(lapply(Lines,function(x){
    c<-strsplit(x, split = "=")[[1]]
    c[2]<-gsub(" ","",c[2],perl=TRUE)
    reg<-strsplit(c[2], split = "[&\\!\\|\\(\\)]",perl=TRUE)[[1]]
    reg<-reg[!(reg%in%"")]
    reg<-reg[!reg%in%c(0,1)]
    reg<-gsub("(U|MOD|ANY)_","",reg,perl=TRUE)#ignore.case=T)
    reg<-gsub("[-\\.\\:]","_",reg,perl=TRUE)
    reg<-gsub("[+=*]","",reg,perl=TRUE)
    reg<-gsub("\\[.*?]","",reg)
  }))
  regulators<-unique(regulators)
  
  node.names<-nodes
  if(length(setdiff(regulators,nodes))>0){
    for(node in setdiff(regulators,nodes)){
      Lines<-c(Lines,paste(node,"=",node))
      node.names<-c(node.names,node)
    }
  }
  

  arguments<-c()
  arguments2<-c()
  fun_header<-c()
  Initial_conditions<-c()
  modulator_duration<-c()
  U_duration<-c()
  
  for(j in 1:length(Lines)){
    c<-strsplit(Lines[j], split = "=")[[1]]
    if(length(c)>2){
      stop(paste("BF",j,"incorrectly written"))
    } 
    output<-node.names[j]
    
    #Regulators:
    c[2]<-gsub(" ","",c[2],perl=TRUE) #Quitar todos los espacios
    
    #Comprobar si el numero de parentesis es el correcto: da un error
    if(((gregexpr("[(\\)]",c[2])[[1]][1])!=-1) & ((sapply(gregexpr("[(\\)]",c[2]),length)%%2)!=0) ){
      stop(paste("The brakets of the BF are wrong for",output))
    }
    chars <- strsplit(c[2], split = "")[[1]]
    #que operadores tenemos:
    z<-gregexpr("[&\\!\\|\\(\\)]",c[2],perl=TRUE)[[1]]
    operators<-chars[z]
    if(z[[1]]==-1) operators<-""
    if(z[1]==1 & (operators[1] %in% c("&","|"))){ #La funcion booleana no puede empezar por & o |
      stop(paste("The Boolean function is incorrently written for",output))
    }

    reg<-strsplit(c[2], split = "[&\\!\\|\\(\\)]",perl=TRUE)[[1]]
    
    upreg_dur<-c()
    regulators<-c()
    Us<-c()
    o=1
    for(i in 1:length(reg)){
      reg[i]<-gsub("[-\\.\\:]","_",reg[i],perl=TRUE)
      reg[i]<-gsub("[+=*]","",reg[i],perl=TRUE)
      
      if(reg[i]==""){
        regulators<-paste(regulators,as.character(operators[o]),sep=" ")
        o=o+1
        next
      }

      if(reg[i]==output & length(reg[!reg%in%""])==1)  Initial_conditions<-c(Initial_conditions,output)
      #Hay U?
      if(grepl("U_",reg[i],perl=TRUE)){
        reg[i]<-gsub("U_","",reg[i],perl=TRUE)
        if(gregexpr("\\[", reg[i])!=-1){
          if(suppressWarnings(is.na(as.numeric(gsub("\\[|\\]", "", regmatches(reg[i], gregexpr("\\[.*?\\]", reg[i]))[[1]]))))){
            U_duration<-c(U_duration,3)
            Us<-c(Us,paste(",",gsub("\\[|\\]", "", regmatches(reg[i], gregexpr("\\[.*?\\]", reg[i]))[[1]])))
            R<-paste("\n\tlastn.f('",gsub("\\[.*?]","",reg[i]),"',time,pattern.m,update.m,asynchronous,",
                     gsub("\\[|\\]", "", regmatches(reg[i], gregexpr("\\[.*?\\]", reg[i]))[[1]]),")",sep="")
            reg[i]<-gsub("\\[.*?]","",reg[i])
          }else{
            U_duration<-c(U_duration,as.numeric(gsub("\\[|\\]", "", regmatches(reg[i], gregexpr("\\[.*?\\]", reg[i]))[[1]])))
            reg[i]<-gsub("\\[.*?]","",reg[i])
            Us<-c(Us,paste(",",paste(reg[i],"max_",output,sep=""),sep=""))
            R<-paste("\n\tlastn.f('",reg[i],"',time,pattern.m,update.m,asynchronous,",
                     reg[i],"max_",output,")",sep="")
          }
        }else{
          U_duration<-c(U_duration,3)
          Us<-c(Us,paste(",",paste(reg[i],"max_",output,sep=""),sep=""))
          R<-paste("\n\tlastn.f('",reg[i],"',time,pattern.m,update.m,asynchronous,",
                   reg[i],"max_",output,")",sep="")
        }
        
      }else if(grepl("MOD_",reg[i],perl=TRUE)){
        reg[i]<-gsub("MOD_","",reg[i],perl=TRUE)
        if(gregexpr("\\[", reg[i])!=-1){
          if(suppressWarnings(is.na(as.numeric(gsub("\\[|\\]", "", regmatches(reg[i], gregexpr("\\[.*?\\]", reg[i]))[[1]]))))){
            modulator_duration<-c(modulator_duration,3)
            upreg_dur<-c(upreg_dur,paste(",",gsub("\\[|\\]", "", regmatches(reg[i], gregexpr("\\[.*?\\]", reg[i]))[[1]])))
            R<-paste("\n\tlastn.f('",gsub("\\[.*?]","",reg[i]),"',time,pattern.m,update.m,asynchronous,",gsub("\\[|\\]", "", regmatches(reg[i], gregexpr("\\[.*?\\]", reg[i]))[[1]]),")",sep="")
          }else{
            modulator_duration<-c(modulator_duration,as.numeric(gsub("\\[|\\]", "", regmatches(reg[i], gregexpr("\\[.*?\\]", reg[i]))[[1]])))
            upreg_dur<-c(upreg_dur,paste(",MOD_",output,sep=""))
            R<-paste("\n\tlastn.f('",gsub("\\[.*?]","",reg[i]),"',time,pattern.m,update.m,asynchronous,",paste("MOD_",output,sep=""),")",sep="")
          }
          reg[i]<-gsub("\\[.*?]","",reg[i])
        }else{
          modulator_duration<-c(modulator_duration,3)
          upreg_dur<-c(upreg_dur,",modulator_dur")
          R<-paste("\n\tlastn.f('",reg[i],"',time,pattern.m,update.m,asynchronous,modulator_dur)",sep="")
        }
      
      }else if(grepl("ANY_",reg[i],perl=TRUE)){
        reg[i]<-gsub("ANY_","",reg[i],perl=TRUE)
        
        if(gregexpr("\\[", reg[i])!=-1){
          if(suppressWarnings(is.na(as.numeric(gsub("\\[|\\]", "", regmatches(reg[i], gregexpr("\\[.*?\\]", reg[i]))[[1]]))))){
            modulator_duration<-c(modulator_duration,3)
            upreg_dur<-c(upreg_dur,paste(",",gsub("\\[|\\]", "", regmatches(reg[i], gregexpr("\\[.*?\\]", reg[i]))[[1]])))
            R<-paste("\n\tlastns.f('",gsub("\\[.*?]","",reg[i]),"',time,pattern.m,update.m,asynchronous,",gsub("\\[|\\]", "", regmatches(reg[i], gregexpr("\\[.*?\\]", reg[i]))[[1]]),")",sep="")
            reg[i]<-gsub("\\[.*?]","",reg[i])
          }else{
            modulator_duration<-c(modulator_duration,as.numeric(gsub("\\[|\\]", "", regmatches(reg[i], gregexpr("\\[.*?\\]", reg[i]))[[1]])))
            reg[i]<-gsub("\\[.*?]","",reg[i])
            upreg_dur<-c(upreg_dur,paste(",any",reg[i],"_",output,sep=""))
            R<-paste("\n\tlastns.f('",reg[i],"',time,pattern.m,update.m,asynchronous,",paste("any",reg[i],"_",output,sep=""),")",sep="")
          }
        }else{
          modulator_duration<-c(modulator_duration,3)
          upreg_dur<-c(upreg_dur,paste(",any",reg[i],"_",output,sep=""))
          R<-paste("\n\tlastns.f('",reg[i],"',time,pattern.m,update.m,asynchronous,",paste("any",reg[i],"_",output,sep=""),")",sep="")
        }
  
      }else if(reg[i]=="1"){ #Si hay un numero es porque es un input node, por lo que lo puedo guardar en Initial_conditions
        R<-reg[i]
        Initial_conditions<-c(Initial_conditions,output)
      }
      else{R<-paste("\n\tlast.f('",reg[i],"',time,Y=pattern.m, X=update.m,asynchronous)",sep="")
      }
      if(o<=length(operators)){
        R<-paste(R,as.character(operators[o]),sep=" ")
        o=o+1
      }
      regulators<-paste(regulators,R)
    }
    
    regulators<-paste("(",regulators,")& ",paste("Polymorphism.f(P_",output,")",sep=""),sep="")
    arguments<-c(arguments,Us)
    arguments2<-c(arguments2,upreg_dur)
    upreg_dur<-unique(upreg_dur)
    Us<-unique(Us)
    
    Def_fun<-paste(output,".f = function(pattern.m, update.m, time",upreg_dur,paste(",P_",output,sep=""),",asynchronous=TRUE){",sep="")
    if(length(Us)>0){
      Def_fun<-paste(output,".f = function(pattern.m, update.m, time",paste(Us,collapse=""),paste(upreg_dur,collapse=""),paste(",P_",output,sep=""),",asynchronous=TRUE){",sep="")
      Us<-paste(',BN$Arguments["',gsub("[,\\ ]","",Us),'"]',sep="")
    }
    if(length(upreg_dur)>0){
      Def_fun<-paste(output,".f = function(pattern.m, update.m, time",paste(Us,collapse=""),paste(upreg_dur,collapse=""),paste(",P_",output,sep=""),",asynchronous=TRUE){",sep="")
      upreg_dur<-paste(',BN$Modulator["',gsub("[,\\ ]","",upreg_dur),'"]',sep="")
    }
    fun_header<-paste(fun_header,paste("\n\t\t\telse if(node_j=='",output,"')\n",
                   "\t\t\t\tpattern.m=",output,".f(pattern.m, update.m, i",paste(Us,collapse=""),paste(upreg_dur,collapse=""),paste(",Polym['",output,"']",sep=""),",asynchronous)",sep=""))
    
    

    row<-paste("\n\trow ='",output,"' \n\tcol = time\n",sep="")
    
    pattern<-"\n\tpattern.m[row,col]="
    
    Pie<-c("\n \n\treturn(pattern.m)\n}")

    All<-paste(Def_fun,row,pattern,regulators,Pie) 
    
    write(All,"dynamic_evolution.R",append=TRUE)
  }
  #write dynamic_evolution.f
  U_duration<-U_duration[!duplicated(arguments)]
  arguments<-unique(arguments)
  modulator_duration<-modulator_duration[!duplicated(arguments2)]
  arguments2<-unique(arguments2)
  arguments<-gsub("[,\\ ]","",arguments,perl=TRUE)
  arguments2<-gsub("[,\\ ]","",arguments2,perl=TRUE)
  modulator<-suppressWarnings(setNames(as.numeric(modulator_duration),arguments2))

  write.dynamic_evolution(fun_header)
  source("dynamic_evolution.R")
  return(list("nodes.names"=node.names,"Initial_conditions"=Initial_conditions,"Modulator"=modulator,
              "Arguments"=suppressWarnings(setNames(as.numeric(U_duration),arguments)),"Polymorphism"=setNames(rep(1,length(node.names)),node.names)))

}



#Test:
#source("read_Boolean_functions.R")
#source("read_Boolean_functions_R.R")
#source("write_dynamic_evolution.R")

#file<-"Example_networks/example_network.txt"
#
#BN<-read.Boolean.functions(file,language="R")
#source("pattern_creator.R")


#pattern=dynamic_evolution.f(BN,time.steps = 30)

#gsub("\\[|\\]", "", regmatches(gene, gregexpr("\\[.*?\\]", gene))[[1]])