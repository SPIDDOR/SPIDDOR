read.Boolean.functions.C<-function(file=NULL,Lines=NULL){
  
  if(length(Lines)==0) Lines <- readLines(file, -1)
  Lines <- gsub("#.*", "", Lines) #remove comments
  Lines <- Lines[nchar(Lines) > 0]
  
  #Create .cpp:
  write("","Boolean_func_C.cpp")
  add_header("Boolean_func_C.cpp")
  
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
    reg<-gsub("(THR|MOD|ANY)_","",reg,perl=TRUE)#ignore.case=T)
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
  arg_modulator<-c()
  U_duration<-c()
  fun_header<-c()
  Initial_conditions<-c()
  modulator_duration<-c()
  modulator<-c()
  
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
    upreg_dur_f<-c()
    regulators<-c()
    regulators_header<-c()
    Us<-c()
    Us_f<-c()
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
      if(grepl("THR_",reg[i],perl=TRUE)){
        reg[i]<-gsub("THR_","",reg[i],perl=TRUE)
        if(gregexpr("\\[", reg[i])!=-1){
          if(suppressWarnings(is.na(as.numeric(gsub("\\[|\\]", "", regmatches(reg[i], gregexpr("\\[.*?\\]", reg[i]))[[1]]))))){
            U_duration<-c(U_duration,3)
            Us_f<-c(Us_f,paste(",",gsub("\\[|\\]", "", regmatches(reg[i], gregexpr("\\[.*?\\]", reg[i]))[[1]])))
            Us<-c(Us,paste(",const int & ",gsub("\\[|\\]", "", regmatches(reg[i], gregexpr("\\[.*?\\]", reg[i]))[[1]]),sep=""))
            R<-paste("\n\tlastn_f(",gsub("\\[.*?]","",reg[i]),",pattern,update,time,ts,asynchronous,",
                     gsub("\\[|\\]", "", regmatches(reg[i], gregexpr("\\[.*?\\]", reg[i]))[[1]]),")",sep="")
            reg[i]<-gsub("\\[.*?]","",reg[i])
          }else{
            U_duration<-c(U_duration,as.numeric(gsub("\\[|\\]", "", regmatches(reg[i], gregexpr("\\[.*?\\]", reg[i]))[[1]])))
            reg[i]<-gsub("\\[.*?]","",reg[i])
            Us_f<-c(Us_f,paste(",",paste(reg[i],"max_",output,sep=""),sep=""))
            Us<-c(Us,paste(",const int & ",reg[i],"max_",output,sep=""))
            R<-paste("\n\tlastn_f(",reg[i],",pattern,update,time,ts,asynchronous,",
                     reg[i],"max_",output,")",sep="")
          }
          #reg[i]<-gsub("\\[.*?]","",reg[i])
        }else{
          U_duration<-c(U_duration,3)
          Us_f<-c(Us_f,paste(",",paste(reg[i],"max_",output,sep=""),sep=""))
          Us<-c(Us,paste(",const int & ",reg[i],"max_",output,sep=""))
          R<-paste("\n\tlastn_f(",reg[i],",pattern,update,time,ts,asynchronous,",
                   reg[i],"max_",output,")",sep="")
        }
        
        regulators_header<-c(regulators_header,paste("\n\tint ",reg[i],'= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"',gsub("THR_","",reg[i],perl=TRUE),'"));',sep=""))
        
      }else if(grepl("MOD_",reg[i],perl=TRUE)){
        reg[i]<-gsub("MOD_","",reg[i],perl=TRUE)
        
        if(gregexpr("\\[", reg[i])!=-1){
          if(suppressWarnings(is.na(as.numeric(gsub("\\[|\\]", "", regmatches(reg[i], gregexpr("\\[.*?\\]", reg[i]))[[1]]))))){
            modulator_duration<-c(modulator_duration,3)
            upreg_dur_f<-c(upreg_dur_f,paste(",",gsub("\\[|\\]", "", regmatches(reg[i], gregexpr("\\[.*?\\]", reg[i]))[[1]])))
            upreg_dur<-c(upreg_dur,paste(",const int& ",gsub("\\[|\\]", "", regmatches(reg[i], gregexpr("\\[.*?\\]", reg[i]))[[1]]),sep=""))
            R<-paste("\n\tlastn_f(",gsub("\\[.*?]","",reg[i]),",pattern,update,time,ts,asynchronous,",gsub("\\[|\\]", "", regmatches(reg[i], gregexpr("\\[.*?\\]", reg[i]))[[1]]),")",sep="")
          }else{
            modulator_duration<-c(modulator_duration,as.numeric(gsub("\\[|\\]", "", regmatches(reg[i], gregexpr("\\[.*?\\]", reg[i]))[[1]])))
            upreg_dur_f<-c(upreg_dur_f,paste(",MOD_",output,sep=""))
            upreg_dur<-c(upreg_dur,paste(",const int& MOD_",output,sep=""))
            R<-paste("\n\tlastn_f(",gsub("\\[.*?]","",reg[i]),",pattern,update,time,ts,asynchronous,",paste("MOD_",output,sep=""),")",sep="")
            
          }
          reg[i]<-gsub("\\[.*?]","",reg[i])
        }else{
          modulator_duration<-c(modulator_duration,3)
          upreg_dur_f<-c(upreg_dur_f,",modulator_dur")
          upreg_dur<-c(upreg_dur,",const int& modulator_dur")
          R<-paste("\n\tlastn_f(",reg[i],",pattern,update,time,ts,asynchronous,modulator_dur)",sep="")
        }
        
        regulators_header<-c(regulators_header,paste("\n\tint ",reg[i],'= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"',reg[i],'"));',sep=""))
        
      }else if(grepl("ANY_",reg[i],perl=TRUE)){
        reg[i]<-gsub("ANY_","",reg[i],perl=TRUE)
        
        if(gregexpr("\\[", reg[i])!=-1){
          if(suppressWarnings(is.na(as.numeric(gsub("\\[|\\]", "", regmatches(reg[i], gregexpr("\\[.*?\\]", reg[i]))[[1]]))))){
            modulator_duration<-c(modulator_duration,3)
            upreg_dur_f<-c(upreg_dur_f,paste(",",gsub("\\[|\\]", "", regmatches(reg[i], gregexpr("\\[.*?\\]", reg[i]))[[1]])))
            upreg_dur<-c(upreg_dur,paste(",const int& ",gsub("\\[|\\]", "", regmatches(reg[i], gregexpr("\\[.*?\\]", reg[i]))[[1]]),sep=""))
            R<-paste("\n\tlastns_f(",gsub("\\[.*?]","",reg[i]),",pattern,update,time,ts,asynchronous,",gsub("\\[|\\]", "", regmatches(reg[i], gregexpr("\\[.*?\\]", reg[i]))[[1]]),")",sep="")
            reg[i]<-gsub("\\[.*?]","",reg[i])
          }else{
            modulator_duration<-c(modulator_duration,as.numeric(gsub("\\[|\\]", "", regmatches(reg[i], gregexpr("\\[.*?\\]", reg[i]))[[1]])))
            reg[i]<-gsub("\\[.*?]","",reg[i])
            upreg_dur_f<-c(upreg_dur_f,paste(",any",reg[i],"_",output,sep=""))
            upreg_dur<-c(upreg_dur,paste(",const int& any",reg[i],"_",output,sep=""))
            R<-paste("\n\tlastns_f(",reg[i],",pattern,update,time,ts,asynchronous,",paste("any",reg[i],"_",output,sep=""),")",sep="")
          }
        }else{
          modulator_duration<-c(modulator_duration,3)
          upreg_dur_f<-c(upreg_dur_f,paste(",any",reg[i],"_",output,sep=""))
          upreg_dur<-c(upreg_dur,paste(",const int& any",reg[i],"_",output,sep=""))
          R<-paste("\n\tlastns_f(",reg[i],",pattern,update,time,ts,asynchronous,",paste("any",reg[i],"_",output,sep=""),")",sep="")
        }
        regulators_header<-c(regulators_header,paste("\n\tint ",reg[i],'= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"',reg[i],'"));',sep=""))
        
      }else if(reg[i]=="1"){ #Si hay un numero es porque es un input node, por lo que lo puedo guardar en Initial_conditions
        R<-reg[i]
        Initial_conditions<-c(Initial_conditions,output)
      }
      else{
        regulators_header<-c(regulators_header,paste("\n\tint ",reg[i],'= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"',reg[i],'"));',sep=""))
        R<-paste("\n\tlast_f(",reg[i],",pattern,update,time,ts,asynchronous)",sep="")
      }
      if(o<=length(operators)){
        R<-paste(R,as.character(operators[o]),sep=" ")
        o=o+1
      }
      regulators<-paste(regulators,R)
    }
    
    regulators<-paste("(",regulators,")& Polymorphism_f(P)",sep="")
    regulators<-paste(regulators,";\n}")
    
    arg_modulator<-c(arg_modulator,upreg_dur_f)
    arguments<-c(arguments,Us_f)
    upreg_dur_f<-unique(upreg_dur_f)
    upreg_dur<-unique(upreg_dur)
    Us_f<-unique(Us_f)
    Us<-unique(Us)
    
    row<-paste("\n\tint ",output,'= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"',output,'"));',sep="")
    regulators_header<-c(row,regulators_header)
    regulators_header<-unique(regulators_header)
    regulators_header<-paste(regulators_header,collapse="")
    
    Def_fun<-paste("void ",output,"_f(int pattern[], int update[], const int& time, std::string nodes_names[], 
                   const int&n_nodes, const int& ts",paste(Us,collapse=""),paste(upreg_dur,collapse=""),",const double& P,bool asynchronous=true){",sep="")
    
    fun_header<-c(fun_header,paste(' if(node_i == "',output,'") ',
                                   output,"_f(pattern, update, j,nodes_names,n_nodes,ts",paste(Us_f,collapse=""),paste(upreg_dur_f,collapse=""),paste(",P_",output,sep=""),",asynchronous);",sep=""))
    fun_header<-paste(fun_header,collapse="\n\t\t\telse")
    
    pattern<-paste("\n \n\tpattern[",output,"*(ts + 1) + time]=",sep="")
    
    All<-paste(Def_fun,regulators_header,pattern,regulators) 
    
    write(All,"Boolean_func_C.cpp",append=TRUE)
    
                   }
  #write dynamic_evolution.f
  if(length(arguments>0)){
    arguments<-gsub("[,\\s]","",arguments,perl=TRUE)
  }
  
  
  arg_modulator<-gsub("[,\\s]","",arg_modulator,perl=TRUE)
  arguments2<-c(arguments,arg_modulator)
  if(length(arguments2)>0)  arguments2<-paste(",",paste("const int&",arguments2))
  arguments2<-unique(arguments2)
  arguments2<-paste(arguments2,collapse="")
  
  U_duration<-U_duration[!duplicated(arguments)]
  arguments<-unique(arguments)
  modulator_duration<-modulator_duration[!duplicated(arg_modulator)]
  arg_modulator<-unique(arg_modulator)
  modulator<-suppressWarnings(setNames(as.numeric(modulator_duration),arg_modulator))
  
  write.dynamic_evolution_cpp(arguments2,fun_header,node.names)
  write.dynamic_evolution_c(arguments,arg_modulator)
  
  source("dynamic_evolution.R")
  
  return(list("nodes.names"=node.names,"Initial_conditions"=Initial_conditions,"Modulator"=modulator,
              "Arguments"=suppressWarnings(setNames(as.numeric(U_duration),arguments)),"Polymorphism"=setNames(rep(1,length(node.names)),node.names)))
  
  }

# source("R/read_Boolean_functions.R")
# source("R/read_Boolean_functions_c.R")
# source("R/add_header.R")
# source("R/write_dynamic_evolution_cpp.R")
# source("R/write_dynamic_evolution_c.R")
# 
# library(Rcpp)
# file<-"Example_networks/example_network.txt"
# BN<-read.Boolean.functions(file)
# pattern=dynamic_evolution.f(BN,time.steps = 30)