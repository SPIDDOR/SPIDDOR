write.dynamic_evolution_cpp=function(arguments,fun_header,nodes.names)
{
  #write("","dyn_evolution.cpp")
 
  Head<-paste("
// [[Rcpp::export]]
NumericVector time_evolution_f(const int& ts",
              arguments,",std::vector<std::string> KO_nodes,std::vector<std::string> Over_expr,
              std::vector<std::string> Over_expr_AA,
              List MCAB_times,List Tx_times, List Polym,
              IntegerVector Initial_cond, const bool asynchronous){

  std::string node_i;
  std::vector<int> samples;")
  
  nodes<-paste("\tstd::string nodes_names[] = {",paste('"',paste(nodes.names,collapse='","'),'"',sep=""),"};",sep="") #Nombre de los nodos entre comillas
  
  dyn1<-"
  const int n_nodes = sizeof(nodes_names) / sizeof(nodes_names[0]);
  //Pattern and update creation:
  int *pattern = new int[n_nodes*(ts + 1)]();
  int *update =new int[n_nodes*(ts + 1)]();
          
  //Initial conditions:
  for (int it = 0; it < Initial_cond.size(); it++){ pattern[Initial_cond[it]*(ts + 1)]=(1);}
  for (int i = 0; i < n_nodes; i++) { update[i*(ts)+i] = ( 1 ); }\n"
  Polymorphism<-c()
  for(i in nodes.names)
    Polymorphism<-c(Polymorphism,paste("\tconst double P_",i,'=as<double>(Polym["',i,'"]);',sep="") ) 

dyn2<-"\n  
  //Iterate:
  for (int j = 1; j <= ts; j++) {
    samples = myrandom(n_nodes);
    for (int i = 0; i <n_nodes; i++) {
      node_i = nodes_names[samples.at(i)];
              
      if (std::find(Over_expr_AA.begin(), Over_expr_AA.end(), node_i) != Over_expr_AA.end()) {
        int node = std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1), node_i));
        if (pattern[node*(ts + 1) + (j - 1)] == 1) {
          pattern[node*(ts + 1) + j] = 1;
          continue;
        }
      }
      if (std::find(KO_nodes.begin(), KO_nodes.end(), node_i) != KO_nodes.end()){// && MCAB_times.size()==0){
        int node = std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1), node_i));
        if(MCAB_times.size()==0){
          pattern[node*(ts + 1) + j] = 0;
          update[samples.at(i)*(ts+1) + j] = 1;
          continue;
        }else{
          int pos = std::find(KO_nodes.begin(), KO_nodes.end(), node_i) - KO_nodes.begin();
          std::vector<int>KO_time=MCAB_times[pos];
          if(std::find(KO_time.begin(), KO_time.end(), j) != KO_time.end()){
            pattern[node*(ts + 1) + j] = 0;
            update[samples.at(i)*(ts+1) + j] = 1;
            continue;
          }
        }
      }
          
      if (std::find(Over_expr.begin(), Over_expr.end(), node_i) != Over_expr.end()){
        int node = std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1), node_i));
        if(Tx_times.size()==0){
          pattern[node*(ts + 1) + j] = 1;
          update[samples.at(i)*(ts+1) + j] = 1;
          continue;
        }else{
          int pos = std::find(Over_expr.begin(), Over_expr.end(), node_i) - Over_expr.begin();
          std::vector<int>OE_time=Tx_times[pos];
          if(std::find(OE_time.begin(), OE_time.end(), j) != OE_time.end()){
            pattern[node*(ts + 1) + j] = 1;
            update[samples.at(i)*(ts+1) + j] = 1;
            continue;
          }
        }
      }
  "      
  
  Pie_dyn<-"
      update[samples.at(i)*(ts+1) + j] = 1;
    }
  } 
  Rcpp::NumericVector P(pattern, pattern +(n_nodes*(ts + 1)));
  delete[] update;
  delete[] pattern;
  return(P);
}"
  All<-c(Head,nodes,dyn1,Polymorphism,dyn2,fun_header,Pie_dyn)
  write(All,"Boolean_func_C.cpp",append=TRUE)
}

