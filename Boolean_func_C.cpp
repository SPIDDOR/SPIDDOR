
 
#include <iostream>
#include <string>
#include <stdlib.h> //rand
#include <vector>
#include <algorithm> //random.suffle
#include <ctime> //time en random seed
#include <Rcpp.h>
using namespace Rcpp;
  
// random generator function:
std::vector<int> myrandom(const int& n_nodes) {
  std::vector<int> samples;
  for (int i = 0; i<n_nodes; ++i) samples.push_back(i);
    std::random_shuffle(samples.begin(), samples.end());
    return(samples);
}
  
bool Polymorphism_f(const double& P){
  int Poli;
  if(P==1){ return 1; }
  if (R::runif(0,1)<=P){
    Poli=1;  
  }else{
    Poli=0;
  }
  return Poli;
}
  
int last_f(int& node, int pattern[], int update[], const int& time, const int ts, bool asynchronous = true) {
  int last=0;
  if (update[node*(ts + 1) +time] == 1 && asynchronous == true) last = pattern[node*(ts + 1) +time];
  else if(update[node*(ts + 1) + time] == 0 && asynchronous == true) last = pattern[node*(ts + 1) +(time - 1)];
  else if(asynchronous == false)last = pattern[node*(ts + 1) + (time - 1)];
  return last;
}

int lastn_f(int& node, int pattern[], int update[], const int& time, const int ts, bool asynchronous = true, const int& n = 1) {
  int last, t1;
  if (time>=n) {
    if (update[node*(ts + 1) + time] == 1 && asynchronous == true) {
      last = 1;
      t1 = time - n + 1;
      for (int i = t1; i <= time; i++) {
        last = last*pattern[node*(ts + 1) +i];
      }
    }
    else {
      last = 1;
      for (int i = (time - n); i <= (time - 1); i++)
      {
        last = last*pattern[node*(ts + 1) +i];
      }
    }
  }
  else if (time == (n-1)) {
    if (update[node*(ts + 1) +time] == 1 && asynchronous == true) {
      last = 1;
      t1 = time - n + 1;
      for (int i = t1; i <= time; i++) {
        last = last*pattern[node*(ts + 1) +i];
      }
    }
    else {
      last = 0;
    }
  }
  else { //if (time < number_back)
    last = 0;
  }
  
  return last;
}

int lastns_f(int& node, int pattern[], int update[], const int& time, const int ts, bool asynchronous = true, const int& number_back = 1)
{
  int last_Xrow = 0;
  int t1;
  int t2 = time - 1;
  if (update[node*(ts + 1) + time] == 1 && asynchronous == true) t2 = time;
  t1 = t2 - number_back + 1;
  if (t1 < 0)t1 = 0;
  for (int i = t1; i <= t2;i++)
  {
  last_Xrow = last_Xrow || pattern[node*(ts + 1) + i];
  }
  
  return last_Xrow;

}
void CycD_f(int pattern[], int update[], const int& time, std::string nodes_names[], 
                   const int&n_nodes, const int& ts,const double& P,bool asynchronous=true){ 
	int CycD= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"CycD")); 
 
	pattern[CycD*(ts + 1) + time]= ( 
	last_f(CycD,pattern,update,time,ts,asynchronous) )& Polymorphism_f(P) ;
}
void Rb_f(int pattern[], int update[], const int& time, std::string nodes_names[], 
                   const int&n_nodes, const int& ts,const double& P,bool asynchronous=true){ 
	int Rb= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"Rb"));
	int CycA= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"CycA"));
	int CycB= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"CycB"));
	int CycD= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"CycD"));
	int CycE= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"CycE"));
	int p27= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"p27")); 
 
	pattern[Rb*(ts + 1) + time]= ( ( ! 
	last_f(CycA,pattern,update,time,ts,asynchronous) & ! 
	last_f(CycB,pattern,update,time,ts,asynchronous) & ! 
	last_f(CycD,pattern,update,time,ts,asynchronous) & ! 
	last_f(CycE,pattern,update,time,ts,asynchronous) ) | ( 
	last_f(p27,pattern,update,time,ts,asynchronous) & ! 
	last_f(CycB,pattern,update,time,ts,asynchronous) & ! 
	last_f(CycD,pattern,update,time,ts,asynchronous) ))& Polymorphism_f(P) ;
}
void E2F_f(int pattern[], int update[], const int& time, std::string nodes_names[], 
                   const int&n_nodes, const int& ts,const double& P,bool asynchronous=true){ 
	int E2F= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"E2F"));
	int Rb= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"Rb"));
	int CycA= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"CycA"));
	int CycB= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"CycB"));
	int p27= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"p27")); 
 
	pattern[E2F*(ts + 1) + time]= ( ( ! 
	last_f(Rb,pattern,update,time,ts,asynchronous) & ! 
	last_f(CycA,pattern,update,time,ts,asynchronous) & ! 
	last_f(CycB,pattern,update,time,ts,asynchronous) ) | ( 
	last_f(p27,pattern,update,time,ts,asynchronous) & ! 
	last_f(Rb,pattern,update,time,ts,asynchronous) & ! 
	last_f(CycB,pattern,update,time,ts,asynchronous) ))& Polymorphism_f(P) ;
}
void CycE_f(int pattern[], int update[], const int& time, std::string nodes_names[], 
                   const int&n_nodes, const int& ts,const double& P,bool asynchronous=true){ 
	int CycE= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"CycE"));
	int E2F= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"E2F"));
	int Rb= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"Rb")); 
 
	pattern[CycE*(ts + 1) + time]= ( ( 
	last_f(E2F,pattern,update,time,ts,asynchronous) & ! 
	last_f(Rb,pattern,update,time,ts,asynchronous) ))& Polymorphism_f(P) ;
}
void CycA_f(int pattern[], int update[], const int& time, std::string nodes_names[], 
                   const int&n_nodes, const int& ts,const double& P,bool asynchronous=true){ 
	int CycA= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"CycA"));
	int E2F= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"E2F"));
	int Rb= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"Rb"));
	int Cdc20= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"Cdc20"));
	int Cdh1= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"Cdh1"));
	int UbcH10= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"UbcH10")); 
 
	pattern[CycA*(ts + 1) + time]= ( ( 
	last_f(E2F,pattern,update,time,ts,asynchronous) & ! 
	last_f(Rb,pattern,update,time,ts,asynchronous) & ! 
	last_f(Cdc20,pattern,update,time,ts,asynchronous) & ! ( 
	last_f(Cdh1,pattern,update,time,ts,asynchronous) & 
	last_f(UbcH10,pattern,update,time,ts,asynchronous) ) ) | ( 
	last_f(CycA,pattern,update,time,ts,asynchronous) & ! 
	last_f(Rb,pattern,update,time,ts,asynchronous) & ! 
	last_f(Cdc20,pattern,update,time,ts,asynchronous) & ! ( 
	last_f(Cdh1,pattern,update,time,ts,asynchronous) & 
	last_f(UbcH10,pattern,update,time,ts,asynchronous) ) ))& Polymorphism_f(P) ;
}
void p27_f(int pattern[], int update[], const int& time, std::string nodes_names[], 
                   const int&n_nodes, const int& ts,const double& P,bool asynchronous=true){ 
	int p27= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"p27"));
	int CycD= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"CycD"));
	int CycE= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"CycE"));
	int CycA= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"CycA"));
	int CycB= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"CycB")); 
 
	pattern[p27*(ts + 1) + time]= ( ( ! 
	last_f(CycD,pattern,update,time,ts,asynchronous) & ! 
	last_f(CycE,pattern,update,time,ts,asynchronous) & ! 
	last_f(CycA,pattern,update,time,ts,asynchronous) & ! 
	last_f(CycB,pattern,update,time,ts,asynchronous) ) | ( 
	last_f(p27,pattern,update,time,ts,asynchronous) & ! ( 
	last_f(CycE,pattern,update,time,ts,asynchronous) & 
	last_f(CycA,pattern,update,time,ts,asynchronous) ) & ! 
	last_f(CycB,pattern,update,time,ts,asynchronous) & ! 
	last_f(CycD,pattern,update,time,ts,asynchronous) ))& Polymorphism_f(P) ;
}
void Cdc20_f(int pattern[], int update[], const int& time, std::string nodes_names[], 
                   const int&n_nodes, const int& ts,const double& P,bool asynchronous=true){ 
	int Cdc20= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"Cdc20"));
	int CycB= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"CycB")); 
 
	pattern[Cdc20*(ts + 1) + time]= ( 
	last_f(CycB,pattern,update,time,ts,asynchronous) )& Polymorphism_f(P) ;
}
void Cdh1_f(int pattern[], int update[], const int& time, std::string nodes_names[], 
                   const int&n_nodes, const int& ts,const double& P,bool asynchronous=true){ 
	int Cdh1= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"Cdh1"));
	int CycA= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"CycA"));
	int CycB= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"CycB"));
	int Cdc20= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"Cdc20"));
	int p27= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"p27")); 
 
	pattern[Cdh1*(ts + 1) + time]= ( ( ! 
	last_f(CycA,pattern,update,time,ts,asynchronous) & ! 
	last_f(CycB,pattern,update,time,ts,asynchronous) ) | ( 
	last_f(Cdc20,pattern,update,time,ts,asynchronous) ) | ( 
	last_f(p27,pattern,update,time,ts,asynchronous) & ! 
	last_f(CycB,pattern,update,time,ts,asynchronous) ))& Polymorphism_f(P) ;
}
void UbcH10_f(int pattern[], int update[], const int& time, std::string nodes_names[], 
                   const int&n_nodes, const int& ts,const double& P,bool asynchronous=true){ 
	int UbcH10= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"UbcH10"));
	int Cdh1= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"Cdh1"));
	int Cdc20= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"Cdc20"));
	int CycA= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"CycA"));
	int CycB= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"CycB")); 
 
	pattern[UbcH10*(ts + 1) + time]= ( ! 
	last_f(Cdh1,pattern,update,time,ts,asynchronous) | ( 
	last_f(Cdh1,pattern,update,time,ts,asynchronous) & 
	last_f(UbcH10,pattern,update,time,ts,asynchronous) & ( 
	last_f(Cdc20,pattern,update,time,ts,asynchronous) | 
	last_f(CycA,pattern,update,time,ts,asynchronous) | 
	last_f(CycB,pattern,update,time,ts,asynchronous) ) ))& Polymorphism_f(P) ;
}
void CycB_f(int pattern[], int update[], const int& time, std::string nodes_names[], 
                   const int&n_nodes, const int& ts,const double& P,bool asynchronous=true){ 
	int CycB= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"CycB"));
	int Cdc20= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"Cdc20"));
	int Cdh1= std::distance(nodes_names, std::find(nodes_names, nodes_names + (n_nodes - 1),"Cdh1")); 
 
	pattern[CycB*(ts + 1) + time]= ( ! 
	last_f(Cdc20,pattern,update,time,ts,asynchronous) & ! 
	last_f(Cdh1,pattern,update,time,ts,asynchronous))& Polymorphism_f(P) ;
}

// [[Rcpp::export]]
NumericVector time_evolution_f(const int& ts  ,std::vector<std::string> KO_nodes,std::vector<std::string> Over_expr,
              std::vector<std::string> Over_expr_AA,
              List MCAB_times,List Tx_times, List Polym,
              IntegerVector Initial_cond, const bool asynchronous){

  std::string node_i;
  std::vector<int> samples;
	std::string nodes_names[] = {"CycD","Rb","E2F","CycE","CycA","p27","Cdc20","Cdh1","UbcH10","CycB"};

  const int n_nodes = sizeof(nodes_names) / sizeof(nodes_names[0]);
  //Pattern and update creation:
  int *pattern = new int[n_nodes*(ts + 1)]();
  int *update =new int[n_nodes*(ts + 1)]();
          
  //Initial conditions:
  for (int it = 0; it < Initial_cond.size(); it++){ pattern[Initial_cond[it]*(ts + 1)]=(1);}
  for (int i = 0; i < n_nodes; i++) { update[i*(ts)+i] = ( 1 ); }

	const double P_CycD=as<double>(Polym["CycD"]);
	const double P_Rb=as<double>(Polym["Rb"]);
	const double P_E2F=as<double>(Polym["E2F"]);
	const double P_CycE=as<double>(Polym["CycE"]);
	const double P_CycA=as<double>(Polym["CycA"]);
	const double P_p27=as<double>(Polym["p27"]);
	const double P_Cdc20=as<double>(Polym["Cdc20"]);
	const double P_Cdh1=as<double>(Polym["Cdh1"]);
	const double P_UbcH10=as<double>(Polym["UbcH10"]);
	const double P_CycB=as<double>(Polym["CycB"]);

  
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
  
 if(node_i == "CycD") CycD_f(pattern, update, j,nodes_names,n_nodes,ts,P_CycD,asynchronous);
			else if(node_i == "Rb") Rb_f(pattern, update, j,nodes_names,n_nodes,ts,P_Rb,asynchronous);
			else if(node_i == "E2F") E2F_f(pattern, update, j,nodes_names,n_nodes,ts,P_E2F,asynchronous);
			else if(node_i == "CycE") CycE_f(pattern, update, j,nodes_names,n_nodes,ts,P_CycE,asynchronous);
			else if(node_i == "CycA") CycA_f(pattern, update, j,nodes_names,n_nodes,ts,P_CycA,asynchronous);
			else if(node_i == "p27") p27_f(pattern, update, j,nodes_names,n_nodes,ts,P_p27,asynchronous);
			else if(node_i == "Cdc20") Cdc20_f(pattern, update, j,nodes_names,n_nodes,ts,P_Cdc20,asynchronous);
			else if(node_i == "Cdh1") Cdh1_f(pattern, update, j,nodes_names,n_nodes,ts,P_Cdh1,asynchronous);
			else if(node_i == "UbcH10") UbcH10_f(pattern, update, j,nodes_names,n_nodes,ts,P_UbcH10,asynchronous);
			else if(node_i == "CycB") CycB_f(pattern, update, j,nodes_names,n_nodes,ts,P_CycB,asynchronous);

      update[samples.at(i)*(ts+1) + j] = 1;
    }
  } 
  Rcpp::NumericVector P(pattern, pattern +(n_nodes*(ts + 1)));
  delete[] update;
  delete[] pattern;
  return(P);
}
