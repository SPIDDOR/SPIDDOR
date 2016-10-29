add_header=function(file){
 write(" 
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

}",file,append=TRUE)
}