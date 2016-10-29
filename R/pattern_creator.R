
pattern_creator.f=function(nodes.names,time.steps,Initial_conditions){
  # The pattern matrix will display the state of each node at any time step 
  number.nodes=length(nodes.names)
  pattern.m=matrix(0,ncol=(time.steps+1),nrow=number.nodes)
  row.names(pattern.m)=nodes.names
  colnames(pattern.m)=seq(0:time.steps)
  
  # The Update matrix will display which nodes have been updated at a particular time step
  update.m=matrix(0,ncol=(time.steps+1),nrow=number.nodes)
  row.names(update.m)=nodes.names
  colnames(update.m)=seq(0:time.steps)
  
  #Initial conditions:
  pattern.m[Initial_conditions,1]=1
  update.m[,1]=rep(1,number.nodes) 
  
  return(list(pattern.m,update.m))
}

##########################  KO_node.f  #################################
########################################################################
KO_node.f = function(nodes,X)
{
  # knocks out the nodes for all the time steps
  row = which(rownames(X) %in% nodes)
  X[row,]= 0 
  return(X)
  #EXAMPLE pattern.temp=KO_node.f("A",pattern.m)
}

##########################  Over_expression.f  #########################
########################################################################
Over_expression.f = function(Xrow,X)
{
  # overexpress a node in all time steps
  row = which(rownames(X) %in% Xrow)
  X[row,]= 1 
  return(X)
  #EXAMPLE pattern.temp=Over_expression.f("B",pattern.temp)
}

##########################  Over_expr_after_activation.f  ##############
########################################################################
Over_expr_after_activation.f = function(node, time.step,X)
{
  # overexpress a node after its first activation
  if(X[node,time.step-1]==1){
    X[node,time.step]= 1 
  }
  return(X)
}

##########################  Polymorphism.f  ############################
########################################################################
Polymorphism.f=function(P){
  if(P==1){ return(1) }
  if (runif(1,min=0,max=1)<=P){
    Poli=1  
  }else{
    Poli=0
  }
  return(Poli)
}
##########################  last.f  ####################################
########################################################################
last.f = function(node, step,Y, X,asynchronous)
{
  #Y is pattern matrix, X is update matrix
  if(X[node, step]==1 & asynchronous==TRUE)last.Xrow=Y[node, step]
  else if(X[node, step]==0 & asynchronous==TRUE){
    last.Xrow=Y[node, step-1]
  }
  else if(asynchronous==FALSE){last.Xrow=Y[node, step-1]}
  return(last.Xrow)
  
}
##########################  lastn.f  ####################################
########################################################################
lastn.f = function(Xrow, step,Y, X,asynchronous=TRUE,number_back = 1)
{
  # Xrow is name of node
  #Y is pattern matrix, X is update matrix
  row = Xrow
  col = step
  Xrow.t = X[row, col]
  if(step > number_back)
  {
    if(Xrow.t==1 & asynchronous==TRUE)
    {
      last.Xrow=1
      t1 = step - number_back +1
      for(i in t1:step)
      {
        last.Xrow =last.Xrow * Y[row,i]
      }
    }
    else
    {
      last.Xrow=1
      for(i in (step-number_back):(step-1))
      {
        last.Xrow=last.Xrow*Y[row,i]
      }
    }
  }
  else if(step == number_back)
  {
    if(Xrow.t==1 & asynchronous==TRUE)
    {
      last.Xrow=1
      t1 = step - number_back +1
      for(i in t1:step)
      {
        last.Xrow=last.Xrow*Y[row,i]
      }
    }
    else
    {
      last.Xrow = 0
    } 
  }
  else # if (step < number_back)
  {
    last.Xrow = 0 
  }
  
  return(last.Xrow)
  #EXAMPLE lastn.f("Ag",step = 5,pattern.test,update.test,asynchronous=TRUE, number_back = 3)
}

##########################  lastns.f  ####################################
##########################################################################
lastns.f = function(node, time_step,Y, X,asynchronous=TRUE,number_back)
{
  # returns TRUE if node "Xrow" is activated in any of the last "number_back" time steps
  # Xrow is name of node
  #Y is pattern matrix, X is update matrix
  last.Xrow = 0
  t2 = time_step-1
  if(X[node, time_step]==1 & asynchronous==TRUE ) t2 = time_step
  t1 = t2 - number_back + 1
  if(t1<1)t1 = 1
  for (i in t1:t2)
  {
    last.Xrow =last.Xrow | Y[node,i]
  }
  
  return(last.Xrow)
  #EXAMPLE lastns.f("Treg",5,pattern.test,update.test, 3, asynchronous=TRUE)
}
