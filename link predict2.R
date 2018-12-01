library(matlab)
library(matrixcalc)
library(igraph)
#A=matrix(c(0,1,1,0,0,1,0,1,1,1,1,1,0,1,0,0,1,1,0,0,0,1,0,0,0),nrow=5,ncol=5)
#A=matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),nrow=5,ncol=5)
#graph<- graph_from_adjacency_matrix(A,mode=c("undirected"))
graph<-read_graph("C:/Users/dell/Documents/dolphin.gml",format = c("gml"))
#graph<-read_graph("C:/Users/dell/Documents/karate.gml",format = c("gml"))
#graph<-read_graph("C:/Users/dell/Documents/lesmis.gml",format = c("gml"))
#graph<-read_graph("C:/Users/dell/Documents/football.gml",format = c("gml"))
#graph<-read_graph("C:/Users/dell/Documents/polblogs.gml",format = c("gml"))
#graph<-read_graph("C:/Users/dell/Documents/adjnoun.gml",format = c("gml"))
#graph<-read_graph("C:/Users/dell/Documents/celegansneural.gml",format = c("gml"))

#graph
#plot(graph)

data<-get.data.frame(graph)
#data
exi_links<-get.adjacency(graph)
#exi_links
n<- nrow(exi_links)
#n
nonexi<-matrix(data=-exi_links,nrow = nrow(exi_links))
nonexi<- nonexi+1
#nonexi
for(i in 1:nrow(nonexi)){
  nonexi[i,i]<-0
}

nongraph<- graph_from_adjacency_matrix(nonexi,mode=c("undirected"))
nonexistlist<-get.data.frame(nongraph)
degree <- function(cnode , graph){
  graph1 <- matrix(data=0,nrow=nrow(graph),ncol=ncol(graph))
  for(i in 1:nrow(graph)){
    for(j in 1:ncol(graph)){
      if(graph[i,j]==1){
        graph1[i,j]=1
      }
    }
  }
  a <- graph1[cnode,]
  s <- sum(a)
  
  s
}
score<-matrix(data=0,nrow=n,ncol=n)
predict<-function(graphmat)
{
  prev<-matrix(data=0,nrow=n,ncol=n)
  score<-matrix(data=0,nrow=n,ncol=n)
  
 
  for(i in 1:n)
  {
    for(j in 1:n)
    {
      for(k in 1:n)
      {
        if(graphmat[i,k]==1 && graphmat[k,j]==1)
        {
          score[i,j]<-score[i,j]+1/degree(k,graphmat)
        }
      }
    }
  }
  for(i in 1:n)
  {
    for(j in 1:n)
    {
      prev[i,j]<-score[i,j]
    }
  }
  for(p in 3:6)
  {
    temp<-matrix(data=0,nrow=n,ncol=n)
    for(i in 1:n)
    {
      for(j in 1:n)
      {
        temp1<-0
        if(i<j)
        {
          for(k in 1:n)
          {
            if(graphmat[i,k]==1 & prev[k,j]!=0)
            {
              temp1<-temp1+(score[i,k]*prev[k,j])*((0.01)^p)
            }
          }
          temp[i,j]<- temp1
        }
      }
    }
    for(i in 1:n)
    {
      for(j in 1:n)
      {
        if(i>j)
        {
          temp[i,j]<-temp[j,i]
        }
      }
    }
    for(i in 1:n)
    {
      for(j in 1:n)
      {
        score[i,j]<-score[i,j]+temp[i,j]
        prev[i,j]<-temp[i,j]
      }
    }
    
  }
  return(score)
}





common_auc <- function(data,n){
  
  graphmat<-matrix(data=-1,nrow=n,ncol=n)
  no_edges=nrow(data)
  nrFolds <- 10
  result=0
  precision=0
  # generate array containing fold-number for each sample (row)
  folds <- rep_len(1:nrFolds, nrow(data))
  
  # actual cross validation
  for(k in 1:nrFolds) {
    # actual split of the data
    fold <- which(folds == k)
    traindata <- data[-fold,]
    testdata <-  data[fold,]
    graphmat<-matrix(data=-1,nrow=n,ncol=n)
    for(i in 1:nrow(traindata)){
      graphmat[traindata[i,1],traindata[i,2]]<-1
      graphmat[traindata[i,2],traindata[i,1]]<- 1
    }
    CNmat<-matrix(data=-1,nrow=n,ncol=n)
    
    score<-predict(graphmat)
    for(i in 1:nrow(graphmat)){
      for(j in 1:ncol(graphmat)){
        if(graphmat[i,j]==-1){
          CNmat[i,j]<-score[i,j]
        }
      }
    }
    
    equal=0
    greater=0
    total=nrow(testdata)*nrow(nonexistlist)
    
    #implementing precision
    allrows<-rbind(testdata,nonexistlist)
    nallrows<-nrow(allrows)
    scorerow<-rep(0,nallrows)
    for(i in 1:nallrows){
      scorerow[i]<-CNmat[allrows[i,1],allrows[i,2]]
    }
    orderscore<-order(-scorerow)
    count=0
    top10<-orderscore[1:10]
    for(i in 1:10){
      if(top10[i]<=nrow(testdata)){
        count=count+1
      }
    }
    precision=precision + count/10

# AUC
    for(i in 1:nrow(testdata)){
      for(j in 1:nrow(nonexistlist)){
        if(CNmat[testdata[i,1],testdata[i,2]] > CNmat[nonexistlist[j,1],nonexistlist[j,2]])
        {
          greater=greater + 1
        }
        if(CNmat[testdata[i,1],testdata[i,2]] == CNmat[nonexistlist[j,1],nonexistlist[j,2]])
        {
          equal=equal+1
        }
      }
    }
    
    result=result+(greater+0.5*equal)/total
  }
  print(precision/10)
  
  result/10
}
CNAUC<-common_auc(data,n)

CNAUC

