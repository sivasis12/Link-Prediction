library(matlab)
library(matrixcalc) 
library(igraph)
#common_graph<-erdos.renyi.game(100,0.35)
#common_graph<-read_graph("C:/Users/dell/Documents/dolphin.gml",format = c("gml"))
#common_graph<-read_graph("C:/Users/dell/Documents/karate.gml",format = c("gml"))
#common_graph<-read_graph("C:/Users/dell/Documents/lesmis.gml",format = c("gml"))
common_graph<-read_graph("C:/Users/dell/Documents/football.gml",format = c("gml"))
#common_graph<-read_graph("C:/Users/dell/Documents/polblogs.gml",format = c("gml"))
#common_graph<-read_graph("C:/Users/dell/Documents/adjnoun.gml",format = c("gml"))
#common_graph<-read_graph("C:/Users/dell/Documents/celegansneural.gml",format = c("gml"))
#data1 <- get.data.frame(common_graph)
#data <- subset(data1,select=-c(value,id))
data<-get.data.frame(common_graph)

exi_links<-get.adjacency(common_graph)

nodes<- nrow(exi_links)
nonexi<-matrix(data=-exi_links,nrow = nrow(exi_links))

nonexi<- nonexi+1

for(i in 1:nrow(nonexi)){
  nonexi[i,i]<-0
}
nongraph<- graph_from_adjacency_matrix(nonexi,mode=c("undirected"))

nonexistlist<-get.data.frame(nongraph)


p=1
commonNode1 <- function(node1,node2,graph){
  node1_row <- graph[node1,]
  node2_row <- graph[node2,]
  common_nodes <- vector(mode = 'numeric')
  p <-0
  for(i in 1:nrow(graph)){
    if(node1_row[i]==node2_row[i] & node1_row[i] ==1){
      
      # common_nodes[p] <- i
      p = p+1;
    }
  }
  
  p
  
}
totalNodes<-function(node1,node2,graph){
  node1_row <- graph[node1,]
  node2_row <- graph[node2,]
  p=1
  totalNodes <- vector(mode = 'numeric')
  for(i in 1:nrow(graph)){
    if((node2_row[i]==1) || (node1_row[i]==1))
    {
      totalNodes[p]=i
      p=p+1
    }
    #  if(node1_row[i]==1)
    #  {
    #  totalNodes[p]=i
    #  p=p+1
    #}
    
  }
  #TN<- unique(totalNodes)
  length(totalNodes)/2
  
}
commonNode <- function(node1,node2,graph){
  node1_row <- graph[node1,]
  node2_row <- graph[node2,]
  common_nodes <- vector(mode = 'numeric')
  p <-0
  for(i in 1:nrow(graph)){
    if(node1_row[i]==node2_row[i] & node1_row[i] ==1){
      
      # common_nodes[p] <- i
      p = p+1;
    }
  }
  
  p
  
}
twonodes <- function(node1,node2,graph)
{
  node1_row <- graph[node1,]
  node2_row <- graph[node2,]
  l<-0
  for(k in node1_row)
  {
    l = l + commonNode1(k,node2,graph)
  }
  
  l
}

lp<-function(node1,node2,graph)
{
  a2 <- commonNode1(node1,node2,graph)
  a3 <- twonodes(node1,node2,graph)
  e <- 0.0001
  a2 + e*a3
}




common_auc <- function(data,nodes){
  
  graphmat<-matrix(data=-1,nrow=nodes,ncol=nodes)
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
    graphmat<-matrix(data=-1,nrow=nodes,ncol=nodes)
    for(i in 1:nrow(traindata)){
      graphmat[traindata[i,1],traindata[i,2]]<-1
      graphmat[traindata[i,2],traindata[i,1]]<- 1
    }
    
    CNmat<-matrix(data=-1,nrow=nodes,ncol=nodes)
    for(i in 1:nrow(graphmat)){
      for(j in 1:ncol(graphmat)){
        if(graphmat[i,j]==-1){
          CNmat[i,j]<-lp(i,j,graphmat)
        }
      }
    }
    equal=0
    greater=0
    total=nrow(testdata)*nrow(nonexistlist)
    
    # implementing Precision
    
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
CNAUC<-common_auc(data,nodes)

CNAUC