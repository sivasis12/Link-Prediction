library(matlab)
library(matrixcalc) 
library(igraph)

common_graph<-read_graph("C:/Users/dell/Documents/dolphin.gml",format = c("gml"))
#common_graph<-read_graph("C:/Users/dell/Documents/karate.gml",format = c("gml"))
#common_graph<-read_graph("C:/Users/dell/Documents/lesmis.gml",format = c("gml"))
#common_graph<-read_graph("C:/Users/dell/Documents/football.gml",format = c("gml"))
#common_graph<-read_graph("C:/Users/dell/Documents/polblogs.gml",format = c("gml"))
#common_graph<-read_graph("C:/Users/dell/Documents/adjnoun.gml",format = c("gml"))
#common_graph<-read_graph("C:/Users/dell/Documents/celegansneural.gml",format = c("gml"))
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

commonNode <- function(node1,node2,graph){
  graph1<-graph_from_adjacency_matrix(graph,mode=c("undirected"))
  
  n <- vcount(graph1)
  score <- matrix(integer(n^2), nrow = n)
  
  neighbors <- neighborhood(graph1, 1)
  neighbors <- lapply(neighbors, function(x) x[-1])
  
  degrees <- degree(graph1)
  for (k in seq(n)){
    tmp <- neighbors[[k]]
    l <- degrees[[k]]
    if (l > 1){
      for (i in 1:(l-1)){
        n1 <- tmp[i]
        for (j in (i+1):l){
          n2 <- tmp[j]
          score[n1, n2] <- score[n1, n2] + 1/l
          score[n2, n1] <- score[n2, n1] + 1/l
        }
      }
    }
  }
  score[node1, node2]
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
          CNmat[i,j]<-commonNode(i,j,graphmat)
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
    # result=(greater+0.5*equal)/total
    result=result+(greater+0.5*equal)/total
  }
  print(precision/10)
  
  
  result/10
}
CNAUC<-common_auc(data,nodes)

CNAUC
