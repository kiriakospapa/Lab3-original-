dijkstra <-function(data,init_node){
  if(is.data.frame(data)==TRUE & init_node %in% data[,1]){
    
    
    allnodes<-data[,1]  #take every elements from first column of input data
    unvisited<-allnodes[!duplicated(allnodes)] #find out how many different elements in first column of data
    visited<-c() #create a empty vector to store visited nodes later on
    init_nodevector<-rep(init_node,length(unvisited)) #create a list of init_noed with the length of every different nodes (for creating path dataframe)
    path_length<-rep(999,length(unvisited))
    previous_nodes<-rep(NA,length(unvisited))
    everynodes<-allnodes[!duplicated(allnodes)] #Use to create path dataframe. In order not to confused with unvisited vector.
    pathdata<-data.frame(init_nodevector,everynodes,previous_nodes,path_length) #create a path dataframe to make calculation easier to see
    
    print(pathdata)
    
    
    
  }
  
  else{stop("input not correct")}
}


###testing
wiki_graph <-data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
                   v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
                   w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

dijkstra(wiki_graph,2)