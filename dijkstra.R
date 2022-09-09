dijkstra <-function(data, init_node){
  if(is.data.frame(data)==TRUE & init_node %in% data[,1]){
    
    ##setup
    allnodes<-data[,1]  #take every elements from first column of input data
    unvisited<-allnodes[!duplicated(allnodes)] #find out how many different elements in first column of data
    visited<-c() #create a empty vector to store visited nodes later on
    init_nodevector<-rep(init_node,length(unvisited)) #create a list of init_noed with the length of every different nodes (for creating path dataframe)
    path_length<-rep(Inf,length(unvisited))
    previous_nodes<-rep(NA,length(unvisited))
    everynodes<-allnodes[!duplicated(allnodes)] #Use to create path dataframe. In order not to confused with unvisited vector.
    pathdata<-data.frame(init_nodevector,everynodes,previous_nodes,path_length) #create a path dataframe to make calculation easier to see
   # print(pathdata)
    
    
    #find the index of nodes which next to init_node
    neighbor_index_init<- which(allnodes %in% init_node) #find the row index of init_node
    node_nextto_init <-data[neighbor_index_init,2] #find the node next to init_node
    
    
    #Calculation
      #Step1 set the path length to init_node=0
    init_index_ineverynodes<- which(everynodes %in% init_node)
    pathdata[init_index_ineverynodes, 4] <- 0
    pathdata[init_index_ineverynodes, 3] <- init_node
      #Step2 calculate the path length next to init_node
    length_of_nodes_next_to_init <- data[neighbor_index_init,3] #get the length data from data(wiki_graph)
    
    i<-1
    neighbor_index_init<- which(initnode %in% init_node) #find the row index of init_node
    node_nextto_init <-data[neighbor_index_init,2] #find the node next to init_node

    distances = data[neighbor_index_init, 3]


    while(length(unvisited) == 0 ){

        neighbor_index_init<- which(allnodes %in% init_node) #find the row index of init_node


    }
    
    
     print(length_of_nodes_next_to_init)  
      
    
    
   print(pathdata)   
  }
    else{stop()}
    
  }













###testing
wiki_graph <-data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
                   v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
                   w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

dijkstra(wiki_graph,2)