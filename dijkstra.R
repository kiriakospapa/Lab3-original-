dijkstra <-function(data, init_node){

  #FUNCTIONS---------------------------------------
  
  what_is_next_to_node <-function(data,node){
    allnodes<-data[,1]  #take every elements from first column of input data
    everynodes<-allnodes[!duplicated(allnodes)] #Use to create path dataframe. In order not to confused with unvisited vector.
    neighbor_index_init<- which(allnodes %in% node) #find the row index of node
    node_nextto_it <-data[neighbor_index_init,2] #find the node next to node
    
    
    return(node_nextto_it)
  }
  
  distance_between_node <-function(data,node){
    allnodes<-data[,1]  #take every elements from first column of input data
    everynodes<-allnodes[!duplicated(allnodes)] #Use to create path dataframe. In order not to confused with unvisited vector.
    neighbor_index_init<- which(allnodes %in% node) #find the row index of node
    distance_between_it<-data[neighbor_index_init,3]
    
    return(distance_between_it)
  }
  
  find_closest_node <-function(data,node){
    subvector<- distance_between_node(data,node)
    smallest_value<- pick_the_smallest_value(subvector,1)
    allnodes<-data[,1]
    neighbor_index_init<- which(allnodes %in% node)
    neighbor_index_of_current_node<- which(data[,1]==node)
    first_index_of_current_node<-first_index_of_current_node <- neighbor_index_init[1]
    closest_node_to_current_node_index <- first_index_of_current_node + which(data[neighbor_index_of_current_node,3] == min(data[neighbor_index_of_current_node,3])) -1
    closest_node = data[closest_node_to_current_node_index, 2]
    
    return(closest_node)
  }
  
  distance_to_closest_node<-function(data,node){
    subvector<- distance_between_node(data,node)
    distance<-min(subvector)
    return(distance)
  }
  
  update_distance_in_pathdata<-function(data,pathdata,node){
    allnodes<-data[,1]
    everynodes<-allnodes[!duplicated(allnodes)]
    neighbor_index_of_current_node<- which(data[,1]==node)
    first_index_of_current_node<-first_index_of_current_node <- neighbor_index_init[1]
    closest_node_to_current_node_index <- first_index_of_current_node + which(data[neighbor_index_of_current_node,3] == min(data[neighbor_index_of_current_node,3])) -1
    closest_node_index_in_pathdata<- which(everynodes %in% node)
    pathdata[closest_node_index_in_pathdata,4]<- data[closest_node_to_current_node_index,3]
    return(pathdata)
  }
  
  update_privious_node_in_pathdata<-function(data,pathdata,node){
    allnodes<-data[,1]  
    everynodes<-allnodes[!duplicated(allnodes)]
    closest_node_index_in_pathdata<- which(everynodes %in% node)
    closest_node<-find_closest_node <-function(data,node)
    pathdata[closest_node_index_in_pathdata,3]<- closest_node
    return(pathdata)
  }
  
  pick_the_smallest_value <- function(subvector, i){
    
    sorted_subvector <- sort(subvector)
    i_smallest_value <- sorted_subvector[i]
    
    return(which(i_smallest_value == sorted_subvector))
    
  }  
  
#END OF FUNCTIONS--------------------------------------- 
  
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
    
    
    # #find the index of nodes which next to init_node
    neighbor_index_init<- which(allnodes %in% init_node) #find the row index of init_node
    node_nextto_init <-data[neighbor_index_init,2] #find the node next to init_node

    #Calculation
    #Step1 set the path length to init_node=0
    init_index_ineverynodes<- which(everynodes %in% init_node)
    pathdata[init_index_ineverynodes, 4] <- 0
    pathdata[init_index_ineverynodes, 3] <- init_node
    visited <-append(visited,init_node)
   
    
    #Step2 calculate the path length next to init_node
    what_is_next_to_node(data,init_node)
    distance_between_node(data,init_node)
    closest_node =  find_closest_node(data,init_node)
    visited <- append(visited, closest_node)
    # length_of_nodes_next_to_init <- data[neighbor_index_init,3] #get the length data from data(wiki_graph)
    # neighbor_index_of_current_node<- which(data[,1]==init_node) #find the row index of init_node
    # nodes_nextto_current_node <-data[neighbor_index_init,2] #find the node next to init_node
    # first_index_of_current_node <- neighbor_index_init[1]
    # 
    # index_closest_node_to_current_node <- first_index_of_current_node + min(data[neighbor_index_of_current_node,2]) # min returns the index
    # closest_node_to_current_node_index <- first_index_of_current_node + which(data[neighbor_index_of_current_node,3] == min(data[neighbor_index_of_current_node,3])) -1 # The variable contains the number of the closest node
    # 
    # distances = data[neighbor_index_init, 2]
    
    
    # closest_node_index_in_pathdata<- which(everynodes %in% closest_node) #find the index of the closet node in pathdata
    # 
    
    # unvisited[closest_node_to_current_node_index] <- NULL
    #unvisited <- within(unvisited, rm(closest_node))
    unvisited <- unvisited[-init_node]
    unvisited <- unvisited[- which( closest_node == unvisited)]
    
    # node_nextto_init <-data[neighbor_index_init,2] #find the node next to init_node
    
    
    i<-1
    while(length(unvisited) > 0 ){
      neighbor_index_init<- which(allnodes %in% closest_node) #find the row index of closest_node
      
      neighbor_index_of_current_node<- which(data[,1]==closest_node) 
      nodes_nextto_current_node <-data[neighbor_index_of_current_node,2] #find the node next to current_node
      first_index_of_current_node <- neighbor_index_init[1]
      #index_closest_node_to_current_node <- first_index_of_current_node + min(data[neighbor_index_of_current_node,2]) # min returns the index
      closest_node_to_current_node_index <- first_index_of_current_node + which(data[neighbor_index_of_current_node,3] == min(data[neighbor_index_of_current_node,3])) -1 # The variable contains the number of the closest node
      
      
      distances = data[neighbor_index_init, 2]
      closest_node = data[closest_node_to_current_node_index, 2]

      repeat{
        stop <- FALSE
        check<- (!(closest_node %in% visited))

        for (boolean_value in check){
          if (boolean_value == FALSE){
            stop<-TRUE
            break
          } 
        }
        if (stop){
          break
        }
        i <- i + 1
        closest_node_to_current_node_index <- first_index_of_current_node + pick_the_smallest_value(data[neighbor_index_of_current_node, 3]) -1 # The variable contains the number of the closest node
        distances = data[neighbor_index_init, 2]
        closest_node = data[closest_node_to_current_node_index, 2]
        
        
        
        
      }
      ## WE NEED TO CHECK OUT IF THE CLOSEST NODE IS IN UNVISITED VECTOR
      
      visited <-append(visited,closest_node)
      
      closest_node_index_in_pathdata<- which(everynodes %in% closest_node) #find the index of the closet node in pathdata
      
      pathdata[closest_node_index_in_pathdata,4]<- data[closest_node_to_current_node_index,3] #update the length 
      pathdata[closest_node_index_in_pathdata,3]<- closest_node # update the previous node
      # unvisited[closest_node_to_current_node_index] <- NULL
      #unvisited <- within(unvisited, rm(closest_node))
      unvisited <- unvisited[- which( closest_node == unvisited)]
      

      
    }
    
    
    
    
  }
  else{stop()}
  
}













###testing
wiki_graph <-data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
                        v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
                        w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

dijkstra(wiki_graph,6)
