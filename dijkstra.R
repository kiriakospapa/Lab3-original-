dijkstra <-function(data, init_node){
  
  
  pick_the_smallest_value <- function(subvector, i){
    
    sorted_subvector <- sort(subvector)
    i_smallest_value <- sorted_subvector[i]
    
    return(which(i_smallest_value == subvector))
    
  }  
  
  update_distance_in_pathdata<-function(data,pathdata,node){
    allnodes<-data[,1]
    everynodes<-allnodes[!duplicated(allnodes)]
    neighbor_index_of_current_node<- which(data[,1]==node)
    neighbor_index_init<-which(allnodes %in% node)
    first_index_of_current_node<-first_index_of_current_node <- neighbor_index_init[1]
    closest_node_to_current_node_index <- first_index_of_current_node + which(data[neighbor_index_of_current_node,3] == min(data[neighbor_index_of_current_node,3])) -1
    closest_node_index_in_pathdata<- which(everynodes %in% node)
    pathdata[closest_node_index_in_pathdata,4]<- data[closest_node_to_current_node_index,3]
    return(pathdata)
  }
  
  update_distance <- function(indexes_of_neighbours, distances, visited, data, pathdata, previous_node, first_index_of_current_node){
    for(index in indexes_of_neighbours){
      if(data[index, 2] %in% visited){
        next
      }else{
        new_distance  <- data[index, 3] + pathdata[data[first_index_of_current_node, 1], 4]
        current_distance <- pathdata[data[index, 2], 4]
        if ( new_distance < current_distance){
          pathdata[data[index, 2], 4] <- new_distance
          pathdata[data[index, 2],3] <- data[first_index_of_current_node, 1]
        }
        
        
      }
    }
    
    return(pathdata)
  }
  
  if(is.data.frame(data)==TRUE & init_node %in% data[,1]){
    
    ##setup
    
    allnodes<-data[,1]  #take every elements from first column of input data
    
    unvisited<-allnodes[!duplicated(allnodes)] #find out how many different elements in first column of data
    
    visited<-c() #create a empty vector to store visited nodes later on
    
    init_nodevector<-rep(init_node,length(unvisited)) #create a list of init_noed with the length of every different nodes (for creating path dataframe)
    
    path_length<-rep(Inf,length(unvisited))
    
    previous_nodes<-rep(NA,length(unvisited))
    
    everynodes<-allnodes[!duplicated(allnodes)] #Use to create path dataframe. In order not to confused with unvisited vector.
    
    previous_node <- init_node
    
    
    pathdata<-data.frame(init_nodevector,everynodes,previous_nodes,path_length) #create a path dataframe to make calculation easier to see
    
    
    
    #find the index of nodes which next to init_node
    indexes_of_neighbours<- which(allnodes %in% init_node) #find the row index of init_node
    node_nextto_init <-data[indexes_of_neighbours,2] #find the node next to init_node
    
    
    #Calculation
    #Step1 set the path length to init_node=0
    init_index_ineverynodes<- which(everynodes %in% init_node)
    pathdata[init_index_ineverynodes, 4] <- 0
    pathdata[init_index_ineverynodes, 3] <- init_node
    visited <-append(visited,init_node)
    
    #Step2 calculate the path length next to init_node
    length_of_nodes_next_to_init <- data[indexes_of_neighbours,3] #get the length data from data(wiki_graph)
    
    
    neighbor_index_of_current_node<- which(data[,1]==init_node) #find the row index of init_node
    nodes_nextto_current_node <-data[indexes_of_neighbours,2] #find the node next to init_node
    first_index_of_current_node <- indexes_of_neighbours[1]
    
    closest_node_to_current_node_index <- first_index_of_current_node + which(data[neighbor_index_of_current_node,3] == min(data[neighbor_index_of_current_node,3])) -1 # The variable contains the number of the closest node
    
    distances = data[indexes_of_neighbours, 2]
    closest_node = data[closest_node_to_current_node_index, 2]

    closest_node_index_in_pathdata<- which(everynodes %in% closest_node) #find the index of the closet node in pathdata
    
    pathdata[closest_node_index_in_pathdata,4]<- data[closest_node_to_current_node_index,3] #update the length 
    pathdata[closest_node_index_in_pathdata,3]<- init_node # update the previous node
    previous_node <- closest_node
    
    unvisited <- unvisited[-init_node]
    unvisited <- unvisited[- which( closest_node == unvisited)]
    pathdata <- update_distance(indexes_of_neighbours, distances, visited, data, pathdata, previous_node, first_index_of_current_node)
    
    
    
    while(length(unvisited) > 0 ){
      indexes_of_neighbours<- which(allnodes %in% closest_node) #find the row index of init_node
      
      neighbor_index_of_current_node<- which(data[,1]==closest_node) 
      nodes_nextto_current_node <-data[neighbor_index_of_current_node,2] #find the node next to init_node
      first_index_of_current_node <- indexes_of_neighbours[1]
      
      closest_node_to_current_node_index <- first_index_of_current_node + which(data[neighbor_index_of_current_node,3] == min(data[neighbor_index_of_current_node,3])) -1 # The variable contains the number of the closest node
      distances = data[indexes_of_neighbours, 2]
      previous_node <- data[first_index_of_current_node, 1]
      closest_node = data[closest_node_to_current_node_index, 2]
      
      
      i<-1
      j <- 1
      
      repeat{
        
        stop <- FALSE
        check<- (!(closest_node %in% visited)) #aka if the closest node it unvisited.  It returns just one element
        #i <- i + 1
        
        
        for (boolean_value in check){
          if (boolean_value == FALSE){
            # i <- i + 1 
            
            stop<-TRUE
          } 
        }
        if (stop){
          
          closest_node_to_current_node_index <- first_index_of_current_node + pick_the_smallest_value(data[neighbor_index_of_current_node, 3], i) -1 # The variable contains the number of the closest node
          
          
          distances = data[indexes_of_neighbours, 2]
          
          previous_node <- data[first_index_of_current_node, 1]
          closest_node = data[closest_node_to_current_node_index, 2]
          
          i<-i+1
          next
        }
        pathdata <- update_distance(indexes_of_neighbours, distances, visited, data, pathdata, previous_node, first_index_of_current_node)
        
        i <- 1
        break
      }
      

      visited <-append(visited,closest_node)
      
      closest_node_index_in_pathdata<- which(everynodes %in% closest_node) #find the index of the closet node in pathdata
      
      
 
      
      pathdata[closest_node_index_in_pathdata,3]<- previous_node # update the previous node
      

      unvisited <- unvisited[- which( closest_node == unvisited)]
      return
      
    }
    
return(pathdata[, 4])    
    
    
  }
  else{stop()}
  
}

