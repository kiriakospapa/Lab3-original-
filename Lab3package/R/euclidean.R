#' Find the greatest common divisor of two numbers. 
#' 
#' @param a A number.
#' @param b A number.
#' @return greatest common divisor of 'a' & 'b'.
#' @examples
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)

euclidean <- function(a, b){
  
  biggest <- b
  smallest <- a
  
  if (a > b ){
    biggest <- a
    smallest <- b
  }
  
  reminder = biggest %% smallest
  remainders_list<- c(a, b, reminder)
  i <- 1
  while( remainders_list[i] > 0) {
   
   remainders_list[i+2] <- remainders_list[i] %% remainders_list[i+1]
   i <- i + 1
      
  }
  
  return(remainders_list[i-1])
}
