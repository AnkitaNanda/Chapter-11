block_prob <- function(){
  
  
  n <- readline("Enter the total number of customers in the population: ")
  n <- as.integer(n)
  s <- readline("Enter the number of servers present in the system: ")
  s <- as.integer(s)
  
  
  
  r <- readline("Enter the rate of arrival of customers to the system in as unit per hour: ")
  r <- as.double(r)
  t <- readline("Enter the average time spent by each customer in the system in hours: ")
  t <- as.double(t)
  a <- r*t
  
  
  deno <- 0
  k <- 0
  for(k in 0:s){
    
    deno <- deno + choose(n,k) * (a ^ k)
    k <- k+1
    
  }
  
  nume <- choose(n,s) * (a ^ s)
  
  B <- nume / deno
  
  print(paste("The blocking probability of the system is given as:", B))
  
  
}

block_prob()