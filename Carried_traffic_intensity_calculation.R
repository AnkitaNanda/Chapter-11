traff_inten <- function(){
  
  
  n <- readline("Enter the total number of customers in the population: ")
  n <- as.integer(n)
  s <- readline("Enter the number of servers present in the system: ")
  s <- as.integer(s)
  
  
  
  r <- readline("Enter the rate of arrival of customers to the system in as unit per hour: ")
  r <- as.double(r)
  t <- readline("Enter the average time spent by each customer in the system in hours: ")
  t <- as.double(t)
  a <- r*t
  
  pi <- double(s+1)
  
  k <- 0
  deno <- 0
  
  for(k in 0:s){
    
    deno <- deno + choose(n,k) * (a ^ k)
    k <- k+1
    
  }
 
  
 k <- 0 
 c <- 1
 
 for (k in 0:s){
   
   pi[c] <- (choose(n,k) * (a ^ k)) / deno
   k <- k+1
   c <- c+1
   
   
 }
 

 sum <- 0
 k <- 0
 c <- 1
 
 for(k in 0:s){
   
   sum <- sum + k * pi[c]
   c <- c+1
   k <- k+1
 }
 
 print(paste("The carried traffic intensity of the system is:", sum))
 

   
}


traff_inten()