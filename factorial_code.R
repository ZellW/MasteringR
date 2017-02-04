#Regular loop
Factorial_loop <- function(x=1) {
     stopifnot(x >= 0)
     if (x == 0) {
          return(1)}
     else{
          output <- 1
          for (y in 1:x) {
               output <- output * y}
          return(output)}
}

#purrr - reduce
#Note:  Below must use numeric - as.integer creates an warning of integer overflow NAs
library(purrr)

Factorial_reduce <- function(x=1){
     stopifnot(x >= 0)
     if (x == 0){
          return(1)}
     else {reduce(as.numeric(1:x), function(x,y){
          x * y})
     }
}

#purrr - Recursion
Factorial_func <- function(x=1){
     stopifnot(x >= 0)
     if(x == 0) {1}
     else {x * Factorial_func(x-1)}
}

#Memoization
Factorial_mem <- function(x=1){
     stopifnot(x >= 0)
     Fact_tbl<- c(1)
     Factorial_mem_in <- function(x=1){
          if(!is.na(Fact_tbl[x])) {Fact_tbl[x]}
          
          else{Fact_tbl[x] <<- x * Factorial_mem_in(x-1)
          return(Fact_tbl[x])}}
}

library(microbenchmark)
##Comparison for n=10
microbenchmark(RegularLoop <- Factorial_loop(10),
               Reduce <- Factorial_reduce(10),
               Recursion <- Factorial_func(10),
               Memoization <- Factorial_mem(10))
##Comparison for n=100
microbenchmark(RegularLoop <- Factorial_loop(100),
               Reduce <- Factorial_reduce(100),
               Recursion <- Factorial_func(100),
               Memoization <- Factorial_mem(100))