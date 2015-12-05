
#summary Statistics 
#author narendra


#mean Function
my.mean <- function(x) {
  return(sum(x) / length(x))
}


#medain function
my.median <- function(x) {
  sorted.x <- sort(x)
  
  if (length(x) %% 2 == 0)
  {
    indices <- c(length(x) / 2, length(x) / 2 + 1)
    return(mean(sorted.x[indices]))
  }
  else
  {
    index <- ceiling(length(x) / 2)
    return(sorted.x[index])
  }
}

#varianc function
my.var <- function(x) {
  m <- mean(x)
  return(sum((x - m) ^ 2) / (length(x) - 1))
}

#standrad deviation
my.sd <- function(x) {
  return(sqrt(my.var(x)))
}


