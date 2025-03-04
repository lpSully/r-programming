x <- NULL
m <- NULL

makeVector <- function(x = numeric()) {
  m <- NULL
  
  # Getter to get cached mean
  get <- function() x
  
  # Sets cached mean
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

dog <- makeVector(43)
cachemean(dog)
dog$getmean()
