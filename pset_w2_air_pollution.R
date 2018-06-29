pollutantmean <- function(directory, pollutant, id=1:332) {
  # Create vector of files then read into single dataframe
  files <- list.files(path=directory)
  ds <- {}
  for (i in id) {
    ds <- rbind(ds, read.csv(paste(directory, "/", files[i], sep="")))
  }
  #ds <- ds[complete.cases(ds),]
  mean(ds[[pollutant]], na.rm=TRUE)
}

complete <- function(directory, id=1:332) {
  files <- list.files(path=directory)
  nobs <- data.frame("id"=integer(), "nobs"=integer())
  ds <- {}
  for (i in id) {
    ds <- rbind(ds, read.csv(paste(directory, "/", files[i], sep="")))
    ds <- ds[complete.cases(ds),]
    count <- nrow(ds[ds$ID==i,])
    newrow <- c(i, count)
    nobs[nrow(nobs) + 1,] <- newrow
  }
  return(nobs)
}

corr <- function(directory, threshold=0) {
  # Scan through data in directory and determine files with number of
  # complete cases that meets threshold, return a vector of 
  # nitrate to sulfate correlation.
  datafiles <- list.files(path=directory, pattern="*.csv")
  corrs <- numeric()
  for (f in datafiles) {
    ds <- read.csv(paste(directory, "/", f, sep=""))
    ds <- ds[complete.cases(ds),]
    count <- nrow(ds)
    if (count >= threshold) {
      correlation <- cor(ds$sulfate, ds$nitrate)
      corrs <- c(corrs, correlation) 
    }
  }
  return(corrs)
}
