# function that corrects "sample" execution in the case of only one element in the population (more info: ?sample and ?sample.int)
sample2 <- function(x, ...) x[sample.int(length(x), ...)]
