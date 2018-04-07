# Load the Zalando MNIST recognition dataset into R
# http://yann.lecun.com/exdb/mnist/
# assume you have all 4 files and gunzip'd them
# creates train$n, train$x, train$y  and test$n, test$x, test$y
# e.g. train$x is a 60000 x 784 matrix, each row is one digit (28x28)
# call:  show_digit(train$x[5,])   to see a digit.
# brendan o'connor - gist.github.com/39760 - anyall.org

library(reshape2)
library(ggplot2)

load_mnist <- function() {
  load_image_file <- function(filename) {
    ret = list()
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    ret$n = readBin(f,'integer',n=1,size=4,endian='big')
    nrow = readBin(f,'integer',n=1,size=4,endian='big')
    ncol = readBin(f,'integer',n=1,size=4,endian='big')
    x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
    ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
    close(f)
    ret
  }
  load_label_file <- function(filename) {
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    n = readBin(f,'integer',n=1,size=4,endian='big')
    y = readBin(f,'integer',n=n,size=1,signed=F)
    close(f)
    y
  }
  train <<- load_image_file('train-images-idx3-ubyte')
  test <<- load_image_file('t10k-images-idx3-ubyte')
  
  train$y <<- load_label_file('train-labels-idx1-ubyte')
  test$y <<- load_label_file('t10k-labels-idx1-ubyte')  
}


show_digit <- function(arr784, col=gray(12:1/12), ...) {
  image(matrix(arr784, nrow=28)[,28:1], col=col, ...)
}

#
# plot this number of examples per class
#
showExamples <- function(nExamplesClass = 10, randomize = F) {
  classes <- sort(unique(train$y)) # a vector of all classes
  nClasses <- length(classes) # Number of classes
  
  iExamples <- as.matrix(sapply(classes, 
                                function(i) {
                                  iLab <- which(train$y == i)
                                  sample(x = iLab, size = nExamplesClass, replace = F)
                                }))
  if(randomize) {
    iExamples <- matrix(sample(x = iExamples, size = length(iExamples), replace = F), nrow = dim(iExamples)[1])
  }
  
  data <- array(train$x[iExamples, ], dim = c(nExamplesClass*nClasses, 28, 28))
  
  plotData <- melt(data, varnames = c("image", "x", "y"), value.name = "pixel")
  nrows <- if(randomize) round(sqrt(nExamplesClass*nClasses)) else 10
  p <- ggplot(plotData, aes(x = x, y = y, fill = pixel)) +
    geom_tile() +
    scale_y_reverse() +
    facet_wrap(~ image, nrow = nrows) +
    theme_bw()+
    theme(
      panel.spacing = unit(0, "lines"),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      legend.position   = "none",
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()
    ) + scale_fill_gradient(low="black", high="white")
  
  plot(p)
}

load_mnist()
# show_digit(train$x[1,]) # plot one example of the data
 
showExamples(12, F)
