bif_diagram <- function(f=function(x,a) (a*x**2*(1-x)),alow=2.5,ahigh=4,
                        thinness=1000, transient=200, collect=200){
  # f function, parameter must be named a
  n <- 1
  R <- seq(alow,ahigh,length=thinness)
  data <- matrix(0,collect,thinness+1)
  
  for(a in R){
    x <- runif(1) # random initial condition
    ## first converge to attractor
    for(i in 1:transient){
      x <- f(x,a)
    } # collect points on attractor
    for(i in 1:collect){
      x <- f(x,a)
      data[i,n] <- x
    }
    n <- n+1
  }
  
  data <- data[,1:thinness]
  yrange <- range(data)+c(-0.1,0.1)
  plot(R,data[1,], pch=".", xlab="a", ylab="States",ylim=yrange)
  for(i in 2:collect) points(R,data[i,],pch=".")
}

f <- function(x,a) a*x**2*(1-x)
bif_diagram(f,alow=4,ahigh=6.75)
