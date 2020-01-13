q_map<-function(r=5,x_o=runif(1,0,1),N=100,burn_in=0,...)
{
  par(mfrow=c(2,1),mar=c(4,4,1,2),lwd=2)
  ############# Trace #############
  x<-array(dim=N)
  x[1]<-x_o
  for(i in 2:N)
    x[i]<-r*x[i-1]**2*(1-x[i-1])
  
  plot(x[(burn_in+1):N],type='l',xlab='t',ylab='x', ylim=c(0,1))
  #################################
  
  ##########  Quadradic Map ########
  x<-seq(from=0,to=1,length.out=100)
  x_np1<-array(dim=100)
  for(i in 1:length(x))
    x_np1[i]<-r*x[i]**2*(1-x[i])
  
  plot(x,x_np1,type='l',xlab=expression(x[t]),ylab=expression(x[t+1]))
  abline(0,1)
  
  start=x_o
  vert=FALSE
  lines(x=c(start,start),y=c(0,r*start**2*(1-start)) )
  for(i in 1:(2*N))
  {
    if(vert)
    {
      lines(x=c(start,start),y=c(start,r*start**2*(1-start)) )
      vert=FALSE
    }
    else
    {
      lines(x=c(start,
                r*start**2*(1-start)),
            y=c(r*start**2*(1-start),
                r*start**2*(1-start)) )
      vert=TRUE
      start=r*start**2*(1-start)
    }
  }
  #################################
}

q_map(r=6.4,x_o=0.80001)
