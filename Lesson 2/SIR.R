library(deSolve)
library(ggplot2)

sir = function(time,state,parms)
{with(as.list(c(state,parms)),{dS=-beta*S*I; dI=beta*S*I-gamma*I; dR=gamma*I; list(c(dS,dI,dR))})}

plot_sir = function(S0,I0,R0,beta,gamma,days)
{
  time = seq(0,days,by=0.01)
  out = ode(y=c(S=S0,I=I0,R=R0),times=time,func=sir,parms=c(beta=beta,gamma=gamma))
  out <- as.data.frame(out)
  time <- c(out$time, out$time, out$time)
  pop <- c(out$S,out$I,out$R)
  S <- sample ("S", length(out$time), replace = T)
  I <- sample ("I", length(out$time), replace = T)
  R <- sample ("R", length(out$time), replace = T)
  state <- c(S, I, R)
  out_df <- data.frame(time,state,pop)
  ggplot(out_df,aes(x=out_df$time,y=out_df$pop,color=out_df$state)) + geom_line(size=2) +
    theme_minimal() +
    labs(title="",
         x="Days",
         y="Population") +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          axis.title.x = element_text (size = 14, face = "bold", vjust = 0),
          axis.title.y = element_text (size = 14, face = "bold", vjust = 1.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(colour = "black", size=1),
          panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    scale_y_continuous(limits=c(0,1)) +
    scale_color_discrete(name = "Population density", labels = c("Infected", "Recovered", "Susceptible"))
}

plot_sir(S0=.5,I0=.5,R0=0,beta=0.23,gamma=0.01,day=500)


variable_init = function(S0,I0,R0,beta,gamma,days)
{
  time = seq(0,days,by=0.01)
  matrix = ode(y=c(S=S0,I=I0,R=R0),times=time,func=sir,parms=c(beta=beta,gamma=gamma))
  df = as.data.frame(matrix)
  df["conditions"] = paste0("S0: ",S0,", I0: ",I0,", R0: ",R0)
  df["param"] = paste0("beta: ",beta,", gamma: ",gamma)
  return(df)
}

plot_variable_init = function(df,y,ytag,type)
{
  plot = ggplot(df,aes(x=time,y=get(y),color=get(type))) + geom_path(size=1.5) +
    xlab("Days") + ylab(ytag) +
    theme_minimal() + theme(legend.position="none") +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          axis.title.x = element_text (size = 14, face = "bold", vjust = 0),
          axis.title.y = element_text (size = 14, face = "bold", vjust = 1.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(colour = "black", size=1),
          panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    scale_y_continuous(limits=c(0,1))
  return(plot)
}

phase_space_df = data.frame()
for (i in seq(0.1,0.9,0.1))
{phase_space_df = rbind(phase_space_df,variable_init(i,1-i,0,0.23,0.01,500))}
plot_variable_init(phase_space_df,"S","Susceptible","conditions")
plot_variable_init(phase_space_df,"I","Infected","conditions")
plot_variable_init(phase_space_df,"R","Recovered","conditions")
