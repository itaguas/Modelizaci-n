library (igraph)
library (ggplot2)
library(scales)

#g <- sample_gnp(200, 8/200, directed = FALSE, loops = FALSE)
#g <- sample_smallworld(1, 200, 5, 8/200)
#g <- simplify(g) #Just for smallworld
g <- sample_pa(200, m=4)
people <- paste0("P", as.character(1:200))
V(g)$name <- people
graph_df <- as_data_frame(g)
graph_df$pass <- 0
graph_df$cf <- 0
for (i in 1:nrow(graph_df)) {
  graph_df[i,4] <- sample (c(0, 1), prob = c(0.9, 0.1), size = 1)
}

add_df <- data.frame(people)
add_df$knows <- 0
vert <- sample(1:200,1)
add_df[vert,2] <- 1
add_df$type <- "I"
add_df[vert,3] <- "SP"

edge_col <- function(add_df, graph_df) {
  add_list <- c()
  for (i in 1:nrow(add_df)) {
    if (add_df[i,3] == "SP") {
      add_list <- c(add_list, as.character(add_df[i,1]))
      for (j in 1:nrow(graph_df)) {
        if (graph_df[j,1] == add_df[i,1] | graph_df[j,2] == add_df[i,1]) {
          graph_df[j,3] <- 1
        }
      }
    } else if (add_df[i,3] == "ST") {
      add_list <- c(add_list, as.character(add_df[i,1]))    ####
      for (j in 1:nrow(graph_df)) {
        if (graph_df[j,1] == add_df[i,1] | graph_df[j,2] == add_df[i,1]) {
          graph_df[j,3] <- 2
        }
      }
    }
  }
  for (i in 1:nrow(graph_df)) {
    if (graph_df[i,3] == 1) {
      if (graph_df[i,2] %in% add_list & graph_df[i,1] %in% add_list) {
        graph_df[i,3] <- 3
      }
    }
  }
  return (graph_df)
}

graph_df <- edge_col(add_df, graph_df)

net <- graph_from_data_frame(d=graph_df, vertices=add_df, directed=F)

V(net)$size <- 5

V(net)$label <- NA

E(net)$color <- ifelse(E(net)$cf == 0, "grey", 
                       "darkblue")

graph_layout <- layout_with_graphopt(g)

plot(net, margin = c(0,0,0,0), asp = 0, layout=graph_layout, main = "Preferential attachment network")

V(net)$color <- ifelse(V(net)$type == "I", "grey",
                       ifelse(V(net)$type == "SP","red",
                              "black"))

E(net)$color <- ifelse(E(net)$pass == 1, "red", 
                       NA)

plot(net, margin = c(0,0,0,0), asp = 0, layout=graph_layout, main = "t = 0")

vertex_col <- function(add_df, graph_df) {
  add_df_1 <- add_df
  for (i in 1:nrow(add_df)) {
    if (add_df[i,3] == "SP") {
      for (j in 1:nrow(graph_df)) {
        if (graph_df[j,1] == add_df[i,1]) {
          for (k in 1:nrow(add_df)) {
            if (add_df[k,1] == graph_df[j,2] & add_df_1[k,2] == 0 & graph_df[j,4] == 0) {
              tell <- sample (c(T, F), prob = c(0.8, 0.2), size = 1)
              if (tell == T) {
                add_df_1[k,2] <- 1
                add_df_1[k,3] <- sample(c("SP","ST"), prob = c(0.7,0.3), size = 1) 
              }
            } else if (add_df[k,1] == graph_df[j,2] & add_df_1[k,2] == 0 & graph_df[j,4] == 1) {
              add_df_1[k,2] <- 1
              add_df_1[k,3] <- sample(c("SP","ST"), prob = c(0.7,0.3), size = 1)
            }
          }
        } else if (graph_df[j,2] == add_df[i,1]) {
          for (k in 1:nrow(add_df)) {
            if (add_df[k,1] == graph_df[j,1] & add_df_1[k,2] == 0 & graph_df[j,4] == 0) {
              tell <- sample (c(T, F), prob = c(0.5, 0.5), size = 1)
              if (tell == T & add_df_1[k,2] == 0) {
                add_df_1[k,2] <- 1
                add_df_1[k,3] <- sample(c("SP","ST"), prob = c(0.7,0.3), size = 1) 
              }
            } else if (add_df[k,1] == graph_df[j,1] & add_df_1[k,2] == 0 & graph_df[j,4] == 1) {
              tell <- sample (c(T, F), prob = c(0.9, 0.1), size = 1)
              if (tell == T & add_df_1[k,2] == 0) {
                add_df_1[k,2] <- 1
                add_df_1[k,3] <- sample(c("SP","ST"), prob = c(0.7,0.3), size = 1) 
              }
            }
          }
        }
      }
    }
  }
  return (add_df_1)
}

t=1
I <- c()
while (sum(add_df$knows)<nrow(add_df)) {
  print(t)
  prev_sum <- sum(add_df$knows)
  add_df <- vertex_col(add_df, graph_df)
  if (sum(add_df$knows) == prev_sum) {
    t=t+1
    break #Either everyone who migh know already does, or the rumor has gone out of style
  }
  graph_df <- edge_col(add_df, graph_df)
  assign(paste0("net_", t), graph_from_data_frame(d=graph_df, vertices=add_df, directed=F))
  I <- c(I, sum(add_df$knows))
  t=t+1
}

for (i in 1:(t-1)) {
  net <- get(paste0("net_", i))
  V(net)$size <- 5
  V(net)$color <- ifelse(V(net)$type == "I", "grey",
                         ifelse(V(net)$type == "SP","red",
                                "black"))
  V(net)$label <- NA
  E(net)$color <- ifelse(E(net)$pass == 1, "red",
                                NA)
  title <- paste0("t = ", i)
  plot(net, margin = c(0,0,0,0), asp = 0, layout=graph_layout, main = title)
}

E(net)$color <- ifelse(E(net)$cf == 0, "grey", 
                       "darkblue")

plot(net, margin = c(0,0,0,0), asp = 0, layout=graph_layout, main = "Final graph")

I <- c(1,I)
t=0:6
b <- c(1)
b <- c(b,b[1]+8*0.1+8*0.9*0.8)
for (i in 3:length(t)) {
  k <- b[i-1]*0.7*(8*0.1+8*0.9*0.8)
  b <- c(b, b[i-1]+k)
}
df <- data.frame(t,b, I)

ggplot(df, mapping = aes(x=t)) + 
  geom_line(mapping = aes(y = b, colour = "forestgreen"), size = 2) + 
  geom_line(mapping = aes(y = I, colour = "steelblue"), size = 2) +
  theme_minimal() +
  labs(title="",
       x="Time",
       y="Number of people that know the rumor") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text (size = 14, face = "bold", vjust = 0),
        axis.title.y = element_text (size = 14, face = "bold", vjust = 1.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black", size=1),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  coord_cartesian(xlim = c(0,6), ylim=c(1,200)) +
  scale_x_discrete(limits=0:6) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_color_discrete(name = "Evolution", labels = c("Theoretical", "Network"), position="top")

eig <- eigen_centrality(g)
df <- data.frame (eig)


ggplot(df, aes(x=vector))+
  geom_density(color="darkblue", fill="lightblue") +
  theme_minimal() +
  labs(title="Eigenvector centrality distribution",
       x="Eigenvector centrality",
       y="Density") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text (size = 14, face = "bold", vjust = 0),
        axis.title.y = element_text (size = 14, face = "bold", vjust = 1.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black", size=1),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
  
