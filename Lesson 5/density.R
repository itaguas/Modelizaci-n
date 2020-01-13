library (igraph)
library (ggplot2)

time1 <- c()
time2 <- c()
time3 <- c()

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

print("RANDOM")
for (i in 1:20) {
  g <- sample_gnp(200, 8/200, directed = FALSE, loops = FALSE)
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
  
  graph_df <- edge_col(add_df, graph_df)
  
  net <- graph_from_data_frame(d=graph_df, vertices=add_df, directed=F)

  
  t=1
  while (sum(add_df$knows)<nrow(add_df)) {
    prev_sum <- sum(add_df$knows)
    add_df <- vertex_col(add_df, graph_df)
    if (sum(add_df$knows) == prev_sum) {
      t=t+1
      break #Either everyone who migh know already does, or the rumor has gone out of style
    }
    graph_df <- edge_col(add_df, graph_df)
    t=t+1
  }
  time1 = c(time1, t-1)
  print (time1)
}

print("SMALL WORLD")
for (i in 1:20) {
  g <- sample_smallworld(1, 200, 5, 8/200)
  g <- simplify(g) #Just for smallworld
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
  
  graph_df <- edge_col(add_df, graph_df)
  
  net <- graph_from_data_frame(d=graph_df, vertices=add_df, directed=F)
  
  
  t=1
  while (sum(add_df$knows)<nrow(add_df)) {
    prev_sum <- sum(add_df$knows)
    add_df <- vertex_col(add_df, graph_df)
    if (sum(add_df$knows) == prev_sum) {
      t=t+1
      break #Either everyone who migh know already does, or the rumor has gone out of style
    }
    graph_df <- edge_col(add_df, graph_df)
    t=t+1
  }
  time2 = c(time2, t-1)
  print (time2)
}

print("PREF attachment")
for (i in 1:20) {
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
  
  graph_df <- edge_col(add_df, graph_df)
  
  net <- graph_from_data_frame(d=graph_df, vertices=add_df, directed=F)
  
  
  t=1
  while (sum(add_df$knows)<nrow(add_df)) {
    prev_sum <- sum(add_df$knows)
    add_df <- vertex_col(add_df, graph_df)
    if (sum(add_df$knows) == prev_sum) {
      t=t+1
      break #Either everyone who migh know already does, or the rumor has gone out of style
    }
    graph_df <- edge_col(add_df, graph_df)
    t=t+1
  }
  time3 = c(time3, t-1)
  print (time3)
}

group.index <- rep(1:3, c(length(time1), length(time2), length(time3)))

den <- sm.density.compare(c(time1, time2, time3), group = group.index, model = "equal", xlim=c(0,14), main = "Rumor extinction times", xlab = "Generations", col = c("steelblue", "forestgreen", "darkred"))

legend(x=9, y=0.45, c("Random network", "Small world network", "Preferential attachment network"), cex= 0.95, pch = 1, col = c("red","green","blue"), bty = "n")

