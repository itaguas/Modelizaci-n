#To convert a number to binary
library(R.utils)

#To draw the plot
library(ggplot2)
library(reshape2)

#We stablish the number of colums
cell <- 257

#We create a matrix with the number of columns and rows desired, all values set to FALSE
#We will use FALSE as 0 (white) and TRUE as 1 (black)
ca <- matrix (FALSE, nrow = 200, ncol = cell)

#We write the number of the rule
rule_dec <- 137

#Using the number, we generate a vector with the corresponding binary number
rule_binary <- as.numeric(strsplit(intToBin(rule_dec), "")[[1]])

#We make sure the vector has length 8; otherwise, we fill it with 0s in the first positions
while (length(rule_binary)<8) {
  rule_binary <- c(0, rule_binary)
}

#We change the 0s for FALSE and the 1s for TRUE
rule <- as.logical(rule_binary)

#We define the function to apply the rule
#This function takes as input a matrix with values TRUE or FALSE and a binary vector of length 8
#The function goes over every value of the matrix (except the first row, which corresponds to initial conditions) applying the rule
#The cells in the first and last columns are taken as neighbors.
#The function returns the modified matrix
ca_rule <- function(ca, rule) {
  for (i in 2:nrow(ca)) {
    print (i)
    for (j in 1:ncol(ca)) {
      if (j==1){
        if (ca[i-1,ncol(ca)] == TRUE) {
          if (ca[i-1,j] == TRUE) {
            if (ca[i-1,j+1] == TRUE) {
              ca[i,j] <- rule[1]
            } else {
              ca[i,j] <- rule[2]
            }
          } else {
            if (ca[i-1,j+1] == TRUE) {
              ca[i,j] <- rule[3]
            } else {
              ca[i,j] <- rule[4]
            }
          }
        } else {
          if (ca[i-1,j] == TRUE) {
            if (ca[i-1,j+1] == TRUE) {
              ca[i,j] <- rule[5]
            } else {
              ca[i,j] <- rule[6]
            }
          } else {
            if (ca[i-1,j+1] == TRUE) {
              ca[i,j] <- rule[7]
            } else {
              ca[i,j] <- rule[8]
            }
          }
        }
      } else if (j==ncol(ca)) {
        if (ca[i-1,j-1] == TRUE) {
          if (ca[i-1,j] == TRUE) {
            if (ca[i-1,1] == TRUE) {
              ca[i,j] <- rule[1]
            } else {
              ca[i,j] <- rule[2]
            }
          } else {
            if (ca[i-1,1] == TRUE) {
              ca[i,j] <- rule[3]
            } else {
              ca[i,j] <- rule[4]
            }
          }
        } else {
          if (ca[i-1,j] == TRUE) {
            if (ca[i-1,1] == TRUE) {
              ca[i,j] <- rule[5]
            } else {
              ca[i,j] <- rule[6]
            }
          } else {
            if (ca[i-1,1] == TRUE) {
              ca[i,j] <- rule[7]
            } else {
              ca[i,j] <- rule[8]
            }
          }
        }
      } else {
        if (ca[i-1,j-1] == TRUE) {
          if (ca[i-1,j] == TRUE) {
            if (ca[i-1,j+1] == TRUE) {
              ca[i,j] <- rule[1]
            } else {
              ca[i,j] <- rule[2]
            }
          } else {
            if (ca[i-1,j+1] == TRUE) {
              ca[i,j] <- rule[3]
            } else {
              ca[i,j] <- rule[4]
            }
          }
        } else {
          if (ca[i-1,j] == TRUE) {
            if (ca[i-1,j+1] == TRUE) {
              ca[i,j] <- rule[5]
            } else {
              ca[i,j] <- rule[6]
            }
          } else {
            if (ca[i-1,j+1] == TRUE) {
              ca[i,j] <- rule[7]
            } else {
              ca[i,j] <- rule[8]
            }
          }
        }
      }
    }
  }
  return(ca)
}


#We set the initial conditions

#The middle cell black
#ca[1,cell/2] <- TRUE

#50% cells black and 50% cells white
#ca[1,] <- sample(c(TRUE,FALSE), size = cell, replace = TRUE, prob = c(0.5,0.5))

#25% cells black and 75% cells white
#ca[1,] <- sample(c(TRUE,FALSE), size = cell, replace = TRUE, prob = c(0.25,0.75))

#90% cells black and 10% cells white
ca[1,] <- sample(c(TRUE,FALSE), size = cell, replace = TRUE, prob = c(0.9,0.1))


#We apply the function
ca <- ca_rule(ca, rule)

#We plot the matrix
melted <- melt(ca)
ggplot(melted, aes(x = Var2, y = Var1, fill = value)) + geom_tile() +
  scale_fill_manual(values = c("white", "black")) +
  theme_bw() +
  scale_y_reverse() +
  theme_void() +
  theme(legend.position = "none")
  
