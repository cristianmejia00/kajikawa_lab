# Clusterize a keyword document cooccurrence matrix.
# cristianmejia00@gmail.com
# 20200428

# Needs a .csv file of keywords cooccurrences with the following format:
# Squared matrix (This means same number of rows and columns)
# rows and column names are the keywords
# Rows and columns are in the same order (keywords from "left to right" are the same from "top to bottom")
# Each intersection contains the number of documents each pair of keywords appear together.

# Libraries
library(plyr)
library(dplyr)
library(igraph)
library(data.table)

# Auxiliar Network Function
# Make comunities in descending order for the desired algorithm.
clusterize <- function(a_network, algorithm = "louvain"){
  net <- simplify(a_network)
  if(algorithm == "louvain") {net <- cluster_louvain(net)}
  if(algorithm == "newman") {net <- cluster_fast_greedy(net)}
  if(algorithm == "infomap") {net <- cluster_infomap(net)}
  com <- membership(net)
  ordered <- table(com) %>% sort(decreasing = TRUE) %>% names %>% as.numeric 
  repl <- sapply(com, function(x) {which(ordered == x)})
  names(repl) <- names(com)
  return(repl)
}

# Read the file with the keywords document cooccureences
keyword_coocurrance_matrix <- read.csv(file.choose(), stringsAsFactors = FALSE, check.names = FALSE) %>% as.matrix
rownames(keyword_coocurrance_matrix) <- keyword_coocurrance_matrix[,1] #Transform the first column to row names.
keyword_coocurrance_matrix <- keyword_coocurrance_matrix[,-1] #Remove the first column, because the matrix must contain only numbers

# Create the network and clusterize it
# The m_com object is the cluster number each word belongs to.
g2 <- graph.adjacency(keyword_coocurrance_matrix, mode="undirected", weighted=TRUE, diag = FALSE)
m_com <- clusterize(g2)
m_com
