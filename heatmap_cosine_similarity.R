# 20210127
# Compute heatmap between 2 analysis from Fukan System.
# Cristian Mejia. Kajikawa Laboratory. Tokyo Institute of Technology
# mejia.c.aa@m.titech.ac.jp

#########################################################################
# Context
# Fukan System is a web app for conducting citation network analysis on scientific literature.
# In Fukan System is possible to compare 2 analysis by creating a heatmap. A heatmap is a similarity matrix
# between the keywords of every pair of clusters. Usually, the cosine similarity is used to obtain the heatmap.

# Fukan System currently does not offer an option to save the similarity matrix. Thus, this code
# was created to compute an alternative similarity matrix also based on cosine similarity.

# The numbers of this code's matrix and the heatmap in Fukan System will not be exactly the same. But the 
# Heatmap pattern will remain the same. 

##########################################################################
# Inputs
# Go to Fukan System and download the results of the 2 analysis to compare.
# Unzip the files. You will have a folder with multiple documents in it. 1 folder per each analysis.

##########################################################################
# Output
# A file named "heatmap.csv" which contains the cosine similarity matrix. This file can be opened in Excel.

###########################################################################
# Preparation
###########################################################################
# Note: the first time you run this in your PC it might take a while.
# Install the necessary libraries if not yet in the system.
if(!"dplyr"        %in% rownames(installed.packages())) {install.packages("dplyr")}
if(!"tm"           %in% rownames(installed.packages())) {install.packages("tm")}
if(!"slam"         %in% rownames(installed.packages())) {install.packages("slam")}

# Load libraries
library(dplyr)
library(tm)
library(slam)

# Open the analysis folder for the X axis. The unzipped folder.
paths_to_files_x = list.files(path = choose.dir(), full.names= TRUE, pattern = "*keyword.[[:digit:]]*.tsv", recursive = TRUE)

# Open the analysis folder for the Y axis. The unzipped folder.
paths_to_files_y = list.files(path = choose.dir(), full.names= TRUE, pattern = "*keyword.[[:digit:]]*.tsv", recursive = TRUE)

###########################################################################
# Execution
###########################################################################
# !!!! Just run the code until the end.
# !!!! No need to change anything. 
# !!!! Warning messages (警告メッセージ) will appear. Ignore them. There is no problem.

# Auxiliary functions.
# Sometimes R reads the clusters' file names in a random order. This helps to fix that.
extract_numeric_order <- function(a_list_of_paths) {
  tmp <- strsplit(a_list_of_paths[[1]], split = "\\.")[[1]]
  tmp <- tmp[length(tmp) - 1]
  tmp <- as.numeric(tmp)
  return(tmp)
}

# Read files and format them properly
read_keyword_files <- function(a_list_of_paths) {
  df <- lapply(a_list_of_paths, function(x) {
    tmp <- read.table(x, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
    tmp <- tmp[tmp$CC > 1,]
    tmp <- tmp[,c(1,2)]
    colnames(tmp) <- c("keywords", "tfidf_score")
    return(tmp)
  })
  return(df)
}

# Merge the list of keywords per cluster as a single dataframe
get_dtm_dataframes <- function (x) {
  lapply(x, function(x) {
    tmp <- x[!is.na(x[,1]),]
    tmp <- tmp[nchar(as.character(tmp[,1])) > 0,]
    tmp <- tmp[c(1:min(500, nrow(tmp))),]
    normalized <- normalize_vector(tmp[,2])
    framed <- data.frame(t(matrix(normalized)))
    colnames(framed) <- tmp[,1]
    return(framed)
  })
}

# Normalize a vector
normalize_vector <- function (x) {
  return(x/sqrt(sum(x^2, na.rm = TRUE)))
}


###########################################################################
# Correct order of files
paths_to_files_x <- paths_to_files_x[order(sapply(paths_to_files_x, extract_numeric_order))]
paths_to_files_y <- paths_to_files_y[order(sapply(paths_to_files_y, extract_numeric_order))]

# Get the data
x_axis <- read_keyword_files(paths_to_files_x)
y_axis <- read_keyword_files(paths_to_files_y)

# Prepare the dataframe
Y_AXIS_dtms <- get_dtm_dataframes(y_axis)
X_AXIS_dtms <- get_dtm_dataframes(x_axis)
ALL_dtms <- append(Y_AXIS_dtms, X_AXIS_dtms)
complete_dtm <- bind_rows(ALL_dtms)
complete_dtm_logic <- complete_dtm > 0
complete_dtm <- complete_dtm[,(colSums(complete_dtm_logic, na.rm = TRUE) > 1)]
for (i in c(1:ncol(complete_dtm))) {
  complete_dtm[is.na(complete_dtm[,i]),i] <- 0
}

# Cosine similarity
tdm <- as.TermDocumentMatrix(t(as.matrix(complete_dtm)), weighting = weightTf)
xross <- crossprod_simple_triplet_matrix(tdm)
heatmap <- xross[1:length(y_axis), (1 + length(y_axis)):ncol(xross)]

# Write the heatmap
write.csv(heatmap, file = "heatmap.csv", row.names = FALSE)

# DONE.
# Your file is here:
getwd()
