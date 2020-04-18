# Code for obtaining ngrams of size 4, 3, and 2 from Web of Science data.
# Tokyo Institute of Technology, Graduate School of Environment and Society, Kajikawa Laboratory
# cristianmejia00@gmail.com
# 20200418 Distribution version.

# Input:
# A dataset file from the Web of Science. With extensions .txt .tsv or .csv

# Output:
# 3 .csv files that can be opened in Excel, one file for each 4, 3, and 2 keywords-long ngrams.

# Notes:
# This code was developed for files downloaded from the Web of Science.
# If multiple files were downloaded, please merge them firts using other codes or Excel.
# If you plan to use this code with other files, like patents from Derwent Innovation, you need to change the column names in the line 47 below. 
# This file will shown several "warning" messages. These are not errors. Please just ignore the warnings. 
# This code was tested in Windows 10. Don't know if works in MAC or Linux. 

#######################################################################
# Call libraries
#######################################################################
# Note: the first time you run this in your PC it might take a while.
if(!"plyr"       %in% rownames(installed.packages())) {install.packages("plyr")}
if(!"dplyr"      %in% rownames(installed.packages())) {install.packages("dplyr")}
if(!"data.table" %in% rownames(installed.packages())) {install.packages("data.table")}
if(!"tm"         %in% rownames(installed.packages())) {install.packages("tm")}
if(!"ngram"      %in% rownames(installed.packages())) {install.packages("ngram")}

library(plyr)
library(dplyr)
library(data.table)
library(tm)
library(ngram)

#######################################################################
# Read the data
#######################################################################
# To read a file from Web of Science (.txt) in your local machine in Windows use this, and select your file
dataset <- fread(file.choose(), stringsAsFactors = FALSE)

# Or use my sample (Remove the "#" symbol in the next line)
#dataset <- fread('https://raw.githubusercontent.com/cristianmejia00/kajikawa_lab/master/test_data_WOS.csv', stringsAsFactors = FALSE, fill = TRUE)

#######################################################################
# Prepare the text
#######################################################################
# In this secuence:
# -- Unify Title and Abstract
# ---- Convert text to "tm" object
# ------ To lowercase
# ------ Remove stopwords
# ------ Remove numbers and symbols
# ------ Remove extra whitespaces
# ---- Convert "tm" object to text

documents <- paste(dataset$TI, dataset$AB, sep = ". ")
text <- Corpus(VectorSource(documents)) %>%
        tm_map(content_transformer(tolower)) %>% 
        tm_map(removeWords, stopwords("english")) %>% 
        tm_map(removeNumbers) %>% 
        tm_map(removePunctuation) %>%
        tm_map(stripWhitespace)
text <- unlist(sapply(1:length(text), function(x){return(text[[x]]$content)}))

#######################################################################
# Obtain ngrams and write reports
#######################################################################
# ngrams are computed with the text of all articles as one single string.
bulk_text <- paste(text, collapse = " ")

# Get the ngrams for 4, 3, and 2 words
# Report only those ngrams appearing 3 times or more in all the dataset.
ngram4 <- get.phrasetable(ngram(bulk_text, n = 4)) %>% .[.$freq >= 3,]
ngram3 <- get.phrasetable(ngram(bulk_text, n = 3)) %>% .[.$freq >= 3,]
ngram2 <- get.phrasetable(ngram(bulk_text, n = 2)) %>% .[.$freq >= 3,]

# Write files
write.csv(ngram4, file="ngram4.csv", row.names = FALSE)
write.csv(ngram3, file="ngram3.csv", row.names = FALSE)
write.csv(ngram2, file="ngram2.csv", row.names = FALSE)

# Check folder, your files are located there:
getwd()