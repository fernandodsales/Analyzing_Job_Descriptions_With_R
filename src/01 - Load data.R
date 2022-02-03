# Social Media Analytics
#Fernando Delgado, Ahmadomar Nakib, Nour Azar

setwd("C:/Users/fdelgado/OneDrive - IESEG/Documents/01. IESEG/12. Social Media Analytics/Group Project")

#===============================================================================
# Libraries
#===============================================================================

#Data manipulation
for (i in c('dplyr','tidytext','tidyverse','data.table')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

#Visualization
for (i in c('ggplot2','scales','maps','maptools','ggmap')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

#Text mining
for (i in c('SnowballC','slam','tm','Matrix', 'hunspell','purrr', 'openNLP', 'NLP', 'wordcloud', 'udpipe', 'textrank')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

#===============================================================================
# Read in Data
#===============================================================================

#Read files
files <- list.files("./data/raw/concept_extraction_cvs",full=T)

#Create an id variable
Jobs <- data.frame(id = c(1:length(files))) 

#Create a job variable with all descriptions
for(i in 1:length(files)){
  Jobs[i,"job"] <- readChar(files[i],file.info(files[i])$size) 
}

#Export csv
write.csv(Jobs, "./data/processed/Jobs.csv")

#Test
print(Jobs[2,2])
