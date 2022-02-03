# Social Media Analytics
#Fernando Delgado, Ahmadomar Nakib, Nour Azar

setwd("C:/Users/fdelgado/OneDrive - IESEG/Documents/01. IESEG/12. Social Media Analytics/Group Project")

#===============================================================================
# Libraries
#===============================================================================

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

#Read in the previously loaded data
Jobs <- fread("./data/processed/Jobs.csv")
Jobs <- subset(Jobs, select = -V1)
#===============================================================================
# Pre-processing
#===============================================================================

#Remove strange symbols
symbols <- c("Â", "â","???", "¢","¦","T","©","´","ã", "*", "â", ".â")

for(i in symbols){
  Jobs$job <- lapply(Jobs$job, function(x) gsub(i, "",x, fixed = TRUE))
  
}

#Remove some acronyms that are identified as sentence stoppers
Jobs$job <- gsub("sr.", "senior", Jobs$job, fixed = TRUE)
Jobs$job <- gsub("Sr.", "senior", Jobs$job, fixed = TRUE)
Jobs$job <- gsub("SR.", "senior", Jobs$job, fixed = TRUE)
Jobs$job <- gsub("dr.", "doctor", Jobs$job, fixed = TRUE)
Jobs$job <- gsub("Dr.", "doctor", Jobs$job, fixed = TRUE)
Jobs$job <- gsub("DR.", "doctor", Jobs$job, fixed = TRUE)

#Set to lowercase
Jobs$job <- tolower(Jobs$job)

#removes trailing whitespace
Jobs$job <- gsub("\\s+"," ",Jobs$job)

#===============================================================================
# Tokenization Test
#===============================================================================

#Tokenize into sentences
test_sentences <- Jobs %>%
  unnest_tokens(sentences, job, token = 'sentences')

#Take only first sentence
first_sentence <-
  test_sentences %>% 
  group_by(id) %>% 
  filter(row_number()==1)

#===============================================================================
# POS Tag
#===============================================================================


## Create udmodel
#this is inspired by this site:
#https://stackoverflow.com/questions/51861346/r-pos-tagging-and-tokenizing-in-one-go

udmodel <- udpipe_download_model(language = "english")
udmodel <- udpipe_load_model(file = udmodel$file_model)

#Extract Nouns from the first sentence of all documents
Nouns <- NULL

for(i in 1:nrow(first_sentence)){
  tmp <- first_sentence$sentences[i]
  print(i)
  
  #fit model to extract POS and set as data frame
  x <- udpipe_annotate(udmodel, tmp)
  x <- as.data.frame(x)
  x %>% select(token, upos)
  
  #Subset Nouns and compounds
  x <- subset(x, x['upos'] == "NOUN" & x['dep_rel'] =="compound")
  x <- subset(x, select = c(doc_id, token))
  
  #rbind into dataframe
  Nouns <- rbind(Nouns, x)
}


#Create a Noun frequency dataframe
nounfreq <- Nouns %>% group_by(token) %>%
  summarize(Count = n())

#subset those repeated at least 10 times
nounfreq <- subset(nounfreq, nounfreq['Count'] > 9 )

#===============================================================================
# Creating a dictionary of positions
#===============================================================================

#Here we export data to csv so we can only keep nouns that refer to a position
write.csv(nounfreq, "./data/processed/nounfreq.csv")

#manually we remove the nouns we dont want with excel and create a dictionary of positions
#this is easy to handle now that our data only contains 700 words
position_dict <- fread("./data/processed/position_dict.csv")


#===============================================================================
# Extract Positions
#===============================================================================

for (i in 1:length(Jobs$job)){    
  #split the text by space  
  split <- strsplit(Jobs$job[i]," ")[[1]]     
  #comparing split with the state abbreviation   
  position <- match(split, position_dict$token)    
  #if it matches, get the position of each state  
  position <- which(!is.na(position))    
  #extract the state based on the position  
  position_split <- split[position]    
  #adding states to the new column   
  Jobs$position[i] <- position_split[1]  
}

#Create a frequency dataframe
positionfreq <- Jobs %>% group_by(position) %>%
  summarize(Count = n())

#remove NAs for plot
positionfreq <- na.omit(positionfreq)

#plot
ggplot(positionfreq, 
       aes(x = as.character(positionfreq$position), 
           y = positionfreq$Count)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 60, vjust = )) + labs(y = "Frequency", x= "Position")

# 2. Word cloud based on the tibble and all text pre-processing

#create word cloud

wordcloud(positionfreq$position, positionfreq$Count,
          max.words=40,
          scale=c(3,1))

install.packages("ggwordcloud")
library(ggwordcloud)

set.seed(43)
ggplot(positionfreq, aes(label = position)) +
  geom_text_wordcloud() +
  theme_minimal()

ggplot(positionfreq, aes(label = position, size = Count)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 20) +
  theme_minimal()
#===============================================================================
# Export csv
#===============================================================================

Jobs_positions <- subset(Jobs, select = -job)

write.csv(Jobs_positions, "./data/final/Jobs_positions.csv")

write.csv(positionfreq, "./data/final/positionfreq.csv")
