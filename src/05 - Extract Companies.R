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

#Read in the previously loaded data
Jobs <- fread("./data/processed/Jobs.csv")
Jobs <- subset(Jobs, select = -V1)

#read in position dictionary
position_dict <- fread("./data/processed/position_dict.csv")

#===============================================================================
# Pre-processing
#===============================================================================

#Cleaning data

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
# Extract Companies
#===============================================================================

#Given that most cvs have the company directly after the position they work, 
#we extract the next 2 words after the position

#Extract the first 3 words after the position (loop 3 times)
for (var in 1:3) {
  
  #iterate through all database
  for (i in 1:length(Jobs$job)){    
    
    #split the text by space  
    split <- strsplit(Jobs$job[i]," ")[[1]]     
    
    #comparing split with the position token  
    company <- match(split, position_dict$token) 
    
    #if it matches, get the first word after position  
    company <- which(!is.na(company))+ var 
    
    #extract the position of the word
    company_split <- split[company] 
    
    #adding states to the new column   
    Jobs$company_1[i] <- company_split[1]  
  }
}

#Concatenate Company name 
#we concatenate based on the middle word. 
#If its an "Of" we concat the next, since it may be "Bank of Amercia". If its a "-", we only take the first string. 

#Iterate through all rows
for(i in 1:nrow(Jobs)){
  
  #If its an "of"
  ifelse(Jobs$company_2[i] == "of", 
        
         #Concat 3 words
         Jobs$company[i] <- str_c(Jobs$company_1[i], " ", Jobs$company_2[i]," ", Jobs$company_3[i]), 
         
         #If its a "-"
         ifelse(Jobs$company_2[i] =="-", 
                
                #Just take the first string
                Jobs$company[i] <- Jobs$company_1[i],
                
                #Else, concat 2 strings
                Jobs$company[i] <- str_c(Jobs$company_1[i]," ",Jobs$company_2[i])))
}

#Subset Company dictionary
Company_dict <- subset(Jobs, select = c(id, company))

#Create a frequency table 
companyfreq <- Company_dict %>% group_by(company) %>%
  summarize(Count = n())

#===============================================================================
# Exporting data and creating dictonary
#===============================================================================

#We export our dictionary to csv and manually remove some words we dont want to see
write.csv(companyfreq, "./data/processed/company_freq.csv")
new_company_dict <- read.csv("./data/processed/company_dict2.csv")

#Tokenize into sentences
test_sentences <- Jobs %>%
  unnest_tokens(sentences, job, token = 'sentences')

#create empty column 
Jobs$company_final <- NA

#WARNING: takes 3 min to load
#for each possible company
for (i in 1:nrow(new_company_dict)){
  print(i)
  #save each company to temporary variable
  tmp_company <- new_company_dict[i,1]

  #for each possible CV
  for (j in 1:nrow(Jobs)){
    
    #Find if its inside the string or not
    tmp <- grepl(tmp_company, test_sentences$sentences[j], fixed = TRUE)
    #if there is a match, add company name 
    if (tmp == "TRUE"){
      Jobs$company_final[j] <- tmp_company
    }
    else {
      next
    }
  }
}

#subset to a table
company <- subset(Jobs, select = c(id, company_final))

#Create a frequency table 
companyfreq <- company %>% group_by(company_final) %>%
  summarize(Count = n())

#===============================================================================
# Visualization
#===============================================================================

#remove NAs for plot
plot_companyfreq <- na.omit(companyfreq)

#Take top 20 companies
plot_companyfreq <- plot_companyfreq %>%
  arrange(desc(Count)) 

top_companies <- head(plot_companyfreq,20)
top_companies

#plot
ggplot(top_companies, 
       aes(x = as.character(top_companies$company_final), 
           y = top_companies$Count)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 60, vjust = )) + labs(y = "Frequency", x= "Company")

#===============================================================================
# Export csv for basetable
#===============================================================================

write.csv(companyfreq, "./data/final/company_freq.csv")
write.csv(company, "./data/final/Jobs_company.csv")
