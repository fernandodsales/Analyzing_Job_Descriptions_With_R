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
# Load Data
#===============================================================================

#Read in the previously loaded data
Jobs <- fread("./data/processed/Jobs.csv")

#We created a csv file with a state abbreviation dictionary and latitude longitude
stateabbreviation <- fread("./data/raw/statedictionary.csv")

#===============================================================================
# Pre-Process
#===============================================================================

#Create a vector of symbols we dont want to see
symbols <- c("Â", "â","???", "¢","¦","T","©","´","ã", "*", "â", ".â")

#Loop them in a function to remove
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

#removes trailing whitespace
Jobs$job <- gsub("\\s+"," ",Jobs$job)

#Here we dont set to lowercase beacause we need the capital letters from the State acronyms.

#===============================================================================
# Extract Locations
#===============================================================================

#The following code is inspired by this site:
#https://towardsdatascience.com/extracting-information-from-a-text-in-5-minutes-using-r-regex-520a859590de

stateabbreviation
#creating location columns 
Jobs$state <- NA 
Jobs$city <- NA

#Extracting states 
 
for (i in 1:length(Jobs$job)){    
  #split the text by space  
  split <- strsplit(Jobs$job[i]," ")[[1]]     
  #comparing split with the state abbreviation   
  state <- match(split, stateabbreviation$Abbreviation)    
  #if it matches, get the position of each state  
  state <- which(!is.na(state))    
  #extract the state based on the position  
  state_split <- split[state]    
  #adding states to the new column   
  Jobs$state[i] <- state_split[1]  
}

#Extracting cities
for (i in 1:length(Jobs$job)){   
  #split the text by space  
  split <- strsplit(Jobs$job[i]," ")[[1]]    
  #comparing split with the state abbreviation   
  state <- match(split, stateabbreviation$Abbreviation)    
  #if it matches, get the position of each state  
  state <- which(!is.na(state))-1    
  #extract the state based on the position  
  state_split <- split[state]    
  #adding states to the new column   
  Jobs$city[i] <- state_split[1]  
}

#===============================================================================
# Visualization
#===============================================================================

#Create a frequency distribution plot
state_count<-table(Jobs$state)
state_count<-as.data.frame(state_count)
ggplot(state_count, 
       aes(x = as.character(state_count$Var1), 
           y = state_count$Freq)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 60, vjust = )) + labs(y = "Frequency", x= "States")

#Create a frequency dataframe
locationfreq <- Jobs %>% group_by(state) %>%
  summarize(Count = n())

#Add latitude and longitude columns
locationfreq$lat <- stateabbreviation$latitude[match(locationfreq$state, stateabbreviation$Abbreviation)]
locationfreq$long <- stateabbreviation$longitude[match(locationfreq$state, stateabbreviation$Abbreviation)]

#create map
usa <- map_data("usa") 
ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group))
mapplot <- ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  coord_fixed(1.3)

#plot states with sizes for count
mapplot
mapplot + geom_point(data = locationfreq, aes(x = long, y = lat, size = Count), color = "red")



#===============================================================================
# Export table
#===============================================================================

#Create final table
Jobs$lat <- stateabbreviation$latitude[match(Jobs$state, stateabbreviation$Abbreviation)]
Jobs$long <- stateabbreviation$longitude[match(Jobs$state, stateabbreviation$Abbreviation)]

#Subset
Jobs_Locations <- subset(Jobs, select = c(id, state, city, lat, long))

sum(is.na(Jobs$state))

#Export csv
write.csv(Jobs_Locations, "./data/final/Jobs_locations.csv")

write.csv(locationfreq, "./data/final/locationfreq.csv")
