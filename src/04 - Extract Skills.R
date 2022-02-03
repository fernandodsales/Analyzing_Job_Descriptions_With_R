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

#We manually cleaned a dictionary with the noun frequency file we generated on the previous code
skills_dict <- fread("./data/processed/skillsfreq.csv")
skills_dict <- subset(skills_dict, select = -V1)
head(skills_dict)

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
# Vizualization top Skills
#===============================================================================

#Select top 20 skills
skills_dict <- skills_dict %>%
  arrange(desc(Count)) 

top_skills <- head(skills_dict,20)
top_skills

#plot
ggplot(top_skills, 
       aes(x = as.character(top_skills$token), 
           y = top_skills$Count)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 60, vjust = )) + labs(y = "Frequency", x= "Skill")

#===============================================================================
# Extract skills by id
#===============================================================================

#Create Dataframe with one column per skill

#empty dataframe
skills <- data.frame()
#create dataframe with number of skills + id. Rows are equal to the total CVs
skills <- data.frame(matrix(ncol = nrow(skills_dict)+1, nrow = nrow(Jobs)))

#Fill the name of the columns with skills
x <- c("id", skills_dict$token)
colnames(skills) <- x

#Create id Column 
idrange <- 1:nrow(Jobs)
skills$id <- idrange
#-------------------------------------------------------------------------------

#For each skill find the skill name in each cv 1 if true, 0 if false

#iterate through all skill names
for (i in colnames(skills)){
  #skip id column
  if (i == 'id') {
    next
  }
  else {
    #iterate through all jobs
    for (j in 1:nrow(Jobs)){
    #split strings
    split <- strsplit(Jobs$job[j]," ")[[1]]
    #find match skill with string
    skill <- match(split, i) 
    #get position of match
    skill <- which(!is.na(skill)) 
    #if match is true, 1 - else 0
    ifelse(!is.na(skill), skills[j,i] <- 1, skills[j,i] <- 0)
    }
  }
}

#Replace NAs with 0
skills[is.na(skills)] <- 0

#Finally we created a basetable of skills with boolean values for each document
write.csv(skills, "./data/final/skills_by_id.csv")
