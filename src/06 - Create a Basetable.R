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

#Job Positions 
Job_positions <- fread("./data/final/Jobs_positions.csv")
Job_positions <- subset(Job_positions, select = -V1)

#Job Locations 
Job_locations <- fread("./data/final/Jobs_locations.csv")
Job_locations <- subset(Job_locations, select = -V1)

#Skills
skills <- fread("./data/final/skills_by_id.csv")
skills <- subset(skills, select = -V1)

#Companies
company <- fread("./data/final/Jobs_company.csv")
company <- subset(company, select = -V1)

#===============================================================================
# Merge
#===============================================================================

basetable <- merge(Job_positions, Job_locations, by="id")
basetable <- merge(basetable, company, by="id")
basetable <- merge(basetable, skills, by="id")

names(basetable)[names(basetable) == "company_final"] <- "company"

#===============================================================================
# Export csv
#===============================================================================

write.csv(basetable, "./data/final/basetable.csv")

#subset table
skills_byposition <- subset(basetable, select = -c(id,state, city, lat, long, company))

#group by skill
skills_byposition <- skills_byposition %>% group_by(position) %>%
  summarise_each(list(sum))

#select max
colnames(skills_byposition)[max.col(skills_byposition, ties.method = "first")] 

skills_byposition[,2:ncol(skills_byposition)]
