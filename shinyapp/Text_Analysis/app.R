# Social Media Analytics
#Fernando Delgado, Ahmadomar Nakib, Nour Azar

#setwd("C:/Users/fdelgado/OneDrive - IESEG/Documents/01. IESEG/12. Social Media Analytics/Group Project/shinyapp/Text_Analysis")

#===============================================================================
# Libraries
#===============================================================================
# install.packages("tmap")
# install.packages("raster")

library(shiny)
library(tidyverse)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(maps)
library(tmap) 
library(tmaptools)
library(rgdal)

library(OpenStreetMap)
library(shinydashboard) 
library(hash)
library(wordcloud)
library(DT)
library(rsconnect)

#===============================================================================
# Import CSV
#===============================================================================

locationfreq <- read.csv("./locationfreq.csv")
basetablev1 <- read.csv("./basetable.csv")
positionfreq <- read.csv("./positionfreq.csv") 
companyfreq <- read.csv("./company_freq.csv") 

basetablev1 <- basetablev1 %>% 
    rename(
        company_final = company
    )



#==============================================================================

# forCloud <- c(basetablev1$position, basetablev1$company_final, colnames(basetablev1))

# displaying top positions by filtering base table
position_info <- basetablev1 %>% group_by(position) %>% summarize(count = n()) %>% arrange(desc(count)) %>% drop_na(position)

# displaying top skill by filtering base table ( should be used for graph instead)
skill_info <- basetablev1 %>% group_by(position) %>% summarize(across(java:webservices, sum)) %>% drop_na(position)

company_info <- basetablev1 %>% 
    group_by(company_final) %>%
    summarize(across(java:webservices, sum)) %>% 
    drop_na(company_final)

# find the most common locations of our Top positions
location_finder <- basetablev1 %>% group_by(position,state,city) %>% summarize(count=n())%>% drop_na(state,city)  %>%
    slice(which.max(count))%>% arrange(desc(count))

user_1 <- position_info %>%     # Top N highest values by group
    group_by(position) %>%
    slice(1:3) %>%  arrange(desc(count)) 

##############################################################################

search <- basetablev1

# Sets our Dashboard
ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "Easy HR", titleWidth = 250),
                    dashboardSidebar(
                        
                        # Setting up our different Tabs
                        radioButtons("Data", "Select Your Desired Analytical View",
                                     c("Summary","Our Geographical Distribution",
                                       "Skills & Company Distribution", "Filtered Search"))
                    ),
                    dashboardBody(
                        conditionalPanel(condition = "input.Data == 'Summary'",
                                         
                                         # Top Position/Top Skill for that pos & top location
                                         fluidRow(infoBoxOutput("box1"),
                                                  infoBoxOutput("box2"),
                                                  infoBoxOutput("box11"),
                                         ),
                                         
                                         fluidRow(
                                             # 2nd Position/Top Skill for that pos & top location
                                             infoBoxOutput("box3"),
                                             infoBoxOutput("box4"),
                                             infoBoxOutput("box12"),
                                             
                                         ),
                                         # 3rd Position/Top Skill for that pos & top location
                                         fluidRow(infoBoxOutput("box5"),
                                                  infoBoxOutput("box6"),
                                                  infoBoxOutput("box13"),
                                         ),
                                         # 4th Position/Top Skill for that pos & top location
                                         fluidRow( infoBoxOutput("box7"),
                                                   infoBoxOutput("box8"),
                                                   infoBoxOutput("box14"),
                                         ),
                                         # 5th Position/Top Skill for that pos & top location
                                         fluidRow( infoBoxOutput("box9"),
                                                   infoBoxOutput("box10"),
                                                   infoBoxOutput("box15"),
                                         ),
                                         
                                         fluidRow( selectInput("Selection_cloud", 
                                                               "Select a Topic:", 
                                                               c('position','company'), 
                                                               
                                                               selected = 'position'),
                                                   plotOutput("WC")
                                         )
                                         
                        ),
                        
                        
                        conditionalPanel(condition = "input.Data == 'Our Geographical Distribution'",
                                         
                                         #entering a specific state by user
                                         selectInput("Selection", 
                                                     "Select a State :", 
                                                     c("All", locationfreq$state), 
                                                     multiple = TRUE,
                                                     selected = "All"),
                                         
                                         
                                         textOutput("sum"),
                                         
                                         # Slider selecting the minimum count by user
                                         
                                         sliderInput("inputslider",
                                                     "Select the population desired",
                                                     value = 245,
                                                     min = 1,
                                                     max= 245,),
                                         
                                         # Outputting the map
                                         
                                         plotOutput("map"),
                                         
                                         # outputting our location distribution graph
                                         plotOutput("location_graph") 
                        ),
                        
                        
                        
                        conditionalPanel(condition = "input.Data == 'Skills & Company Distribution'",
                                         
                                         # skill selection by user
                                         
                                         selectInput("skill_selection", 
                                                     "Select a Skill :",
                                                     c(colnames(skill_info)),
                                                     selected = "python"),
                                         
                                         # outputting the skill graph
                                         plotOutput("skill_graph"),
                                         
                                         # Selecting companies by user 
                                         selectInput("company_selection", 
                                                     "Select Your Desired Companies :",
                                                     company_info$company_final,
                                                     multiple = TRUE,
                                                     selected = c("google","amazon","intel"),
                                         ),
                                         
                                         # outputting our skill vs company graph
                                         plotOutput("company_graph"),
                                         
                                         # selecting 1 of 2 different views by user (company or skill for the map below)
                                         
                                         selectInput("dist_select", 
                                                     "Check Distribution by Company or by Skill :",
                                                     c("Company", "Skill"),
                                                     selected = c("Company"),
                                                     
                                         ),
                                         
                                         # output a map that shows the distribution of skills or employees by companies
                                         plotOutput("map2"),),
                        
                        
                        conditionalPanel( condition = "input.Data == 'Filtered Search'",
                                          
                                          radioButtons("search_criteria", "Select your Search Criteria",
                                                       c("Search by ID", "Search by Skill")),
                                          
                                          
                                          conditionalPanel(condition = "input.search_criteria == 'Search by ID'",
                                                           
                                                           selectInput("id_select", 
                                                                       "Select your desired ID :",
                                                                       c(search$id),
                                                                       
                                                                       multiple = TRUE,
                                                                       selected = c(1,2,3,4,5)),  
                                          ),
                                          
                                          
                                          
                                          conditionalPanel(condition = "input.search_criteria == 'Search by Skill'",
                                                           selectInput("skill_select_search", 
                                                                       "Select desired skill to search :",
                                                                       c(colnames(search)))),
                                          
                                          DT::dataTableOutput('table')
                                          
                                          
                        )
                        
                        
                        
                        
                        
                    ))


server <- function(input, output, session){
    
    
    output$sum_others <- renderUI({
        str1 <- paste("Users information:")
        
        HTML(paste("<b>",str1,"</b>"))
    }) ## used HTML Code from https://www.w3schools.com/jsref/jsref_concat_string.asp
    
    
    
    output$box1 <- renderInfoBox({
        
        
        user_1 <- position_info %>%     # Top N highest values by group
            group_by(position) %>%
            slice(1:3) %>%  arrange(desc(count)) 
        
        # displaying the most common Job position
        infoBox("Top Position", user_1[1,1], icon = icon("users"),
                color = "orange"
        )
    })
    
    output$box2 <- renderInfoBox({
        # Top skills for previous Job position
        
        infoBox(
            "Most Common Skill", "java" , icon = icon("credit-card"),
            color = "orange"
        )
    })
    
    # second most common positon
    output$box3 <- renderInfoBox({
        
        infoBox(
            "Second to Top", user_1[2,1] , icon = icon("users"),
            color = "red"
        )
    })
    
    # Top skills for previous Job position
    output$box4 <- renderInfoBox({
        infoBox(
            "Most Common Skill", "python", icon = icon("credit-card"),
            color = "red"
        )
    })
    
    # Third most common position
    output$box5 <- renderInfoBox({
        infoBox(
            "Third to Top", user_1[3,1] , icon = icon("users"),
            color = "navy"
        )
    })
    
    # Top skills for previous Job position
    output$box6 <- renderInfoBox({
        infoBox(
            "Most Common Skill", "python" , icon = icon("credit-card"),
            color = "navy"
        )
    })
    
    # fourth most common position
    output$box7 <- renderInfoBox({
        infoBox(
            "Fourth to Top", user_1[4,1] , icon = icon("users"),
            color = "fuchsia"
        )})
    # Top skills for previous Job position
    output$box8 <- renderInfoBox({
        infoBox(
            "Most Common Skill", "sql"  , icon = icon("credit-card"),
            color = "fuchsia"
        )
        
        
    })
    # Fifth most common position
    
    output$box9 <- renderInfoBox({
        infoBox(
            "Most Common Skill", user_1[5,1] , icon = icon("users"),
            color = "olive"
        ) })
    
    # Top skills for previous Job position
    output$box10 <- renderInfoBox({
        infoBox(
            "Most Common Skill", "java" , icon = icon("credit-card"),
            color = "olive"
        ) })
    
    ##################################
    
    output$box11 <- renderInfoBox({
        
        z <- location_finder[1,2]
        
        # Location for Top position 1
        infoBox(
            "Top Location", z , icon = icon("refresh"),
            color = "orange"
        ) })
    
    output$box12 <- renderInfoBox({
        
        z <- location_finder %>% filter(position == user_1[2,1])
        
        # Location for Top position 2
        infoBox(
            "Top Location", z[1,2] , icon = icon("refresh"),
            color = "red"
        ) })
    
    output$box13 <- renderInfoBox({
        
        z <- location_finder %>% filter(position == user_1[3,1])
        
        # Location for Top position 3
        infoBox(
            "Top Location", z[1,2] , icon = icon("refresh"),
            color = "navy"
        ) })
    
    output$box14 <- renderInfoBox({
        
        z <- location_finder %>% filter(position == user_1[4,1])
        
        # Location for Top position 4
        infoBox(
            "Top Location", z[1,2] , icon = icon("refresh"),
            color = "fuchsia"
        ) })
    
    output$box15 <- renderInfoBox({
        
        # Location for Top position 5
        z <- location_finder %>% filter(position == user_1[5,1])
        
        infoBox(
            "Top Location", z[1,2] , icon = icon("refresh"),
            color = "olive"
        ) })
    
    #++++++++++++++++++++++++++++++++++++++
    
    
    
    output$WC <- renderPlot({
        
        if(input$Selection_cloud == 'position'){
            wordcloud(positionfreq$position, positionfreq$Count,
                      max.words=200,
                      scale=c(15,3),  colors=brewer.pal(8, "Dark2"))
        }else{
            
            wordcloud(companyfreq$company_final, companyfreq$Count,
                      max.words=200,
                      scale=c(15,3),  colors=brewer.pal(8, "Dark2"))
            
            
            
        }
        
        
        
    })
    
    #############################################################################
    output$map <- renderPlot({
        
        # Error Handling in case of no input by user
        validate(
            need(input$Selection != "", "Please Make a selection!")
        )
        
        # Handles the 'All' Case where all are selected and when specific variables are selected
        if(input$Selection == "All"){    interactive_data <- locationfreq %>%
            filter( Count < input$inputslider)}else{    interactive_data <- locationfreq %>%
                filter( Count < input$inputslider, state %in% input$Selection) }
        
        
        
        # Displays the location fequency on the map based on the previously selected filters by user 
        usa <- map_data("usa") 
        ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) + coord_fixed(1.3) + 
            geom_point(data = interactive_data, aes(x = long, y = lat, size = Count), color = "red") +   
            geom_point()
        
        
    })
    
    
    output$location_graph <- renderPlot({
        
        # Error Handling in case of no input by user
        validate(
            need(input$Selection != "", "Please Make a selection!")
        )
        
        
        # setting up required information
        state_count<-table(basetablev1$state)
        state_count<-as.data.frame(state_count)
        
        # Handles the 'All' Case where all are selected and when specific variables are selected
        if(input$Selection == "All"){    interactive_data_2 <- state_count %>%
            filter( Freq < input$inputslider)}else{interactive_data_2 <- state_count %>%
                filter( Freq < input$inputslider, Var1 %in% input$Selection) }
        
        # displays var input 1 with respect to input 2 by user
        ggplot(interactive_data_2, 
               aes(x = as.character(interactive_data_2$Var1), 
                   y = interactive_data_2$Freq)) + geom_bar(stat="identity", fill = "#E69F00") + theme(axis.text.x = element_text(angle = 60, vjust = )) + 
            labs(y = "State Count", x= "state", fill= "#E69F00")
        
        
        
    })
    
    output$skill_graph <- renderPlot({
        
        vec1 <- as.vector(pull(skill_info, input$skill_selection))
        
        class(vec1)
        
        ggplot(skill_info, 
               aes_(x = as.character(skill_info$position), y = vec1)) + geom_bar(stat="identity", fill = "#56B4E9") + theme(axis.text.x = element_text(angle = 60, vjust = )) 
        
        
    })
    
    output$company_graph <- renderPlot({
        
        
        interactive_data3 <- company_info %>%
            filter(company_final %in% input$company_selection)
        
        vec2 <- as.vector(pull(interactive_data3, input$skill_selection))
        
        
        
        
        # interactive_data3[, colSums(interactive_data3) != 0]
        
        
        
        ggplot(interactive_data3,#interactive_data_3, 
               aes(y = company_final, #as.character(interactive_data_3$company_final), 
                   x = vec2)) + geom_bar(stat="identity",color = "#FF6666", fill = "#FF6665") + theme(axis.text.x = element_text(angle = 60, vjust = ))
        
    })
    
    
    output$map2 <- renderPlot({
        
        
        
        
        if(input$dist_select == "Company"){
            company_info2 <- basetablev1 %>% 
                group_by(company_final,lat,long) %>%
                summarize(across(java:webservices, sum,Count = n())) %>% 
                drop_na(company_final,lat,long)
            
            interactive_data4 <- company_info2 %>%
                filter(company_final %in% input$company_selection)
            
            
            usa <- map_data("usa") 
            ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) + coord_fixed(1.3) + 
                geom_point(data = interactive_data4, aes(x = long, y = lat), color = "red") +   
                geom_point() }else{
                    
                    
                    company_info2 <- basetablev1 %>% 
                        group_by(company_final,lat,long) %>%
                        summarize(across(java:webservices, sum)) %>% 
                        drop_na(company_final,lat,long)
                    
                    
                    interactive_data4 <- company_info2
                    interactive_data4 <- company_info2 %>%
                        dplyr::select(long,lat,all_of(input$skill_selection))
                    
                    interactive_data4[interactive_data4==0] <- NA
                    
                    interactive_data4 <- interactive_data4 %>% drop_na(all_of(input$skill_selection))
                    
                    
                    usa <- map_data("usa") 
                    ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) + coord_fixed(1.3) + 
                        geom_point(data = interactive_data4, aes(x = long, y = lat, size = input$skill_selection), color = "red") +   
                        geom_point()
                    
                    
                }
        
    })
    
    output$table = DT::renderDataTable({
        
        
        
        if(input$search_criteria == 'Search by ID'){ 
            
            search <- basetablev1 %>%
                filter(id %in% input$id_select)
            
            
            
            search[sapply(search, function(x) any(x > 0))]
            
            
        }else{ 
            
            search <- basetablev1 %>%
                dplyr::select(id,position,state,city,long,lat,company_final,input$skill_selection)
            
            
            
        }
        
        
        
        
        
        
        
        
        
    })
    
}

shinyApp(ui = ui, server = server)
