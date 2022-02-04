#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Load libraries, data, and data mining.
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(openxlsx)
library(shiny)
library(bslib)
library(ggtext)
thematic::thematic_shiny(font = "auto")
library(ggplot2)
library(knitr)
library(dplyr)

#The function to generate a vector of "week" values based on an inserted number of days.
generate_weeks <- function(x = 300)
{
  output <- c()
  count <- 1
  week <- 1
  
  for(i in 1:x)
  {
    if(count > 7)
    {
      count <- 1
      week <- week + 1
    }
    
    output <- append(output, week)
    count <- count + 1
  }
  
  return(output)
}

#The function to add a team total to the inserted data frame.
add_team_total <- function(data)
{
  data <- data %>% mutate("TEAM" = round(rowSums(as.data.frame(sapply(data[,c(2:ncol(data))],as.numeric))), digits = 1))
  
  return(data)
}

#The function to add a team total to the inserted data frame.
add_team_avg <- function(data)
{
  data <- data %>% mutate("TEAM" = round(rowSums(as.data.frame(sapply(data[,c(2:ncol(data))],as.numeric)))/(ncol(data) - 1), digits = 1))
  
  return(data)
}

#The funtion to convert RPE data to Training Load data.
get_tl <- function(cleaned_rpe_data)
{
  for(i in 1:length(cleaned_rpe_data$`SESSION DURATION`))
  {
    cleaned_rpe_data[i,c(4:34)] <- as.numeric(cleaned_rpe_data[i,c(3)]) * as.numeric(cleaned_rpe_data[i,c(4:34)])
  }
  
  return(cleaned_rpe_data)
}

clean_data <- function(rpe_data)
{
  #Removing the first two columns, which don't tell us any relevant information. 
  cleaned_rpe_data <- rpe_data[,-c(1:2, 5)]
  
  #Removing the unwanted noise from the data set, such as "?", "INJ", or "inj
  cleaned_rpe_data[(cleaned_rpe_data == "?") | 
                     (cleaned_rpe_data == "INJ") | 
                     (cleaned_rpe_data == "inj") |
                     (cleaned_rpe_data == "OC match") |
                     (cleaned_rpe_data == "N") |
                     (cleaned_rpe_data == "T") |
                     (cleaned_rpe_data == "OC")] <- NA
  
  #Removing the ":" character from the data frame. This just makes things cleaner.
  cleaned_rpe_data <- data.frame(lapply(cleaned_rpe_data, function(x) { gsub(":", "", x)}))
  
  #Removing the top two rows of the data frame, which are then used to properly rename the columns.
  cleaned_rpe_data <- cleaned_rpe_data[-c(1:2),]
  names(cleaned_rpe_data) <- c("SESSION TYPE",
                               "SESSION DURATION",
                               "PLAYER 1",
                               "PLAYER 2",
                               "PLAYER 3",
                               "PLAYER 4",
                               "PLAYER 5",
                               "PLAYER 6",
                               "PLAYER 7",
                               "PLAYER 8",
                               "PLAYER 9",
                               "PLAYER 10",
                               "PLAYER 11",
                               "PLAYER 12",
                               "PLAYER 13",
                               "PLAYER 14",
                               "PLAYER 15",
                               "PLAYER 16",
                               "PLAYER 17",
                               "PLAYER 18",
                               "PLAYER 19",
                               "PLAYER 20",
                               "PLAYER 21",
                               "PLAYER 22",
                               "PLAYER 23",
                               "PLAYER 24",
                               "PLAYER 25",
                               "PLAYER 26",
                               "PLAYER 27",
                               "PLAYER 28",
                               "PLAYER 29",
                               "PLAYER 30",
                               "PLAYER 31")
  
  #Adding the appropriate WEEK variable
  cleaned_rpe_data <- cbind(WEEK = generate_weeks(length(cleaned_rpe_data$`SESSION TYPE`)), cleaned_rpe_data)
  
  #Removing all off days from the data set
  cleaned_rpe_data <- subset(cleaned_rpe_data, `SESSION TYPE` != "O")
  
  #Replacing NA values
  cleaned_rpe_data[is.na(cleaned_rpe_data)] <- 0
  
  #Getting the TRAINING LOAD version of the data set. RPE Score * Session Duration.
  cleaned_tl_data <- get_tl(cleaned_rpe_data)
  
  #Transposing both data sets again to get it in the proper format to easily graph.
  cleaned_rpe_data <- data.frame(t(cleaned_rpe_data))
  cleaned_tl_data <- data.frame(t(cleaned_tl_data))
  
  return(list(cleaned_rpe_data, cleaned_tl_data))
}


get_weekly_sum <- function(data)
{
  #Replacing NA values
  data[is.na(data)] <- 0
  
  output <- NA
  
  #Looping through each week
  for(i in 1:max(as.numeric(data[c(1),])))
  {
    #Getting only the columns that represent the week in focus
    tempData <- data[,which(as.numeric(data[c(1),]) == i)]
    
    #Getting the average for each row 
    weeklySums <- round(rowSums(as.data.frame(sapply(tempData[c(4:34),],as.numeric))), digits = 1)
    
    #Adding the calculated average for each week to the output
    if(i == 1)
    {
      output <- data.frame(weeklySums)
    }
    else
    {
      output <- cbind(output, weeklySums)
    }
  }
  
  #Replacing NA values
  output[is.na(output)] <- 0
  
  #Finally, let's rename the  index of the data frame we just created and add a column to define the week.
  rownames(output) <- c("PLAYER_1",
                        "PLAYER_2",
                        "PLAYER_3",
                        "PLAYER_4",
                        "PLAYER_5",
                        "PLAYER_6",
                        "PLAYER_7",
                        "PLAYER_8",
                        "PLAYER_9",
                        "PLAYER_10",
                        "PLAYER_11",
                        "PLAYER_12",
                        "PLAYER_13",
                        "PLAYER_14",
                        "PLAYER_15",
                        "PLAYER_16",
                        "PLAYER_17",
                        "PLAYER_18",
                        "PLAYER_19",
                        "PLAYER_20",
                        "PLAYER_21",
                        "PLAYER_22",
                        "PLAYER_23",
                        "PLAYER_24",
                        "PLAYER_25",
                        "PLAYER_26",
                        "PLAYER_27",
                        "PLAYER_28",
                        "PLAYER_29",
                        "PLAYER_30",
                        "PLAYER_31")
  
  output <- data.frame(t(output))
  
  output <- cbind(Week = c("1",
                           "2",
                           "3",
                           "4",
                           "5",
                           "6",
                           "7",
                           "8",
                           "9",
                           "10",
                           "11",
                           "12",
                           "13",
                           "14",
                           "15",
                           "16",
                           "17",
                           "18",
                           "19",
                           "20",
                           "21",
                           "22",
                           "23",
                           "24",
                           "25",
                           "26",
                           "27",
                           "28",
                           "29",
                           "30",
                           "31",
                           "32",
                           "33",
                           "34",
                           "35",
                           "36",
                           "37",
                           "38",
                           "39",
                           "40",
                           "41",
                           "42",
                           "43"), output)
  
  output$Week <- factor(output$Week , levels=c("1",
                                               "2",
                                               "3",
                                               "4",
                                               "5",
                                               "6",
                                               "7",
                                               "8",
                                               "9",
                                               "10",
                                               "11",
                                               "12",
                                               "13",
                                               "14",
                                               "15",
                                               "16",
                                               "17",
                                               "18",
                                               "19",
                                               "20",
                                               "21",
                                               "22",
                                               "23",
                                               "24",
                                               "25",
                                               "26",
                                               "27",
                                               "28",
                                               "29",
                                               "30",
                                               "31",
                                               "32",
                                               "33",
                                               "34",
                                               "35",
                                               "36",
                                               "37",
                                               "38",
                                               "39",
                                               "40",
                                               "41",
                                               "42",
                                               "43"))
  
  row.names(output) <- NULL
  
  return(output)
}

#The function to get the players'/team's weekly average for a number of metrics.
get_weekly_average <- function(data)
{
  #Replacing NA values
  data[is.na(data)] <- 0
  
  output <- NA
  
  #Looping through each week
  for(i in 1:max(as.numeric(data[c(1),])))
  {
    #Getting only the columns that represent the week in focus
    tempData <- data[,which(as.numeric(data[c(1),]) == i)]
    
    #Getting the average for each row 
    weeklyAvgs <- round(rowMeans(as.data.frame(sapply(tempData[c(4:34),],as.numeric))), digits = 1)
    
    #Adding the calculated average for each week to the output
    if(i == 1)
    {
      output <- data.frame(weeklyAvgs)
    }
    else
    {
      output <- cbind(output, weeklyAvgs)
    }
  }
  
  #Replacing NA values
  output[is.na(output)] <- 0
  
  #Finally, let's rename the  index of the data frame we just created and add a column to define the week.
  rownames(output) <- c("PLAYER_1",
                        "PLAYER_2",
                        "PLAYER_3",
                        "PLAYER_4",
                        "PLAYER_5",
                        "PLAYER_6",
                        "PLAYER_7",
                        "PLAYER_8",
                        "PLAYER_9",
                        "PLAYER_10",
                        "PLAYER_11",
                        "PLAYER_12",
                        "PLAYER_13",
                        "PLAYER_14",
                        "PLAYER_15",
                        "PLAYER_16",
                        "PLAYER_17",
                        "PLAYER_18",
                        "PLAYER_19",
                        "PLAYER_20",
                        "PLAYER_21",
                        "PLAYER_22",
                        "PLAYER_23",
                        "PLAYER_24",
                        "PLAYER_25",
                        "PLAYER_26",
                        "PLAYER_27",
                        "PLAYER_28",
                        "PLAYER_29",
                        "PLAYER_30",
                        "PLAYER_31")
  
  output <- data.frame(t(output))
  
  output <- cbind(Week = c("1",
                           "2",
                           "3",
                           "4",
                           "5",
                           "6",
                           "7",
                           "8",
                           "9",
                           "10",
                           "11",
                           "12",
                           "13",
                           "14",
                           "15",
                           "16",
                           "17",
                           "18",
                           "19",
                           "20",
                           "21",
                           "22",
                           "23",
                           "24",
                           "25",
                           "26",
                           "27",
                           "28",
                           "29",
                           "30",
                           "31",
                           "32",
                           "33",
                           "34",
                           "35",
                           "36",
                           "37",
                           "38",
                           "39",
                           "40",
                           "41",
                           "42",
                           "43"), output)
  
  output$Week <- factor(output$Week , levels=c("1",
                                               "2",
                                               "3",
                                               "4",
                                               "5",
                                               "6",
                                               "7",
                                               "8",
                                               "9",
                                               "10",
                                               "11",
                                               "12",
                                               "13",
                                               "14",
                                               "15",
                                               "16",
                                               "17",
                                               "18",
                                               "19",
                                               "20",
                                               "21",
                                               "22",
                                               "23",
                                               "24",
                                               "25",
                                               "26",
                                               "27",
                                               "28",
                                               "29",
                                               "30",
                                               "31",
                                               "32",
                                               "33",
                                               "34",
                                               "35",
                                               "36",
                                               "37",
                                               "38",
                                               "39",
                                               "40",
                                               "41",
                                               "42",
                                               "43"))
  
  row.names(output) <- NULL
  
  return(output)
}

#The function to get the percentage change in each player/the team's training load.
get_percentage_change <- function(data)
{
  #copying the inputted data
  output <- data
  
  #Looping through each column
  for(i in 2:ncol(output))
  {
    #Looping through each row
    for(j in 2:nrow(output))
    {
      output[c(j),c(i)] <- round((data[c(j),c(i)]/data[c(j - 1),c(i)] - 1) * 100, digits = 1)
    }
  }
  
  #Setting the first row to zero in place of a NULL values
  output[c(1),c(2:ncol(output))] <- 0
  
  #Replacing null values with 0
  output[is.na(output)] <- 0
  
  #Replacing Inf values with 100
  output[output == Inf] <- 100
  
  return(output)
}


#Reading in the provided data.
rpe_data <- read.xlsx("./Data/LAFC data assignment .xlsx")

#Transposing the data to make each training session its own row.
rpe_data <- data.frame(t(rpe_data))

#Cleaning the provided data to put it into a usable format.
cleaned_data <- clean_data(rpe_data)

#Getting the weekly averages, sums, and percentage change statistics for each of the necessary data sets.
avg_rpe_data <- add_team_avg(get_weekly_average(cleaned_data[[1]]))

sum_tl_data <- add_team_total(get_weekly_sum(cleaned_data[[2]]))

avg_tl_data <- add_team_avg(get_weekly_average(cleaned_data[[2]]))

pctcng_tl_data <- get_percentage_change(sum_tl_data)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#User Interface
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Page 1 - Project Write Up ------------------------------------------
intro_panel <- tabPanel(
  "Prjoect Write Up",
  
  titlePanel("Player Exertion Project Write Up"),
  
  br(), #Space
  
  strong("-------------------------- PROJECT DESCRIPTION --------------------------"),
  
  br(), #Space
  
  p("The first challenge of this project was to extract a usable data set from the Excel file that was provided. 
      Though it was not simple, I decided to write functions that can easily handle similarly structured data sets
    in the future. This makes my project easily reproducible and more valuable than a simple one-off approach to an
    assignment."),
  
  br(), #Space
  
  p("Next, I chose to present the data as a weekly average of each player's exertion to avoid presenting the coaching
    staff with a complex graphic of over 300 data points in each graph. Additionally, I chose to use a simple bar graph because it is
    a graphic anyone on LAFC's staff can digest. Though these decisions resulted in the loss of the context of what type of 
    session (Recovery, Match, etc.) each score was tied to, I believe it was the most effective method of delivering 
    actionable information."),
  
  br(), #Space
  
  p("To add valuable context to the graphs that present averages, I ploted three lines to designate which scores should be considered
    low, average, and high exertion weeks. High exertion weeks were at least one standard deviation more stressful
    than the average, while low exertion weeks were one standard deviation less. The inclusion of such context enables this
    app to be a tool to start data-driven conversations with the LAFC's sport scientists on outliers in specific player's 
    training load. For example, Player 21's Week 20 and Player 30's Week 31 stand out."),
  
  br(), #Space
  
  p("Finally, to demonstrate my ability to build dynamic, adaptable apps I also included the user to be able to switch between
     their perfered metric and pick which player they'd like to analyze."),
  
  br(), #Space
  
  p(a(href = "https://github.com/gkrhines/lafc_app", "Use this link to take an in-dpeth look at the code behind this app.")),
  
  br(), #Space
  
  strong("--------------------------- METRIC GLOSSARY ---------------------------"),
  p("Rating of Percieved Exertion (RPE) = The percieved stress that each player experienced after a match or training session. An answer of '1' implies a very light, easy session, while a '10' signals maximum exertion."),
  
  br(), #Space
  
  p("Training Load = A player's RPE score multiplied by the duration of the session. This metric provides a more accurate approximation of the true stress a player experienced.")
)

# Page 2 - RPE Visualization ----------------------------------------
select_values <- colnames(avg_tl_data)
select_values <- select_values[! select_values %in% c('Week')] # remove unwanted columns

sidebar_content <- sidebarPanel(
                      selectInput(
                        "y_var",
                        label = "Select which player's exertion you would like to analyze.",
                        choices = select_values,
                        selected = 'TEAM'
                      ),
                      radioButtons("metric","Choose which metric to use:",
                                   choices = c("Avg. Rating of Percieved Exertion",
                                               "Avg. Training Load",
                                               "Total Training Load", 
                                               "Percentage Change Training Load"),
                                   selected = "Avg. Rating of Percieved Exertion")
                      
)

main_content <- mainPanel(
  plotOutput("plot")
)

second_panel <- tabPanel(
  "Visualization",
  titlePanel("What has each player's physical exertion been over the weeks?"),
  sidebarLayout(
    sidebar_content, main_content
  )
)


# User Interface -----------------------------------------------------
ui <- navbarPage(
  theme = bs_theme(version = 4, bootswatch = "solar"),
  "LAFC Player Exertion Dashboard",
  intro_panel,
  second_panel
)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Server
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Create server -------------------------------------------------------
server <- function(input, output) {
  bs_themer()
  
  observe({
      if(input$metric == "Avg. Rating of Percieved Exertion")
      {
          output$plot <- renderPlot({
            
            ggplot(data=avg_rpe_data, aes_string(x=input$y_var, y="Week")) +
              geom_bar(stat="identity", width=0.8, fill="snow1") +
              scale_x_continuous(name = "Player's Weekly Average RPE",limits = c(0, 10)) +
              labs(x=input$y_var, y="Week") + coord_flip() +
              geom_vline(xintercept = 3.28, linetype='dashed', color=c('black')) +
              geom_richtext(aes(x= 3.3, y=38, label= "**Avg. Player Exertion**"),
                          size=4, col = 'white', fill = 'black')+
              geom_vline(xintercept = 5.63, linetype='dashed', color=c('red')) +
              geom_richtext(aes(x= 5.65, y=38, label= "**High Player Exertion**"),
                            size=4, col = 'red', fill = 'black') +
              geom_vline(xintercept = 1.28, linetype='dashed', color=c('green2')) +
              geom_richtext(aes(x= 1.3, y=38, label= "**Low Player Exertion**"),
                            size=4, col = 'green2', fill = 'black') +
              labs(title = "Weekly Average Rating of Percieved Exertion")
        
            })
      }
      else if (input$metric == "Avg. Training Load")
      {
        output$plot <- renderPlot({
          
          ggplot(data=avg_tl_data, aes_string(x=input$y_var, y="Week")) +
            geom_bar(stat="identity", width=0.8, fill="white") +
            scale_x_continuous(name = "Player's Weekly Average TL",limits = c(0, 705.396341)) +
            labs(x=input$y_var, y="Week") + coord_flip() +
            geom_vline(xintercept = 231.37, linetype='dashed', color=c('black')) +
            geom_richtext(aes(x= 231.39, y=38, label= "**Avg. Player Exertion**"),
                          size=4, col = 'white', fill = 'black') +
            geom_vline(xintercept = 408.79, linetype='dashed', color=c('red')) +
            geom_richtext(aes(x= 408.81, y=38, label= "**High Player Exertion**"),
                          size=4, col = 'red', fill = 'black') +
            geom_vline(xintercept = 120, linetype='dashed', color=c('green2')) +
            geom_richtext(aes(x= 120.02, y=38, label= "**Low Player Exertion**"),
                          size=4, col = 'green2', fill = 'black') +
            labs(title = "Weekly Average Training Load")
          
        })
      }
      else if (input$metric == "Total Training Load")
      {
        output$plot <- renderPlot({
          
          ggplot(data=sum_tl_data, aes_string(x=input$y_var, y="Week")) +
            geom_bar(stat="identity", width=0.8, fill="white") +
            scale_x_continuous(name = "Player's Weekly Total TL") +
            labs(x=input$y_var, y="Week") + coord_flip() +
            labs(title = "Weekly Total Training Load")
          
        })
      }
      else if (input$metric == "Percentage Change Training Load")
      {
        output$plot <- renderPlot({
          
          ggplot(data=pctcng_tl_data, aes_string(x=input$y_var, y="Week")) +
            geom_bar(stat="identity", width=0.8, fill="white") +
            scale_x_continuous(name = "Player's Weekly Percentage Change in TL") +
            labs(x=input$y_var, y="Week") + coord_flip() +
            labs(title = "Weekly Percentage Change in Training Load")
          
        })
      }
  })
}


shinyApp(ui, server)
