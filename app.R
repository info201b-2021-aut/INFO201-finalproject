#install.packages("shinyWidgets")
library(shiny)
library(tidyverse)
library(plotly)
library(shinyWidgets)

unemployment <- read.csv("https://raw.githubusercontent.com/info201b-2021-aut/final-project-ydodobara/main/unemployment_data_us.csv?token=AV5HNBIXZBOMVUYCBJUB4D3BW2RJ4")

# This creates a column called "Month_num" that changes month abbreviations into numbers (Jan = 1, etc.) and creates a column called "Gender" with values from both "Men" and "Women" columns
unemployment_date_gender_table <- unemployment %>%
  mutate(Month_num = match(unemployment$Month, month.abb)) %>%
  pivot_longer(cols = c("Men", "Women"), names_to = "Gender", values_to = "Rate by Gender") 

# This groups together all education levels into one column called "Education_Level" and shows the rates of each education level in a column called "Rate_by_Education_Level"
unemployment_education_table <- unemployment %>%
  pivot_longer(cols = c("Primary_School", "High_School", "Associates_Degree", "Professional_Degree"), names_to = "Education_Level", values_to = "Rate_by_Education_Level")

# Intro page

# Interactive page 1
unemployment_education <- tabPanel(
  "Unemployment by Education Level",
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "radio",
        label = h3("Select an education level:"),
        choices = list("Primary School" = "Primary_School", "High School" = "High_School", "Associates Degree" = "Associates_Degree", "Professional Degree" = "Professional_Degree")
      )
    ),
    mainPanel(
      plotlyOutput(outputId = "unemployment_education_chart"),
      p("Throughout the years displayed, the average unemployment rate has constantly decreased in all education levels showing
      that the U.S. has been more lenient of who to hire and not judge based off of education level. But in comparison to one another, 
      the average unemployment rates were always greater the lower the education level someone has. This demonstrates that even though 
      unemployment rates have been lower recently, there still is a bias that the less educated you are, the more likely you are to be unemployed.")
    )
  )
)

# Interactive page 2
InteractivePageTwo <- 
  tabPanel("Unemployment Data by Race", 
           fluidPage(
             h1("Visualization of the unemplotment rate by race.")
           ),
           fluidRow(
             column(8,
                    #Select Race
                    selectInput(inputId = "race", "Race",
                                c("White", "Black", "Asian", "Hispanic")
                    )
             )
           ),
           plotOutput(outputId = "graph"),
           textOutput(outputId = "textguy")
  )


# Interactive page 3

unemployment_date_gender <- tabPanel(
  "Unemployment by Date and Gender",
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "gender",
        label = h3("Select one or more genders"),
        choices = list("Men" = "Men", "Women" = "Women")
      ),
      radioGroupButtons(
        "years",
        label = h3("Select a range of years"),
        choices = unique(unemployment_date_gender_table$Year)
      )
    ),
    mainPanel(
      plotlyOutput(outputId = "date_gender_chart"),
      p("Economic change can change unemployment rates, and analyzing the unemployment rates in a certain month and/or year 
      can describe the economic situation the U.S. was in during that time. In addition, unemployment rates by gender are important
        to analyze due to the fact that women are known to be treated less fairly than men.")
    )
  )
)

# Conclusion page


# UI code

ui <- navbarPage(
  "Unemployment Data Analysis",
  unemployment_education,
  InteractivePageTwo,
  unemployment_date_gender
)

# Server code

server <- function(input, output){
  #Interactive page 1
  output$unemployment_education_chart <- renderPlotly({
    unemployment_education_table %>%
      filter(Education_Level %in% input$radio) %>%
      group_by(Year) %>%
      mutate(rate = mean(Rate_by_Education_Level, na.rm = TRUE)) %>%
      plot_ly(x = ~Year, y = ~rate, type = "scatter", mode = "markers",
              text = ~paste("Year:", Year, "Rate:", rate),
              color = ~Education_Level,
              colors = "medium purple") %>%
      layout(title = "Average Unemployment Rates by Education Level Per Year",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Unemployment Rate"))
  })
  #InteractivePageTwo 
  race_data <- unemployment %>%
    select(Date, White, Black, Asian, Hispanic)
  
  #display graph of unemployment rate by race  
  output$graph <- renderPlot({
    #select filter 
    if(input$race == "White"){
      select_race_data <- data.frame(xaxis =race_data$Date, yaxis =race_data$White)
    } 
    if(input$race == "Black"){
      select_race_data <- data.frame(xaxis =race_data$Date, yaxis=race_data$Black)
    } 
    if(input$race == "Asian"){
      select_race_data <- data.frame(xaxis=race_data$Date, yaxis=race_data$Asian)
    }  
    if(input$race == "Hispanic"){
      select_race_data <- data.frame(xaxis=race_data$Date, yaxis=race_data$Hispanic)
    }  
    #plot
    ggplot(select_race_data, aes(x=xaxis, y=yaxis, na.rm=TRUE), xaxt = "n") + 
      geom_point(color="red", size = 3) +
      labs(title = "Unemployment Fluctuation by each Race", subtitle = "From January 2010 to December 2020") +
      xlab("Month, Year (2010-2020)") +
      ylab("Unemployment Rate") +
      theme(axis.text.x = element_blank())
  })
  
  #Indicate the selected Continent 
  output$textguy <- renderText({ 
    paste("You are currently looing at the recorded unemployment rate of ", input$race, ".", 
          "Carefully examine the change in scale differs by each different Race. Note: one tick accounts for one month.")
  })
  
  # Interactive page 3
  output$date_gender_chart <- renderPlotly({
    unemployment_date_gender_table %>%
      filter(Year %in% input$years) %>%
      filter(Gender %in% input$gender) %>%
      group_by(Gender) %>%
      plot_ly(x = ~Month_num, y = ~`Rate by Gender`, type = "scatter", mode = "lines",
              text = ~paste("Month:", Month, "Year:", Year, "Rate:", `Rate by Gender`), 
              color = ~Gender) %>%
      layout(title = "Unemployment Rates by Date and Gender",
             xaxis = list(title = "Month (in numerical form)"),
             yaxis = list(title = "Unemployment Rate"))
    
  })
}

shinyApp(ui = ui, server = server)


