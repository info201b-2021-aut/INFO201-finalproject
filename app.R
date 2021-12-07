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

# This creats the table of that mutate the race types into one column. 
unemployment_race_table <- unemployment %>%
  pivot_longer(cols =c("White", "Black", "Asian","Hispanic"), names_to = "Race_type", values_to = "Rate_by_race")


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
  tabPanel(
    "Unemployment Data by Race", 
    fluidPage(
      h2("Visualization of the unemployment rate by race.")
    ),
        selectInput(inputId = "race", 
                    label = h4("Select the Race"),
                    choices = list("White" = "White","Black" = "Black", "Asian" =  "Asian","Hispanic" = "Hispanic")
                  ),
    mainPanel(
      plotlyOutput(outputId = "unemployment_race_chart"),
      p("Carefully examine the change in scale which differs by each selected Race. When the unemplayment rate gets compared by each race, 
        the data suggests the higest unemployment rate in the Black. The lowest was from the Asian. Also, the trend can be observed. In all
        races, there is a strong tendency of increasing in the unemployment rate.")
    )
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
  output$unemployment_race_chart <- renderPlotly({
    unemployment_race_table %>%
      filter(Race_type %in% input$race) %>%
      group_by(Year) %>%
      mutate(rate = mean(Rate_by_race, na.rm = TRUE)) %>%
      plot_ly(x = ~Year, y = ~rate, type = "scatter", mode = "markers",
              text = ~paste("Year:", Year, "Rate:", rate),
              color = ~Race_type,
              colors = "Orange") %>%
      layout(title = "Unemployment of Each selected Race",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Unemployment Rate"))
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


