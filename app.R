#install.packages("shinyWidgets")
library(shiny)
library(tidyverse)
library(plotly)
library(shinyWidgets)

unemployment <- read.csv("https://raw.githubusercontent.com/info201b-2021-aut/final-project-ydodobara/main/unemployment_data_us.csv?token=AV5HNBIXZBOMVUYCBJUB4D3BW2RJ4")

# This creates a column called "Month_num" that changes month abbreviations into numbers (Jan = 1, etc.) and creates a column called "Gender" with values from both "Men" and "Women" columns
unemployment <- unemployment %>%
  mutate(Month_num = match(unemployment$Month, month.abb)) %>%
  pivot_longer(cols = c("Men", "Women"), names_to = "Gender", values_to = "Rate by Gender") 

# Intro page

# Interactive page 1


# Interactive page 2


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
        choices = unique(unemployment$Year)
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
  unemployment_date_gender
)

# Server code

server <- function(input, output){
  output$date_gender_chart <- renderPlotly({
    unemployment %>%
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

