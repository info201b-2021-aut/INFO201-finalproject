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

# This creates the table of that mutate the race types into one column. 
unemployment_race_table <- unemployment %>%
  pivot_longer(cols =c("White", "Black", "Asian","Hispanic"), names_to = "Race_type", values_to = "Rate_by_race")


# Intro page
introduction <- tabPanel(
  "Introduction",
    mainPanel(
      h1("Unemployment Rates Through an Analytical Lens", style = "font-family: 'times'; font-si16pt"),
      h4("A Overview on What, Why, and How We Analyzed Data on Unemployment", style = "font-family: 'times'; font-si16pt"),
      p("When asked to find a data set to analyze as a collective for this class, our group seeked to find something ", em("interesting and understandable"), "for the everyday person. With that, we went on to analyze
        various data sets and settled for one on unemployment rates in our nation to analyze. We chose this question for the many questions we had for it at first glance, and upon diving deeper we decided on 
        three to focus on in our analysis. The questions were the following: ", strong("'Has the unemployment rate increased or decreased more over time?', 'What group of people is the most unemployed?', and 'How is education 
        related to unemployment?'."), " Our data set contained all the information necessary to understand and answer these questions, and so with it, we set out to create answers to our questions, which we successfully 
        were able to do and can be found in the tabs within this shiny application.
", p(), img(src = "unemployment.jpg", width = 535), style = "font-family: 'times'; font-si16pt")
  )
)


# Interactive page 3
unemployment_education <- tabPanel(
  "Unemployment by Education Level",
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "radio",
        label = h3("Select an education level:", style = "font-family: 'times'; font-si16pt"),
        choices = list("Primary School" = "Primary_School", "High School" = "High_School", "Associates Degree" = "Associates_Degree", "Professional Degree" = "Professional_Degree")
      )
    ),
    mainPanel(
      plotlyOutput(outputId = "unemployment_education_chart"),
      p("Throughout the years displayed, the average unemployment rate has ", em("consistently decreased as educational levels have risen"), "showing
      that in the U.S. those with higher education levels are less likely to be unemployed. Upon comparing the education level and unemployment rates to each other, it can be seen that ", em("
      each additional level of education drops unemployment rates by an additional 3-4% (out of 100)."), " This demonstrates that despite the lower unemployment rates in recent years,
      it can still be seen that there is bias towards those with higher education, and it can logically be assumed that ", em("the less educated you are, the more likely you are to be unemployed."), style = "font-family: 'times'; font-si16pt")
    )
  )
)

# Interactive page 2

unemployment_race <- 
  tabPanel(
    "Unemployment Data by Race", 
    fluidPage(
 #     h2("Visualization of the unemployment rate by race.")
    ),
        selectInput(inputId = "race", 
                    label = h4("Select the Race", style = "font-family: 'times'; font-si16pt"),
                    choices = list("White" = "White","Black" = "Black", "Asian" =  "Asian","Hispanic" = "Hispanic")
                  ),
    mainPanel(
      plotlyOutput(outputId = "unemployment_race_chart"),
      p("The difference between races and their associated unemployment rates is made apparent with this visualization. Upon comparing White unemployment to Black unemployment, it can be see that there is a drastic 
      change in scale as the percentages change noticeably for the two races. When unemployment rate is compared by each race, it can be seen that the highest unemployment rates come from Black (African American) 
      individuals. The lowest  unemployment rates being from Asian Americans. Also, the trend of ", em("lowered")," rates of unemployment can be observed in ", strong("all")," races.", style = "font-family: 'times'; font-si16pt")
    )
  )


# Interactive page 1

unemployment_date_gender <- tabPanel(
  "Unemployment by Date and Gender",
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "gender",
        label = h3("Select one or more genders:", style = "font-family: 'times'; font-si16pt"),
        choices = list("Men" = "Men", "Women" = "Women")
      ),
      radioGroupButtons(
        "years",
        label = h3("Select a range of years:", style = "font-family: 'times'; font-si16pt"),
        choices = unique(unemployment_date_gender_table$Year)
      )
    ),
    mainPanel(
      plotlyOutput(outputId = "date_gender_chart"),
      p(em("Economic change can change unemployment rates drastically,"), " and analyzing the unemployment rates in a certain month and/or year within this visualization 
      can help describe the economic situation that the U.S. was in during that time. For example, following the 2008 crash with higher than ever unemployment rates, we can see rates recover from 2010 onwards, 
      with the rates going up only a ", em("single"), " year in the last decade (during the COVID crash). In addition, unemployment rates by gender are important to analyze due to the fact that women are known to be treated less fairly 
      than men, and it can be seen that the trend follows in unemployment rates as their rates show to be higher then mens in the visualizations above.", style = "font-family: 'times'; font-si16pt")
    )
  )
)

# Conclusion page
takeaways <- tabPanel(
  "Takeaways",
  mainPanel(
    h1("Our Various Findings on What Affects Unemployment (And How)", style = "font-family: 'times'; font-si16pt"),
    h4("A Brief Summary on Our Questions and Our Takeaways from Their Answers", style = "font-family: 'times'; font-si16pt"),
    p("The questions we started with were the following: ", strong("'Has the unemployment rate increased or decreased more over time?', 'What group of people is the most unemployed?', and 'How is education 
        related to unemployment?'."), " With the analysis seen in our visualizations we found that the answers to our questions were as follows. For our first question we saw that unemployment rates had noticeably 
        decreased every year since 2010, with 2020 (the year of coronavirus) being the first in many where we saw a notable increase. In our visualizations we found that African Americans had the highest rates of 
        unemployment in 2020. We were also able to conclude that as educational levels increased, unemployment rates dropped alongside them. Our major takeaways from this project were that despite initially thinking 
        that unemployment rates would have been increasing yearly, in the last decade they have shown to be decreasing year by year. Minorities were seen to have higher rates of unemployment in comparison to White Americans 
        and those with higher education levels had lower unemployment rates as education increased. Another takeaway was that the notable trend of white men having power in society (in positions of power and law, 
        incarcerations rates, and as talk show hosts even) followed itself out from society into the statistics and showed itself within the visualizations of our project. Our analysis allowed us to realize that 
        minorities and women had the lowest rates of employment, with minority women specifically having drastically lower rates of employment than white men, along with various other conclusions that we were able to 
        come to with the aid of our combined visualizations and analysis.
", p(), img(src = "summary.png", width = 535), style = "font-family: 'times'; font-si16pt")
  )
)

# UI code

ui <- navbarPage(
  "Unemployment Data Analysis",
  introduction,
  unemployment_date_gender,
  unemployment_race,
  unemployment_education,
  takeaways
)

# Server code

server <- function(input, output){
  #Introduction
  
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
      layout(title = "Average Unemployment by Race Per Year",
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
      layout(title = "Average Unemployment Rates by Date and Gender",
             xaxis = list(title = "Month (in numerical form)"),
             yaxis = list(title = "Unemployment Rate"))
    
  })
}

shinyApp(ui = ui, server = server)


