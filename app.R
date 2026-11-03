
# ============================================================
# PROJECT: An Interactive Analysis of Social Media Usage
#          and Screen Time
# APPLICATION: R Shiny Dashboard
# ============================================================

# 1. LOAD REQUIRED PACKAGES

library(shiny)
library(ggplot2)
library(DT)

# 2. IMPORT DATASET

social_media <- read.csv(
  "social_media_usage_screen_time_sample.csv"
)

# Remove duplicate and incomplete records
social_media <- social_media[
  !duplicated(social_media), ]

social_media <- na.omit(social_media)

# Convert categorical columns
social_media$Gender <- as.factor(social_media$Gender)
social_media$Social_Media_Platform <-
  as.factor(social_media$Social_Media_Platform)
social_media$Usage_Purpose <-
  as.factor(social_media$Usage_Purpose)
social_media$Daily_Usage_Frequency <-
  as.factor(social_media$Daily_Usage_Frequency)
social_media$Productivity_Level <-
  as.factor(social_media$Productivity_Level)
social_media$Social_Media_Impact <-
  as.factor(social_media$Social_Media_Impact)

# 3. USER INTERFACE

ui <- fluidPage(
  
  titlePanel(
    "Social Media Usage and Screen Time Dashboard"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Dashboard Filters"),
      
      selectInput(
        "gender",
        "Select Gender:",
        choices = c(
          "All",
          levels(social_media$Gender)
        ),
        selected = "All"
      ),
      
      selectInput(
        "platform",
        "Select Social Media Platform:",
        choices = c(
          "All",
          levels(social_media$Social_Media_Platform)
        ),
        selected = "All"
      ),
      
      selectInput(
        "purpose",
        "Select Usage Purpose:",
        choices = c(
          "All",
          levels(social_media$Usage_Purpose)
        ),
        selected = "All"
      ),
      
      selectInput(
        "productivity",
        "Select Productivity Level:",
        choices = c(
          "All",
          levels(social_media$Productivity_Level)
        ),
        selected = "All"
      ),
      
      sliderInput(
        "screen_time",
        "Daily Screen Time Range (Hours):",
        min = floor(min(
          social_media$Daily_Screen_Time_Hours
        )),
        max = ceiling(max(
          social_media$Daily_Screen_Time_Hours
        )),
        value = c(
          floor(min(
            social_media$Daily_Screen_Time_Hours
          )),
          ceiling(max(
            social_media$Daily_Screen_Time_Hours
          ))
        )
      )
    ),
    
    mainPanel(
      
      h3("Overview"),
      
      fluidRow(
        
        column(
          4,
          wellPanel(
            h4("Total Respondents"),
            textOutput("total_respondents")
          )
        ),
        
        column(
          4,
          wellPanel(
            h4("Average Screen Time"),
            textOutput("average_screen")
          )
        ),
        
        column(
          4,
          wellPanel(
            h4("Average Sleep Hours"),
            textOutput("average_sleep")
          )
        )
      ),
      
      hr(),
      
      tabsetPanel(
        
        tabPanel(
          "Platform Analysis",
          br(),
          plotOutput("platform_plot")
        ),
        
        tabPanel(
          "Screen Time Analysis",
          br(),
          plotOutput("screen_plot")
        ),
        
        tabPanel(
          "Sleep Analysis",
          br(),
          plotOutput("sleep_plot")
        ),
        
        tabPanel(
          "Productivity Analysis",
          br(),
          plotOutput("productivity_plot")
        ),
        
        tabPanel(
          "Usage Purpose",
          br(),
          plotOutput("purpose_plot")
        ),
        
        tabPanel(
          "Data Table",
          br(),
          DTOutput("data_table"),
          br(),
          downloadButton(
            "download_data",
            "Download Filtered CSV"
          )
        )
      )
    )
  )
)

# 4. SERVER

server <- function(input, output, session) {
  
  # Filter dataset based on user selections
  
  filtered_data <- reactive({
    
    data <- social_media
    
    if (input$gender != "All") {
      data <- data[
        data$Gender == input$gender, ]
    }
    
    if (input$platform != "All") {
      data <- data[
        data$Social_Media_Platform ==
          input$platform, ]
    }
    
    if (input$purpose != "All") {
      data <- data[
        data$Usage_Purpose == input$purpose, ]
    }
    
    if (input$productivity != "All") {
      data <- data[
        data$Productivity_Level ==
          input$productivity, ]
    }
    
    data <- data[
      data$Daily_Screen_Time_Hours >=
        input$screen_time[1] &
        data$Daily_Screen_Time_Hours <=
        input$screen_time[2], ]
    
    data
  })
  
  # SUMMARY CARDS
  
  output$total_respondents <- renderText({
    nrow(filtered_data())
  })
  
  output$average_screen <- renderText({
    
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return("No data")
    }
    
    paste0(
      round(
        mean(data$Daily_Screen_Time_Hours),
        2
      ),
      " hours"
    )
  })
  
  output$average_sleep <- renderText({
    
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return("No data")
    }
    
    paste0(
      round(mean(data$Sleep_Hours), 2),
      " hours"
    )
  })
  
  # GRAPH 1: PLATFORM DISTRIBUTION
  
  output$platform_plot <- renderPlot({
    
    ggplot(
      filtered_data(),
      aes(
        x = Social_Media_Platform,
        fill = Social_Media_Platform
      )
    ) +
      geom_bar() +
      labs(
        title = "Social Media Platform Distribution",
        x = "Platform",
        y = "Number of Respondents"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # GRAPH 2: SCREEN TIME DISTRIBUTION
  
  output$screen_plot <- renderPlot({
    
    ggplot(
      filtered_data(),
      aes(x = Daily_Screen_Time_Hours)
    ) +
      geom_histogram(
        bins = 10,
        fill = "steelblue",
        color = "white"
      ) +
      labs(
        title = "Daily Screen Time Distribution",
        x = "Screen Time (Hours)",
        y = "Number of Respondents"
      ) +
      theme_minimal()
  })
  
  # GRAPH 3: SCREEN TIME AND SLEEP
  
  output$sleep_plot <- renderPlot({
    
    data <- filtered_data()
    
    p <- ggplot(
      data,
      aes(
        x = Daily_Screen_Time_Hours,
        y = Sleep_Hours
      )
    ) +
      geom_point(
        color = "darkgreen",
        size = 3
      ) +
      labs(
        title = "Screen Time and Sleep Hours",
        x = "Daily Screen Time (Hours)",
        y = "Sleep Hours"
      ) +
      theme_minimal()
    
    if (nrow(data) >= 2) {
      p <- p + geom_smooth(
        method = "lm",
        se = FALSE
      )
    }
    
    p
  })
  
  # GRAPH 4: PRODUCTIVITY LEVEL
  
  output$productivity_plot <- renderPlot({
    
    ggplot(
      filtered_data(),
      aes(
        x = Productivity_Level,
        fill = Productivity_Level
      )
    ) +
      geom_bar() +
      labs(
        title = "Respondents' Productivity Levels",
        x = "Productivity Level",
        y = "Number of Respondents"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # GRAPH 5: USAGE PURPOSE
  
  output$purpose_plot <- renderPlot({
    
    ggplot(
      filtered_data(),
      aes(
        x = Usage_Purpose,
        fill = Usage_Purpose
      )
    ) +
      geom_bar() +
      labs(
        title = "Purpose of Social Media Usage",
        x = "Usage Purpose",
        y = "Number of Respondents"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # DATA TABLE
  
  output$data_table <- renderDT({
    
    datatable(
      filtered_data(),
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # DOWNLOAD FILTERED DATA
  
  output$download_data <- downloadHandler(
    
    filename = function() {
      "filtered_social_media_data.csv"
    },
    
    content = function(file) {
      write.csv(
        filtered_data(),
        file,
        row.names = FALSE
      )
    }
  )
}

# 5. RUN APPLICATION

shinyApp(
  ui = ui,
  server = server
)
