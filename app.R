#second attempt at application, application works better than first 
library(shiny)
library(ggplot2)
library(fpp3)
library(plotly)
library(ggeasy)

data(souvenirs)

ui <- fluidPage(
  #shinythemes::themeSelector(), possibly add later 
  theme = bslib::bs_theme(bootswatch = "lux"),
  sidebarLayout(
    sidebarPanel(
      textOutput("panel"),
    ),
    mainPanel(
      tabsetPanel(
        id = "tabset",
        #first panel for instructions and description of data 
        tabPanel("Instructions", 
                 h5("Data Overview"),
                 "The data used in this app comes from the 'souvenirs' data set
                 in the fpp3 package. The data is for the monthly sales for
                 a souvenir shop on the wharf at a beach resort town in
                 Queensland, Australia.", 
                 h5("Instructions"),
                 "The purpose of this application is to plot and analyze time 
                 series data for the souvenirs data set. Plots are divided up 
                 into four sections: Descriptions, Simple Models, Exponential 
                 Smoothing, and ARIMA. Corresponding plots are shown in each of 
                 these tabs according to the label. Select a tab to view plots 
                 for the souvenirs data set. Follow additional instructions on 
                 tabs if necessary.",
                 h5(" "),
                 "The current tab that you have selected is displayed in the top 
                 left corner of the application.",
                 h5(" ")
        ),
        
        #descriptions of data
        tabPanel("Data Interpretations",
                 radioButtons(inputId = "plot_type" , 
                              label = "On this tab there are four plots with plot analyses 
                 provided. Select a plot and description to view from the options 
                 below:", 
                              choices = c("Autoplot", "Seasonality", "ACF", "Decomposition")),
                 plotOutput("myplot"),
                 textOutput("mytext")
        ),
        
        #naive models plot
        tabPanel("Simple Models", 
                 h5("Naive Model"),
                 plotOutput("plot5"),
                 h5("Seasonal Naive Model"),
                 plotOutput("plot6"),
                 h5("Mean Model"),
                 plotOutput("plot7"),
                 h5("Drift Model"),
                 plotOutput("plot8"),
        ),
        
        #exponential smoothing tab
        tabPanel("Exponential Smoothing", 
                 "Please allow up to a minute for these models to load on the 
                  page.",
                 h5("Holt's Model"),
                 plotOutput("plot9"),
                 h5("Holt-Winters’ Model"),
                 plotOutput("plot10")
        ),   
        
        #ARIMA models
        tabPanel("ARIMA Models",
                 "Please allow up to a minute for these models to load on the 
                  page.",
                 h5("ARIMA: Automatically selected parameters"),
                 plotOutput("plot11"),
                 h5("ARIMA: Manually selected parameters"),
                 plotOutput("plot12"),
        ),
        
      )
    )
  )
)


server <- function(input, output, session) {
  #current page feature
  output$panel <- renderText({
    paste("Current Tab: ", input$tabset)
  })
  
  #description plots output
  output$myplot <- renderPlot({
    if (input$plot_type == "Autoplot") {
      autoplot(souvenirs)
    } else if (input$plot_type == "Seasonality") {
      gg_season(souvenirs)
    }
    else if (input$plot_type == "ACF") {
      acf(souvenirs)
    }
    else if (input$plot_type == "Decomposition") {
      #finish
    }
  })
  output$mytext <- renderText({
    if (input$plot_type == "Autoplot") {
      "The autoplot of the data shows strong seasonality. There is a peak towards 
      the end of each year. There is also a trend in the data as it rises dramatically 
      towards the year 1994, the end of the data set."
    } else if (input$plot_type == "Seasonality") {
      "The seasonal plot shows strong seasonality in the data. There is a small peak 
      around March each year and a large peak around the month of December each year. 
      Sales appear to increase overall with each additional year."
    }
    else if (input$plot_type == "ACF") {
      "The ACF (auto correlation function) plot"
    }
    else if (input$plot_type == "Decomposition") {
      "Description"
    }
  })
  
  #simple models output
  output$plot5 <- renderPlot({
    souvenirs %>% model(NAIVE(Sales)) -> fit
    fit %>% forecast(h = 12) %>% autoplot(souvenirs)
  })
  output$plot6 <- renderPlot({
    souvenirs %>% model(SNAIVE(Sales ~ lag("year"))) -> fit2
    fit2 %>% forecast(h = 12) %>% autoplot(souvenirs)
  })
  output$plot7 <- renderPlot({
    souvenirs %>% model(MEAN(Sales)) -> fit3
    fit3 %>% forecast(h = 12) %>% autoplot(souvenirs)
  })
  output$plot8 <- renderPlot({
    souvenirs %>% model(RW(Sales ~ drift())) -> fit4
    fit4 %>% forecast(h = 12) %>% autoplot(souvenirs)
  })
  
  #holts and holts/winters models
  output$plot9 <- renderPlot({
    souvenirs %>% model(ETS = ETS(Sales ~ error("A") + trend("A") + season("N"))) -> fit5
    fit5 %>% forecast(h = 12) %>% autoplot(souvenirs)
  })
  output$plot10 <- renderPlot({
    souvenirs %>% model(ETS(Sales ~ error("M") + trend("A") + season("M"))) -> fit6
    fit6 %>% forecast(h = 12) %>% autoplot(souvenirs)
  })
  
  #ARIMA models 
  output$plot11 <- renderPlot({
    souvenirs %>% model(ARIMA(Sales)) -> fit7
    fit7 %>% forecast(h = 12) %>% autoplot(souvenirs)
  })
  output$plot12 <- renderPlot({
    souvenirs %>% model(ARIMA(Sales ~ pdq(4, 1, 0))) -> fit8
    fit8 %>% forecast(h = 12) %>% autoplot(souvenirs)
  })
}

shinyApp(ui = ui, server = server)

