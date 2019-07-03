library(shiny)
library(forecast)
library(dplyr)

#Resources
#https://www.r-bloggers.com/building-shiny-apps-an-interactive-tutorial/
#https://deanattali.com/blog/building-shiny-apps-tutorial/

df <- read.csv("C:/Users/P2877594/Desktop/R stuff/offered_group_channel.csv")
df <- data.frame(df)
names(df) <- c("Date","Digital","General","No_metadata","Direct_Marketing","Media")

df$Date = as.Date(df$Date, '%d-%b-%y')

ui <- fluidPage(

  titlePanel("Heading"),
  
  sidebarLayout(
    
    sidebarPanel(
      radioButtons("Series", "Series", c("Digital","General","No_metadata","Direct_Marketing","Media"), "Digital") ,
      br(),
      radioButtons("model", "Forecast Model:",
                   c("STL decomposition" = "stl",
                    "Neural Nets" = "nnetar",
                    "Auto-arima" = "auto.arima",
                    "Exp smoothing" = "hw"), "stl"),
      br(),
      dateRangeInput("date", "Date Range",
                     df$Date[1],df$Date[length(df$Date)]),
      br(),
      numericInput("obs", "Forecast period:", 14)
    ),
    
    mainPanel(
      plotOutput("tsplot1"), plotOutput("resid"), tableOutput("forecast") 
    )
  )
)
server <- function(input, output) {

  output$tsplot1 <- renderPlot({

    df_date_filtered <- filter(df, df$Date > input$date[1] & df$Date < input$date[2])
    x <- ts(na.omit(df_date_filtered[input$Series]), frequency = 7)
  
    if (input$model == "stl"){
        x %>% stlf(t.window=7, h=input$obs)%>% autoplot()
    } else if (input$model == "nnetar"){
        x %>% nnetar(P=6) %>% forecast(h=input$obs) %>% autoplot()
    } else if (input$model == "auto.arima"){
        x %>% auto.arima() %>% forecast(h=input$obs) %>% autoplot()
    } else if (input$model == "hw"){
        x %>% hw(damped = TRUE, seasonal="multiplicative", h=input$obs)  %>% autoplot()
    }
    
  })
  
  output$resid <- renderPlot({
    df_date_filtered <- filter(df, df$Date > input$date[1] & df$Date < input$date[2])
    x <- ts(na.omit(df_date_filtered[input$Series]), frequency = 7)
    
    if (input$model == "stl"){
      checkresiduals(x %>% stlf(t.window=7))
    } else if (input$model == "nnetar"){
      checkresiduals(x %>% nnetar(P=6) %>% forecast(h=input$obs))
    } else if (input$model == "auto.arima"){
      checkresiduals(x %>% auto.arima() %>% forecast(h=input$obs))
    } else if (input$model == "hw"){
      checkresiduals(x %>% hw(damped = TRUE, seasonal="multiplicative", h=input$obs))
    }
    
  })

  output$forecast <- renderTable({
    df_date_filtered <- filter(df, df$Date > input$date[1] & df$Date < input$date[2])
    x <- ts(na.omit(df_date_filtered[input$Series]), frequency = 7)
    
    if (input$model == "stl"){
      stlf(x,t.window=7, h=input$obs)
    } else if (input$model == "nnetar"){
      x %>% nnetar(P=6) %>% forecast(h=input$obs) 
    } else if (input$model == "auto.arima"){
      x %>% auto.arima() %>% forecast(h=input$obs) 
    } else if (input$model == "hw"){
      x %>% hw(damped = TRUE, seasonal="multiplicative", h=input$obs)  
    }
    
  })

  
}
shinyApp(ui = ui, server = server)
