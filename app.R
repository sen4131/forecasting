library(shiny)
library(forecast)

#Resources
#https://www.r-bloggers.com/building-shiny-apps-an-interactive-tutorial/
#https://deanattali.com/blog/building-shiny-apps-tutorial/

df <- read.csv("C:/Users/P2877594/Desktop/R stuff/offeredGroupChannel.csv")
df <- data.frame(df)
names(df) <- c("Date","Digital","DM","General","In_matrix","Media")
df$Date = as.Date(df$Date, '%m/%d/%Y')

ui <- fluidPage(

  titlePanel("Heading"),
  
  sidebarLayout(
    
    sidebarPanel(
      radioButtons("Series", "Series", c("Digital","DM","General","In_matrix","Media"), "Digital") ,
      
      radioButtons("model", "Forecast Model:",
                   c("STL decomposition" = "stl",
                    "Neural Nets" = "nnetar",
                    "Auto-arima" = "auto.arima",
                    "Exp smoothing" = "hw"), "stl")
    ),
    mainPanel(
      plotOutput("tsplot1"), plotOutput("resid"), tableOutput("forecast") 
    )
  )
)
server <- function(input, output) {

  output$tsplot1 <- renderPlot({
    x <- ts(na.omit(df[input$Series]), frequency = 7)
  
    if (input$model == "stl"){
        x %>% stlf(t.window=7, h=24)%>% autoplot()
    } else if (input$model == "nnetar"){
        x %>% nnetar(P=6) %>% forecast(h=24) %>% autoplot()
    } else if (input$model == "auto.arima"){
        x %>% auto.arima() %>% forecast(h=24) %>% autoplot()
    } else if (input$model == "hw"){
        x %>% hw(damped = TRUE, seasonal="multiplicative", h=24)  %>% autoplot()
    }
    
  })
  
  output$resid <- renderPlot({
    x <- ts(na.omit(df[input$Series]), frequency = 7)
    
    if (input$model == "stl"){
      checkresiduals(x %>% stlf(t.window=7))
    } else if (input$model == "nnetar"){
      checkresiduals(x %>% nnetar(P=6) %>% forecast(h=24))
    } else if (input$model == "auto.arima"){
      checkresiduals(x %>% auto.arima() %>% forecast(h=24))
    } else if (input$model == "hw"){
      checkresiduals(x %>% hw(damped = TRUE, seasonal="multiplicative", h=25))
    }
    
  })

  output$forecast <- renderTable({
    x <- ts(na.omit(df[input$Series]), frequency = 7)
    
    if (input$model == "stl"){
      x %>% stlf(t.window=7, h=24)
    } else if (input$model == "nnetar"){
      x %>% nnetar(P=6) %>% forecast(h=24) 
    } else if (input$model == "auto.arima"){
      x %>% auto.arima() %>% forecast(h=24) 
    } else if (input$model == "hw"){
      x %>% hw(damped = TRUE, seasonal="multiplicative", h=24)  
    }
    
  })

  
}
shinyApp(ui = ui, server = server)
