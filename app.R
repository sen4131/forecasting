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
      plotOutput("tsplot1")
    )
  )
)
server <- function(input, output) {
  
  output$tsplot1 <- renderPlot({
    #ts.plot(ts(df[input$Series]), main = "Time series Plot", ylab = input$Series)
    x <- ts(na.omit(df[input$Series]), frequency = 7)
    
    if (input$model == "stl"){
      
      
      #2.1 STL decomposition (Seasonal and Trend decomposition using Loess)
   
      fit1 <- stl(x, s.window=7)
      fc_stl <- forecast(fit1,h=14)

      autoplot(x) +
        autolayer(fc_stl, series="STL", PI=FALSE)+
        guides(colour=guide_legend(title="Daily forecasts"))
      
      checkresiduals(fc_stl)
      
    } else if (input$model == "nnetar"){
      
      fit2 <- nnetar(x, lambda=0)
      fc_nn <- (forecast(fit2,h=25))
      
      autoplot(x) +
        autolayer(fc_nn, series="Neural Nets", PI=FALSE)+
        guides(colour=guide_legend(title="Daily forecasts"))
      
      checkresiduals(fc_nn)
      
    } else if (input$model == "auto.arima"){
      
    } else if (input$model == "hw"){
      
    }
    

  })
  
}
shinyApp(ui = ui, server = server)
