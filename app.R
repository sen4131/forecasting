library(shiny)
#https://www.r-bloggers.com/building-shiny-apps-an-interactive-tutorial/
df <- read.csv("C:/Users/P2877594/Desktop/R stuff/offeredGroupChannel.csv")
df <- data.frame(df)
names(df) <- c("Date","Digital","DM","General","In_matrix","Media")
ui <- fluidPage(
  
  titlePanel("Gross Calls"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("typeInput", "Group",choices = c("Date","Digital","DM","General","In_matrix","Media"))
    ),
    mainPanel("the results will go here")
  )
)
server <- function(input, output, session) {
  print(str(df))
}
shinyApp(ui = ui, server = server)
