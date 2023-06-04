#Justin Orellana
library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)

merge_df <- read.csv("MergeDF.csv")

ui <- navbarPage(
  "Esports",
  
  theme = bslib::bs_theme(bootswatch = "darkly"),
  tabPanel("Home",
           h1("Is Esports As a Whole Declining?"),
           p("This website let's us look at the amount of cash in tournament prizes which
    allows us to see how financially stable and alive the scene is as a whole. 
    The reason we are looking at the amount of money being generated is that it
    shows the amount of community interaction there is in that specific games
    Esports team either with the money being generated either by crowd funding
    or by having investors. With a bigger economy in the scene it allows for a 
    more healhy scene with more growth and engagement. We will see the overall 
    time span of the amount of earnings to see if the amount in money that is 
    being put in is declining which if it is the scene is percieved as failing.
    The way this data was collected was from wonderful people who report their own
    winnings or observers, this may not be a consistent way of gaining data since
    it's by volunteered data input but this is the only way that this data is 
    kept currently. The website people input the data is, "),
           tags$a(href="https://www.esportsearnings.com/",
                  "Esportsearnings.com",),
           br(),
           img(src="esports.png"),
           tags$a(href="https://www.archdaily.com/942235/a-new-type-of-entertainment-the-rise-of-esports-arenas-around-the-globe",
                  "Kaley, 2020, A New Type of Entertainment: The Rise of Esports Arenas Around the Globe",),
           ),

  tabPanel("Overall",
    sidebarLayout(
      sidebarPanel(
        selectInput("Game", "Pick a Game", choices = merge_df$Game),
      ),
      mainPanel(
        plotlyOutput(outputId = "scatter")
      )
    )
  ),
           

  tabPanel("The Now", 
           plotlyOutput(outputId = "scatterNow")),

  tabPanel("Outliers", 
           plotlyOutput(outputId = "scatterFar")),
)
server <- function(input, output) {
  output$scatter <- renderUI({
    
    p <- ggplot(merge_df, aes(x = Date , y = Earnings, text = Game)) +
      geom_point(aes(color = merge_df$Game)) +
      geom_text(aes(label = ifelse(Game == input$Game, Game, ""))) +
      labs(color = "Game")
    
    p <- ggplotly(p, tooltip = "text")
    return(p)
    
  })
}

shinyApp(ui = ui, server = server)
