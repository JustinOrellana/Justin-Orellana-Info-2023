#Justin Orellana
library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)
yes <- aes(label = ifelse(Game == input$Game, Game, ""))
merge_df <- read.csv("MergeDF.csv")

greater <- function(Game){
  gameFil <- filter(merge_df, Game == Game)
  ifelse(gameFil$TotalEarnings >= 8000000, TRUE, FALSE)
}
merge_df$Date <- format(as.Date(merge_df$Date, format="%Y-%m-%d"),"%Y")
merge_df$Greater_8Mil <- greater(merge_df$Game)
mergeLier1 <- filter(merge_df, Greater_8Mil == "TRUE")
mergeLier2 <- filter(merge_df, Greater_8Mil == "FALSE")
mergeNow <- filter(merge_df, Date %in% c("2020", "2021", "2022", "2023"))


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
           img(src="esports.png", height = "60%", width = "60%"),
           br(),
           tags$a(href="https://www.archdaily.com/942235/a-new-type-of-entertainment-the-rise-of-esports-arenas-around-the-globe",
                  "Kaley, 2020, A New Type of Entertainment: The Rise of Esports Arenas Around the Globe",),
           ),

  tabPanel("Overall",
    sidebarLayout(
      sidebarPanel(
        p("This graph allows us to see the overall change that has occured in 
           the past 50 starting from the 1980s - present. We are able to choose
           which game we want to view and see their growth or decline throughout
           the years with the graph that is displayed for that game's Esports 
           scene."),

        selectInput("Game", "Pick a Game", choices = merge_df$Game),
      ),
      mainPanel(
        h3("Game Value Over the Years:"),
        plotlyOutput("scatter")
      )
    ),
  ),
           

  tabPanel("The Now",
    sidebarLayout(
      sidebarPanel(
        p("If we zoom in to our data we can see the changes in the most recent 
           years to see if there had been any big impacts. With the scrolldown 
           we can chooose the game and the graph will display the games data 
           throughout 2020-2023"),
        selectInput("GameNow", "Pick a Game", choices = merge_df$Game),
             ),
      mainPanel(
        h3("Growth in the Past 3 Years:"),
           plotlyOutput("scatterNow")
           ),
    ),
  ),
  tabPanel("Outliers",
    sidebarLayout(
      sidebarPanel(
        p("We have quite a few industries that invest a lot in their Esports
           scene, if we take out these outliers we are able to see a more 
           streamlined graph on what the average Esports scene looks like. We 
           can choose to either have above 8 million or below 8 million to change
           the graph. The reason for 8 millions is that thats when it starts to 
           become the top of our box and whisker point."),
        radioButtons("radio", label = h3("Game Outliers"),
                     choices = list("Above 8M" = 1, 
                                    "Below 8M" = 2), 
                     selected = 1),
      ),
           mainPanel(
             h3("Outliers over time:"),
             plotlyOutput("scatterFar")
           )
)
),
tabPanel("Conclusion",
h1("In my opinion"),
p("If we go through the graphs we get to see a very healthy growth throughout 
  the years with some really big growth for some industries such as Fortnite
  and Dota 2. We see in the end we do see a little of plateu in the past
  3 years, that can either be because the data hasn't been entered or that it's
  an aftereffect from our recent pandemic of corona that destroyed a lot of 
  growth in the industry. In my opinion I think Esports is doing alright as a 
  business, it just went through some unfortunate problems with covid and the 
  economy dying. Overall I expect Esports to keep on growing and that people be
  more invested into Esports as this problem should be studied more and observed
  as it is a business in its infancy and is going through much needed growing 
  pains. This is important for the investors and the people out in the world 
  looking to support Esports."),
h3("Thank you for looking through my website!"),
),         
)

server <- function(input, output) {
  
  rv <- reactiveValues(
    i  = NULL,
    df = NULL,
    n = NULL
  )
  
  observe({ if (input$radio == 1) {
    rv$i <- mergeLier1
  }
  })

  observe({ if (input$radio == 2) {
    rv$i <- mergeLier2
  }
  })
  
  observe({ rv$df <- input$Game
  })
  
  observe({ rv$n <- input$GameNow
  })
  
  output$scatter <- renderPlotly({
    merge_df1 <- filter(merge_df, Game == rv$df) 
    p <- ggplot(merge_df1, aes(x = Date , y = Earnings, text = Game)) +
      geom_point(aes(color = Game))
    
    p <- ggplotly(p)
    return(p)
    
  })
  
  output$scatterNow <- renderPlotly({

    mergeNow1 <- filter(mergeNow, Game == rv$n) 
    p1 <- ggplot(mergeNow1, aes(x = Date , y = Earnings, text = Game)) +
      geom_point(aes(color = Game))
    
    p1 <- ggplotly(p1)
    return(p1)
    
  })

  output$scatterFar <- renderPlotly({

    p2 <- ggplot(rv$i, aes(x = Date , y = Earnings, text = Game)) +
      geom_point(aes(color = Game)) 

    
    p2 <- ggplotly(p2, tooltip = "text")
    return(p2)
  })
}

shinyApp(ui = ui, server = server)
