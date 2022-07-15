library(shiny)
library(shinydashboard) 
library(leaflet)
library(DT)
library(plotly)
library(tidyverse)
library(ggplot2)


ui <- dashboardPage(
    skin = "red",
    dashboardHeader(title = "Netflix Movie Engine"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Overview", tabName = "page1", icon = icon("couch")),
            menuItem("Search", tabName = "page2", icon = icon("film")),
            menuItem("Map", tabName = "page3", icon = icon("tape")),
            menuItem("General Analysis",tabName = "page4",icon = icon("video"),
                     selected = T,
                menuItem(
                    "Genre Trend",
                    tabName = "page5",
                    icon = icon("file-video")),
                menuItem(
                    "Other Trends",
                    tabName = "page6",
                    icon = icon("compact-disc"))),
            menuItem("Filters", tabName = "page7", icon = icon("fas fa-ticket-alt"))
        )
    ),
    dashboardBody(
        tags$head( 
            tags$style(HTML(".main-sidebar { font-size: 16px; }"))
        ),
        tabItems(
            tabItem(tabName = "page1",
                    fluidRow(
                        box(
                            title = "Project description", solidHeader = TRUE,
                            status = "success", width = 12, collapsible = TRUE,
                            column(12, 
                                   fluidRow(column(12,"Visualization Web Application for Netflix Movies ."), style = "font-size:16px"),
                                   br(),
                                   fluidRow(column(12,"para2."), style = "font-size:16px"),
                                   br(),
                                   fluidRow(column(12,"para3."), style = "font-size:16px")
                                   
                            )#cloumn12
                        )
                    ),#fluidrow
                    
                    
                    fluidRow(
                        box(
                            title = "About the Dataset", solidHeader = TRUE,
                            status = "primary", width = 12, collapsible = TRUE,
                            column(12, 
                                   fluidRow(column(12,"description. "), style = "font-size:16px")
                                   
                            )#cloumn12
                        )
                    ),#fluidrow
                    
                    fluidRow(
                        box(
                            title = "About Us", solidHeader = TRUE, 
                            status = "info", width = 12, collapsible = TRUE,
                            column(12,fluidRow(column(12,"information. "), style = "font-size:16px"))))
            ),
            
            tabItem(tabName = "page2",
                    h2(strong("Choose Your Movie")),
                    fluidRow(
                        column(12, box(width = 4, tags$div(
                            img(src="Netflix-logos.jpg",alt="Netflixlogos",width="100%"),
                            style="text-align:center"),
                            br(),
                            selectInput("title", "Movie",
                                        choices= 0, 
                                        selected = 0)
                        ),
                        box(width = 8, 
                            mainPanel(
                                h2("Description"),
                                uiOutput("description"),
                                br(),br(),br(),br(),br(),br(),br(),br(),br()
                            )
                        )
                        )
                    )
            ),
        
            tabItem( 
                tabName = "page3", 
                h1("Map"),
                fluidRow(
                    h3("Year filter"), 
                    column( 
                        width = 4, 
                        tabBox( 
                            width = 12, 
                            id = "tabset1", 
                            height = "250px", 
                            tabPanel( 
                                title = "Total Movie Made by Country", 
                                radioButtons( 
                                    "radio1", 
                                    "Year Filter", 
                                    choices = list("With year filter" , "Without year filter"), 
                                    selected = "With year filter" 
                                ) 
                            ) 
                        ) 
                    ), 
                    column(width = 8, 
                           box( 
                               width = 12, 
                               plotOutput(outputId = "plot1", 
                                          height = 500) 
                           )), 
                    br(), 
                    br(), 
                    h3("   Avg Ratings by Country"), 
                    column( 
                        width = 4, 
                        tabBox( 
                            width = 12, 
                            id = "tabset1", 
                            height = "250px", 
                            tabPanel( 
                                title = "Avg Ratings by Country", 
                                sliderInput( 
                                    inputId = "ep2", 
                                    label = "Year", 
                                    min = 2011, 
                                    max = 2016, 
                                    value = 2011,
                                    step = 1,
                                    animate = animationOptions(interval = 2600,loop = TRUE)
                                ) 
                            ) 
                        ) 
                    ), 
                    column(width = 8, 
                           box( 
                               width = 12, 
                               plotOutput(outputId = "plot2", 
                                          height = 500) 
                           )), 
                    br(), 
                    br() 
                ) 
            ),

            tabItem(
                tabName = "page5",
                titlePanel("Genre Trends"),
                br(),
                br(),
                tabsetPanel(
                    tabPanel(
                        "Genre Trends Over Years",
                        fluidRow(
                            sliderInput(
                                "Plot31Input",
                                "Select a year to show word cloud of genres:",
                                min = 1945,
                                max = 2022,
                                value = 1945,
                                step = 1,
                                width = "90%",
                                animate = animationOptions(interval = 1000, loop = FALSE)
                            ),
                            br(),
                            column(11,style = "background-color: white;",
                                   plotOutput("plot31", width = "100%")
                            )
                        )
                    ),
                    tabPanel(
                        "Genre Trends Over Countries",
                        fluidRow(
                            sliderInput(
                                "Plot32Input",
                                "Select a year to show word cloud of genres:",
                                min = 1945,
                                max = 2022,
                                value = 1945,
                                step = 1,
                                width = "90%",
                                animate = animationOptions(interval = 1000, loop = FALSE)
                            ),
                            selectInput(
                                "select",
                                label = "Choose a country",
                                width = "90%",
                                choices = list("US", "CA", "UK", "FR", "IN", "JP"),
                                selected = 1
                            )
                        ),
                        br(),
                        column(11, style = "background-color: white;",
                               plotOutput("plot32")
                        ),
                        fluidRow(br(),br(),br(),br(),br(),br(),
                                 mainPanel(br())
                        )
                    )
                )
            ),
            tabItem(
                tabName = "page6",
                h2("Other trends over years:"),
                fluidRow(
                    sliderInput(
                        "be_yr",
                        "Select begin year:",
                        min = 1945,
                        max = 2022,
                        value = 1945,
                        step = 1,
                        width = "90%"
                    ),
                    sliderInput(
                        "en_yr",
                        "Select end year:",
                        min = 1955,
                        max = 2022,
                        value = 2022,
                        step = 1,
                        width = "90%"
                    ),
                    selectInput(
                        "select",
                        label = "Choose a variable ",
                        width = "90%",
                        choices = list(
                            "number of movies" = "num_movie",
                            "average runtime" = "runtime",
                            "imdb score" = "imdb_score",
                            "imdb vote" = "imdb votes",
                            "average popularity" = "tmdb_popularity"
                        ),
                        selected = 1
                    )
                ),
                br(),
                column(11, style = "background-color: white;",
                       plotOutput("plot33")
                ),
                fluidRow(br(),br(),br(),br(),br(),br(),
                         mainPanel(br())
                )
            ),  

        tabItem(tabName = "page7",
                titlePanel("Discover the Movies You Like"),
                fluidRow(
                    column(12,
                              box(width = 3,
                        dateInput("Start Date", "Start Date"),
                        dateInput("End Date", "End Date"),
                        sliderInput("Mruntime",
                                    label = "Runtime in min",
                                    min = 0, max = 240, value = 0),
                        sliderInput("Mrating",
                                    label = "IMDb Rating",
                                    min = 0, max = 10, value = 0),
                        selectInput(inputId = "Mgenre",
                                    label = "Genres",
                                    choices = 0),
                        selectInput(inputId = "Mcountry",
                                    label = "Production Country",
                                    choices = 0),
                        selectInput(inputId = "Mdirector",
                                    label = "Director",
                                    choices = 0),
                        actionButton(inputId = "actButton",
                                     label="Search",
                                     icon=icon('play-circle'))
                    ),
                box(width = 9,
                    height = "700px",
                    mainPanel(
                        tabsetPanel(
                            tabPanel(
                                "Movie List",
                                br()
                                ),
                            tabPanel(
                                "View Details",
                                br()
                                )
                        ))
                    )
                )
        )
    )  
    )
)
)
    






server <- function(input, output, session) {
    
}


shinyApp(ui = ui, server = server)

