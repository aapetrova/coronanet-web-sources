#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)
library(urltools)

df <- read_csv("https://raw.githubusercontent.com/saudiwin/corona_tscs/master/data/CoronaNet/coronanet_release.csv")
# extract websites from source links
df$link <- domain(df$link)
count_per_country <- df %>%
    group_by(country) %>%
    tally() %>%
    ungroup()
count_per_website <- df %>%
    group_by(country, link) %>%
    tally(sort = TRUE) %>%
    ungroup() %>%
    arrange(country, -n) %>%
    left_join(count_per_country, by="country") %>%
    mutate(n = n.x) %>%
    mutate(prop = n/n.y * 100) %>%
    select(country, link, n, prop) %>%
    arrange(country, desc(prop))

# Define UI for application that draws stacked bar plots
ui <- fluidPage(

    # Application title
    titlePanel("CoronaNet Data Sources"),
    p("This app displays the distribution of the used news sources per country in the CoronaNet dataset."),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectizeInput("countries",
                           "Countries",
                           unique(count_per_website$country),
                           selected = c("Afghanistan", "Albania"),
                           multiple = TRUE,
                           options = list(maxItems = 20),
                           width = "100%"),
            width = "100%"
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("linkDist"),
           width = "100%"
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$linkDist <- renderPlotly({
        req(input$countries)
        
        count_per_website %>%
            filter(country %in% input$countries) %>%
            plot_ly(x=~country,
                    y=~prop,
                    color=~link,
                    text=~link,
                    hovertemplate="%{y:,.2f}%") %>%
            add_bars() %>%
            layout(barmode = "stack") %>%
            layout(showlegend = FALSE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
