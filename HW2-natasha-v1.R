library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)

migrants <- read.csv("migrants2.csv")

#Defining the UI for application 
#I will use a FluidPage instead of navbar

ui<-fluidPage(
  # Typo
        titlePanel("Migrants Deaths across the World"),
        sidebarLayout(
          sidebarPanel(
            #Month Selection
            selectInput("month_Select",
                        "month:",
                        choices = levels(migrants$Reported.Month),
                        multiple = TRUE,
                        selectize = TRUE,
                        selected = c("")),
            # Year Selection
            sliderInput("yearSelect",
                        "Year:",
                        min = min(migrants$Reported.Year, na.rm = T),
                        max = max(migrants$Reported.Year, na.rm = T),
                        value = c(min(migrants$Reported.Year, na.rm = T), max(migrants$Reported.Year, na.rm = T)),
                        step = 1),
            #Cause of Death Selection
            selectInput("cause_Select", h3("Select Cause"),
                        choices= levels(migrants$Cause.of.Death),
                        selected=0),
            actionButton("reset", "Reset Filters", icon = icon("refresh"))
          ),
          mainPanel(
            tabsetPanel(
            tabPanel("plot1",
                        plotlyOutput("plot1")
                      ),
            tabPanel("plot2",
                     plotlyOutput("plot2")
            ),
            tabPanel("plot3",
                     plotlyOutput("plot3")
            ),
            tabPanel("table",
                     inputPanel(
                       downloadButton("downloadData","Download Migrants Deaths ")
                     ),
                     fluidPage(DT::dataTableOutput("table"))
                     )
       )
     )
    )
)
# Define server logic
server <- function(input, output, session = session) {
  # Filtered Starwars data
  migrantsInput <- reactive({
    migrants <- migrants %>%
      # Slider Filter
      # Your select input for cause is single and so it can never be blank, it makes more sense to put up here
      filter(Reported.Year >= input$yearSelect[1] & Reported.Year <= input$yearSelect[2] & Cause.of.Death == input$cause_Select)
    # Month and Cause of Dead Filter
    if (length(input$month_Select) > 0) {
      # This filter isn't really correct. For something like this you'll need to work with dates.
      # dateRangeInput() instead of the year slider and month would work MUCH better
      migrants <- subset(migrants, Reported.Month %in% input$month_Select)
    }
    # This won't work under certain instances
    return(migrants)
  })
  # Bar plot showing Number of Deads per Year
  output$plot1 <- renderPlotly({
    migrants <- migrantsInput()
    ggplotly(
      ggplot(data = migrants, aes(x = Reported.Year, y = Number.Dead)) + 
        # Identity is breaking this
        geom_bar(stat="identity")+
        labs(x="Year", y= "Number of Deads", title="Number of Deads per Year"))
  })
  # Point plot showing Number of Deads per Region
  output$plot2 <- renderPlotly({
    migrants<- migrantsInput()
    # You need to make a table to base this plot off of.
     ggplotly(
       ggplot(data = migrants, aes(x = Region, y = Number.Dead)) + 
        geom_point()+
         labs(x="Region",y="Number of Deads", title="Number of Deads per Region"))
  })
   output$plot3 <- renderPlotly({
     migrants <- migrantsInput()
      ggplotly(
        ggplot(data = migrants, aes(x = Reported.Year, y = Number.Dead, fill=Region)) + 
          geom_histogram()+
           labs(x="Year", y="Number of Deads",title="Number of Dead per Year and Region"))
  })
  # Data Table
  output$table <- DT::renderDataTable({
    migrants <- migrantsInput()
    subset(migrants, select = c(Region, Reported.Date, Number.Dead, Total.Dead.and.Missing, Number.of.Females, Number.of.Males, Cause.of.Death))
  })
  # Download data in the datatable
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("migrants-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(migrantsInput(), file)
    }
  )
  # Reset Filter Data
  observeEvent(input$reset, {
    updateSelectInput(session, "month_Select", selected = c(""))
    updateSliderInput(session, "YearSelect", value = c(min(migrants$Reported.Year, na.rm = T), max(migrants$Reported.Year, na.rm = T)))
    updateSelectInput(session,"cause_Select", selected =c (""))
    showNotification("You have successfully reset the filters", type = "message")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)