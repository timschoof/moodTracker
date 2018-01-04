# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above. 
#
# This app allows you to track your mood over time. You can record your mood, on
# a scale from 1 to 5 (from frowny to smiley). The data are saved in a locally 
# stored excel spreadsheet. You can also plot your mood data over time, by day of
# the week, time of day, or by date. 

# NB: The app runs locally on your own computer because you don't necessarily 
# want other people to know what mood you're in!

library(shiny)

# specify output filename
fileName <- "MyMood.csv"

# Define UI for application
ui <- fluidPage(
  navbarPage("Mood Tracker",
             tabPanel("Record",
                      mainPanel(
                        # slider from 1 to 5 used to indicate mood
                        sliderInput(inputId = "mood", label = "How was your day?",value=3,min=1,max=5),
                        
                        # submit button
                        actionButton("submit", "Submit"),
                        
                        # message will be printed once mood has been submitted
                        textOutput("submitMessage")
                      )
             ),
             tabPanel("Plot",
                        sidebarPanel(
                          # select the variable along the x-axis (y-axis is mood score)
                          selectInput('xcol', 'X Variable', c("date", "day","time.of.day"))
                        ),
                      mainPanel(
                        plotOutput("plot")
                      )
             )
  )
)

# Define server logic
server <- function(input, output){
  
  # read data - if file exists
  if(file.exists(here(fileName))){
    d<-read.csv(here(fileName),header=TRUE)
  }
  
  # For recording data
  # execute when user clicks 'submit' button
  observeEvent(input$submit, {
    # extract submitted mood score
    md = reactive({
      md <- input$mood
      return(md)
    })
    
    # record the hour at which mood was submitted
    hr <- as.numeric(format(Sys.time(),format="%H"))
    # infer the time of day based on the hour
    if(hr < 12 & hr > 4){
      t.o.d.<- "morning"
    }else if(hr < 18 & hr >= 12){
      t.o.d.<-"afternoon"
    }else if(hr > 18 & hr < 24) {
      t.o.d.<- "evening"
    }else{
      t.o.d.<-"night"
    }
    
    # create data frame to be saved as csv file
    data<-data.frame(
      "date" = format(Sys.time(), format="%d %b %Y"),
      "day" = format(Sys.time(), format="%A"),
      "time" = format(Sys.time(), format="%X"),
      "time.of.day" = t.o.d.,
      "mood" = md()
    )
    
    # append new entry to loaded data sheet - if data has previously been stored
    if(file.exists(here(fileName))){
      data <- rbind(d,data)
    }
    
    # write data into a csv file
    write.csv(x=data,file=here(fileName),row.names=FALSE)
    
    # print message indicating data has been recorded
    output$submitMessage<-renderText("Thank you!")
  })
  
  # For plotting
  # Combine variables to be plotted into a new data frame
  selectedData <- reactive({
    d[, c(input$xcol, "mood")]
  })
  
  # create the plot
  output$plot <- renderPlot({
    plot(selectedData(),ylab="Mood")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

