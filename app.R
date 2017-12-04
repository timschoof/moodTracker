# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# This app...

# To Do:
# - https://ipub.com/shiny-crud-app/
# - pull previously entered data and let people plot it 
#  (e.g mood over time, by time of day or day of the week)
# - insert Anna's image (ideally: click on those icons!)
# - multiple pages via navbar (one for data entry, one for plotting) https://shiny.rstudio.com/gallery/navbar-example.html
# - make it look nice

library(shiny)

# Define UI for application
ui <- fluidPage(
  # App title
  navbarPage("Mood Tracker",
             tabPanel("Record",
                      mainPanel(
                        #img(src='goat.jpg', align = "right"),
                        
                        sliderInput(inputId = "mood", label = "How was your day?",value=3,min=1,max=5),
                        
                        actionButton("submit", "Submit"),
                        
                        textOutput("submitMessage")
                      )
             ),
             tabPanel("Plot",
                        sidebarPanel(
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
  
  # specify output directory
  outputDir<-"C:/Users/tim/Documents/DataScienceProjects/moodTracker/"
  
  # specify output filename
  fileName <- "MyMood.csv"
  
  # read data - if file exists
  if(file.exists(file.path(outputDir,fileName))){
    d<-read.csv(file.path(outputDir,fileName),header=TRUE)
  }
  
  # execute when user clicks 'submit' button
  observeEvent(input$submit, {
    md = reactive({
      md <- input$mood
      return(md)
    })
    
    hr <- as.numeric(format(Sys.time(),format="%H"))
    if(hr < 12 & hr > 4){
      t.o.d.<- "morning"
    }else if(hr < 18 & hr >= 12){
      t.o.d.<-"afternoon"
    }else if(hr > 18 & hr < 24) {
      t.o.d.<- "evening"
    }else{
      t.o.d.<-"night"
    }
    
    data<-data.frame(
      "date" = format(Sys.time(), format="%d %b %Y"),
      "day" = format(Sys.time(), format="%A"),
      "time" = format(Sys.time(), format="%X"),
      "time.of.day" = t.o.d.,
      "mood" = md()
    )
    
    # append new entry to loaded data sheet - if data has previously been stored
    if(file.exists(file.path(outputDir,fileName))){
      data <- rbind(d,data)
    }
    
    write.csv(x=data,file=file.path(outputDir,fileName),
              row.names=FALSE)
    
    output$submitMessage<-renderText("Thank you!")
  })
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    d[, c(input$xcol, "mood")]
  })
  
  output$plot <- renderPlot({
    plot(selectedData(),ylab="Mood")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

