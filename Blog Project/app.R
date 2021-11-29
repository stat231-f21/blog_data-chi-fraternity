# Load necessary packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggrepel)
library(shiny)

#Import data
merged_grad_rates <- read_csv("merged_grad_rates.csv")
merged_earnings <- read_csv("merged_earnings.csv")

#############################################################
# Define choice values and labels for widgets (user inputs) #
# - Define vectors for choice values and labels             #
# - Can then refer to them in server                        #
#############################################################

# For TAB 1: EARNINGS widgets:
## For selectInput,
demographic_choices <- merged_earnings$demographic
degree_choices <- merged_earnings$`Degree level completed`

############
#    ui    #
############
ui <- navbarPage(
  title = "College Outcomes",
  
  # Tab 1: Earnings
  tabPanel(
    title = "Earnings Line Graph",
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "barvar1",
                    label = "Choose a demographic to plot:",
                    choices = demographic_choices, 
                    selected = "All Demographics"),
        selectInput(inputId = "barvar2",
                    label = "Choose a degree level to plot:",
                    choices = degree_choices, 
                    selected = "Less than high school completion")
      ),
      
      mainPanel(plotOutput(outputId = "bar"))
    )
  )
  
)

############
# server   #
############
server <- function(input, output){
  
  # TAB 1: BAR
  output$line <- renderPlot({
    data <- merged_earnings
    ggplot(data, mapping = aes(x = Year, y = Earnings)) +
      geom_line(color = "#2c7fb8", fill = "#7fcdbb", alpha = 0.7)+
      labs(x = "Year", y = "Earnings") +
      scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) #set origin at (0,0)
  })
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)