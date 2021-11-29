# Load necessary packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggrepel)
library(robotstxt)
library(datetime)

#Import data
grad_earnings <- read_csv("Excel Wrangling Final.csv")

#############################################################
# Define choice values and labels for widgets (user inputs) #
# - Define vectors for choice values and labels             #
# - Can then refer to them in server                        #
#############################################################
## For checkboxGroupInput
demographic_choices <-  unique(grad_earnings_new$demographic)
cluster_choices <- c(1,2,3,4,5)

############
#    ui    #
############
ui <- fluidPage(

    # Application title
    titlePanel("Graduation Rate and Earning Outcomes"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            #Choose year range
            sliderInput("years",
                        "Choose a year range:",
                        min = as.year(1995),
                        max = as.year(2013),
                        value = c(1995, 2013)),
            
            #Choose number of clusters
            selectInput(inputId = "clustervar",
                        label = "Choose number of clusters:",
                        choices = cluster_choices,
                        selected = 3),
            
            #Choose demograhics
            checkboxGroupInput(inputId = "typ",
                               label = "Include demographics:",
                               choices = demographic_choices,
                               selected = "Male",
                               inline = TRUE)
        ),

        # Show a plot of the generated distribution
        mainPanel(plotOutput(outputId = "scatter"))
        )
    )


############
# server   #
############

server <- function(input, output) {
    
    data_for_scatter <- reactive({
        data <- filter(grad_earnings_cluster, demographic %in% input$typ)
    })

    output$scatter <- renderPlot({
        data <- data_for_scatter()
        #a bit of tidying
        grad_earnings_new <- grad_earnings %>%
            rename(graduation_rate = `Graduation Rate`)%>%
            rename(demographic = Demographic) %>%
            rename(earnings = Earnings) %>%
            slice(-(46:66)) %>%
            select(-c(5, 6)) 
        
        grad_earnings_new <- grad_earnings_new %>%
            mutate(earnings_num = as.numeric(earnings),
                   grad_rate_num = as.numeric(graduation_rate))
        
        grad_earnings_new <- grad_earnings_new %>%
            drop_na(earnings_num, grad_rate_num) %>%
            select(-c(earnings, graduation_rate))
        
        #kmeans clustering
        set.seed(2)
        clustering_vars <- c("earnings_num", "grad_rate_num")
        grad_earnings_km2 <- grad_earnings_new %>% 
            select(clustering_vars) %>% 
            kmeans(centers = input$clustervar)
        
        # Vector of cluster assignments
        grad_earnings_km2$cluster
        
        # The centroids for the fit
        grad_earnings_km2$centers
        
        # Add cluster assignment to the data frame
        grad_earnings_cluster <- grad_earnings_new %>%
            mutate(clusters2 = factor(grad_earnings_km2$cluster)) 
        
        # Visualize the cluster assignments and centroids
        ggplot(data = data, aes(x = earnings_num, y = grad_rate_num)) +
            geom_point(aes(color = clusters2)) +
            geom_text_repel(aes(label = demographic, color = clusters2), size = 3) +
            labs(x = "Earnings", 
                 y = "Graduation Rates") +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
            facet_wrap(~ Year)
    })
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)
