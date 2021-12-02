# Load necessary packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggrepel)
library(robotstxt)
library(datetime)

#############################################################
# Define choice values and labels for widgets (user inputs) #
# - Define vectors for choice values and labels             #
# - Can then refer to them in server                        #
#############################################################

# For TAB 1 and TAB 2 TIME SERIES widgets:

## For checkboxGroupInput
demographic_choices <- unique(merged_grad_rates2$Demographic)

## For selectizeInput choices 
years_completed_choices <- unique(merged_grad_rates2$years_completed)

# For TAB 3 CLUSTER widgets:

## For checkboxGroupInput
cluster_choices <- c(1,2,3,4,5)

############
#    ui    #
############
ui <- navbarPage(
    
    # Application title
    titlePanel("Graduation Rate and Earning Outcomes"),
    
    # Tab 1: Graduation Rate Time Series
    tabPanel(
        title = "Graduation Rates Time Series",
        
        sidebarLayout(
            sidebarPanel(
                
                selectInput(inputId = "gradlinevar1",
                            label = "How many years it took to graduate:",
                            choices = years_completed_choices,
                            selected = "4 years"),
                
                checkboxGroupInput(inputId = "gradlinevar2",
                                   label = "Demographic:",
                                   choices = demographic_choices,
                                   selected = demographic_choices,
                                   inline = TRUE)
            ),
            
            mainPanel(plotOutput(outputId = "gradrateline"))
        )
    ),
    
    # Tab 2: Earnings Time Series
    tabPanel(
        title = "Earnings Time Series",
        
        sidebarLayout(
            sidebarPanel(
                
                checkboxGroupInput(inputId = "earningsvar",
                                   label = "Demographic:",
                                   choices = demographic_choices,
                                   selected = demographic_choices,
                                   inline = TRUE)
            ),
            
            mainPanel(plotOutput(outputId = "earningsline"))
        )
    ),
    
    # Tab 3: Clustering
    tabPanel(
        title = "Clustering",
        
        sidebarLayout(
            sidebarPanel(
                
                #Choose year range
                sliderInput("year",
                            "Choose a year range:",
                            min = as.year(1996),
                            max = as.year(2013),
                            value = c(1996, 2013)),
                
                #Choose number of clusters
                selectInput(inputId = "clustervar",
                            label = "Choose number of clusters:",
                            choices = cluster_choices,
                            selected = 3),
                
                #Choose demographic
                checkboxGroupInput(inputId = "clustdemovar",
                                   label = "Demographic:",
                                   choices = demographic_choices,
                                   selected = demographic_choices,
                                   inline = TRUE)
            ),
            
            mainPanel(plotOutput(outputId = "clust"))
        )
    )
)

############
# server   #
############
server <- function(input, output){
    
    # TAB 1: Graduation Rate Time Series
    data_for_gradline <- reactive({
       data <- filter(merged_grad_rates2, years_completed == input$gradlinevar1, Demographic %in% input$gradlinevar2)
       data
    })
    
    output$gradrateline <- renderPlot({
        data <- data_for_gradline()
        ggplot(data = data, aes(x = year_num, y = grad_rates_num, color = Demographic)) +
            geom_line()+
            labs(x = "Year", y = "Graduation Rate") +
            lims(x = c(1995, 2015), y= c(0,80)) 
    })
    
    # TAB 2: Earnings Time Series
    data_for_earningsline <- reactive({
       data <- filter(earnings2, Demographic %in% input$earningsvar)
       data
    })
    
    output$earningsline <- renderPlot({
        data <- data_for_earningsline()
        ggplot(data = data, aes(x = year_num, y = Earnings, color = Demographic)) +
            geom_line()+
            labs(x = "Year", y = "Graduation Rate") +
            scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) #set origin at (0,0)
    })
    
    # TAB 3: Clustering
    data_for_cluster <- reactive({
        
        #a bit of tidying
        grad_rates_earnings1 <- grad_rates_earnings %>%
            group_by(Demographic, Year, Earnings) %>%
            summarize(Grad_Rates = mean(Grad_Rates)) %>%
            ungroup() %>%
            drop_na(Grad_Rates, Earnings)
        
        #kmeans clustering
        set.seed(2)
        clustering_vars <- c("Grad_Rates", "Earnings")
        grad_earnings_km2 <- grad_rates_earnings1 %>% 
            select(clustering_vars) %>% 
            kmeans(centers = input$clustervar)
        
        # Vector of cluster assignments
        grad_earnings_km2$cluster
        
        # The centroids for the fit
        grad_earnings_km2$centers
        
        # Add cluster assignment to the data frame
        grad_earnings_cluster <- grad_rates_earnings1 %>%
            mutate(clusters2 = factor(grad_earnings_km2$cluster)) 
        
        #demographic choices
        data <- filter(grad_earnings_cluster, Demographic %in% input$clustdemovar)
        
        #year slider
        data <- filter(grad_earnings_cluster, Year %in% (input$year[1]:input$year[2]))
        data
    })
        
        output$clust <- renderPlot({
            data <- data_for_cluster()
            # Visualize the cluster assignments and centroids
            ggplot(data = data, aes(x = Earnings, y = Grad_Rates)) +
                geom_point(aes(color = clusters2)) +
                geom_text_repel(aes(label = Demographic, color = clusters2), size = 3) +
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
