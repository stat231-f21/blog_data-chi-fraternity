# Load necessary packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(sf)
library(leaflet)
library(rnaturalearth)

## Data sets
areas_of_study <- read.csv("map_data/areas_of_study.csv")
admission_enrollment_data <- read.csv("map_data/admission_enrollment_data.csv")
institution_types <- read.csv("map_data/institution_types.csv")
student_demographics <- read.csv("map_data/student_demographics.csv")
student_aid <- read.csv("map_data/student_aid.csv")
cluster_table <- read.csv("map_data/cluster_table.csv")

## state geometries
states_sf_rne <- ne_states(country = "united states of america", returnclass = "sf") %>%
  select(name, geometry) %>%
  rename(state = name)

## Plot choices
plot_choice_values <- c("student_aid", "institutions", "majors", "demographics", "admissions", "enrollments")
plot_choice_names <- c("Student Aid Awarded", "Types of Institutions", "Areas of Study",
                       "Student Demographics","Admission Rates Over Time", "Enrollment Rates Over Time")
names(plot_choice_values) <- plot_choice_names

## Clustering Choices
names <- colnames(cluster_table)
cluster_choice_values <- names[! names %in% c("state", "X", "total")]
#cluster_choice_names <- c()
#names(cluster_choice_values) <- cluster_choice_names

######
# UI #
######

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "plot",
                  label = "Choose the data you are interested in:",
                  choices = plot_choice_values,
                  selected = "Student Aid Awarded"),
      
      selectizeInput(inputId = "cluster_var",
                     label = "Choose the variable(s) to cluster states by",
                     choices = cluster_choice_values,
                     multiple = TRUE,
                     options = list(maxItems = 2)),
      
      sliderInput(inputId = "num_clusters",
                  label = "Choose the number of state clusters",
                  min = 1, max = 10, value = 3),
    ), 
    
    mainPanel(
      leafletOutput("map"),
      
      plotOutput("vis"),
      
      plotOutput("cluster_plot"),
      
      plotOutput("elbow_plot")
    )
  )
)

##########
# Server #
##########

server <- function(input, output) {
  ## Plot generation
  getPlot <- function(state_name, plot_type) {
    if (plot_type == "student_aid") {
      plot <- ggplot(data = student_aid %>% filter(state == state_name), aes(x = aid_type, y = amount)) +
        geom_bar(stat = "identity", fill = "darkgreen") + 
        scale_x_discrete(limits = c("Total Aid", "Need-Based Grants", "Non-Need-Based Grants", "Non-Grant Aid")) + 
        scale_y_continuous(
          name = "Amount ($)",
          labels = scales::comma
        ) + 
        labs(
          title = "Distribution of State Spending on Student Aid",
          subtitle = paste("by the state of", state_name, "in 2018-19"),
          x = "Type of Aid"
        ) + 
        geom_text(aes(label = format(amount, big.mark = ",", scientific = FALSE)), vjust = -0.5)
      
    } else if (plot_type == "institutions") {
      plot <- ggplot(data = institution_types %>% filter(state == state_name), aes(x = institution_type, y = count)) + 
        geom_bar(stat = "identity", fill = "darkorchid4") + 
        labs(
          title = "Distribution of Different Types of Institutions of Higher Education",
          subtitle = paste("in", state_name, "in 2019"),
          x = "Type of Institution"
        ) + 
        geom_text(aes(label = count), vjust = -0.3)
      
    } else if (plot_type == "majors") {
      plot <- ggplot(data = areas_of_study %>% filter(state == state_name), aes(x = major, y = count)) + 
        geom_bar(stat = "identity", fill = "darkblue") + 
        scale_x_discrete(limits = c("Humanities", "Psychology", "Social Sciences\n& History", "Natural Sciences\n& Mathematics", "Computer Science", "Engineering", "Education", "Business", "Healthcare", "Other", "Total")) + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
        labs(
          title = "Distribution of Areas of Study of Bachelor's Degrees Granted",
          subtitle = paste("in the state of", state_name, ", 2018-19"),
          x = "Area of Study"
        ) + 
        geom_text(aes(label = format(count, big.mark = ",", scientific = FALSE)), vjust = -0.3)  
      
    } else if (plot_type == "demographics") {
      plot <- ggplot(data = student_demographics %>% filter(state == state_name), aes(x = group, y = percent)) + 
        geom_bar(stat = "identity", fill = "darkred") + 
        scale_x_discrete(
          name = NULL,
          limits = c("Women", "Men", "Native\nAmerican", "Asian", "Black", "Hispanic", "Pacific\nIslander", "White", "2 or more\nRaces")
        ) + 
        labs(
          title = paste("Demographic Distributions of Enrolled College Students in", state_name, "in Fall 2019"),
          y = "% of All Students"
        ) + 
        geom_text(aes(label = paste0(percent, "%")), vjust = -0.3)
      
      #} else if (plot_type == "stud_fac_staff") {
      
      
    } else if (plot_type == "admissions") {
      plot <- ggplot(data = admission_enrollment_data %>% filter(state == state_name), aes(x = year, y = admission_rate)) + 
        geom_line() + 
        geom_point() + 
        labs(
          title = paste("Statewide Fall Undergraduate Admission Rates in", state_name, "from 2014-2020"),
          x = "Year",
          y = "Admission Rate"
        ) + 
        geom_text(aes(label = paste0(admission_rate, "%")), vjust = -0.5, color = "blue")
      
    } else { #enrollment
      plot <- ggplot(data = admission_enrollment_data %>% filter(state == state_name), aes(x = year, y = enrollment_rate)) + 
        geom_line() + 
        geom_point() + 
        labs(
          title = paste("Statewide Fall Undergraduate Enrollment Rates in", state_name, "from 2014-2020"),
          x = "Year",
          y = "Enrollment Rate"
        ) + 
        geom_text(aes(label = paste0(enrollment_rate, "%")), vjust = -0.5, color = "blue")
      
    }
    
    return(plot)
  }
  
  # getColor <- function(num) {
  #   return(ifelse(num==1, "red",
  #                 ifelse(num==2, "blue", "green")))
  # }
  
  # center_on <- states_sf_rne %>%
  #   filter(state == "Alaska") %>%
  #   pull(geometry) %>%
  #   st_coordinates()
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles()
  })
  
  observeEvent(
    {
      input$cluster_var
      input$num_clusters
    }, 
    {
      proxy <- leafletProxy("map")
      
      if (!is.null(input$cluster_var)) {
        
        if (length(input$cluster_var) == 2) {
          # Clustering
          set.seed(3)
          
          cluster_table_2 <- cluster_table %>%
            drop_na(input$cluster_var[1], input$cluster_var[2])
          
          table_k <- cluster_table_2 %>%
            select(input$cluster_var) %>%
            kmeans(centers = input$num_clusters, nstart = 20)
          
          cluster_table_2 <- cluster_table_2 %>%
            mutate(clusters = factor(table_k$cluster))
          
          states_sf_and_clusters <- states_sf_rne %>%
            left_join(cluster_table_2, by = "state")
          
          pal <- colorFactor(topo.colors(input$num_clusters), cluster_table_2$clusters)
          
          # Display clusters in map
          proxy %>%
            clearShapes() %>%
            clearControls() %>%
            addPolygons(
              data = states_sf_and_clusters,
              color = ~pal(clusters),
              stroke = FALSE,
              fillOpacity = 0.5,
              layerId = ~state
            ) #%>%
          #addLegend(values = ~pal(clusters), opacity = 0.5, position = "bottomleft")
          
          # Display scatterplot of clusters
          output$cluster_plot <- renderPlot({
            ggplot(data = cluster_table_2, aes_string(x = input$cluster_var[2], y = input$cluster_var[1])) + 
              geom_point(aes(color = clusters)) + 
              labs(color = "Cluster assignment")
          })
          
          # Elbowplot
          elbow_plot <- data.frame(elbow_clusters = 1:10,
                                   within_ss = rep(NA, 10))
          
          for(i in 1:10) {
            table_kmi_output <- cluster_table_2 %>%
              select(input$cluster_var) %>%
              kmeans(centers = i, nstart = 20)
            
            elbow_plot$within_ss[i] <- table_kmi_output$tot.withinss
          }
          
          output$elbow_plot <- renderPlot({
            ggplot(elbow_plot, aes(x = elbow_clusters, y = within_ss)) +
              geom_point() + 
              geom_line() +
              scale_x_continuous(breaks = 1:10) +
              labs(x = "Number of clusters (k)", y = expression("Total W"[k]))
          })
          
        } else if (length(input$cluster_var) == 1) {
          
          table_map <- states_sf_rne %>%
            left_join(cluster_table, by = "state") %>%
            mutate(selected_ratio = .data[[input$cluster_var[1]]]/max( .data[[input$cluster_var[1]]], na.rm = TRUE))
          
          mybins <- c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1)
          mypalette <- colorBin(palette = "magma", domain = table_map$selected_ratio, na.color="transparent", bins = mybins)
          
          proxy %>%
            clearShapes() %>%
            clearControls() %>%
            addPolygons(
              data = table_map,
              #fillColor = ~mypalette(.data[[input$cluster_var[1]]]), 
              fillColor = ~mypalette(selected_ratio),
              stroke=FALSE, 
              fillOpacity = .5,
              layerId = ~state
            ) %>%
            #addLegend(pal = mypalette, values = table_map[[input$cluster_var[1]]], opacity=0.9, position = "bottomleft")
            addLegend(pal = mypalette, values = table_map$selected_ratio, opacity=0.9, position = "bottomleft")
          
          output$cluster_plot <- renderPlot({
            ggplot(table_map, aes_string(x = input$cluster_var[1])) + 
              geom_histogram(bins = 10)
          })
          
          # Hide Elbow Plot output
          output$elbow_plot <- renderPlot({
            ggplot()
          })
          
        }
        
      } else {
        # Map without clustering
        proxy %>%
          clearShapes() %>%
          clearControls() %>%
          addPolygons(
            data = states_sf_rne,
            layerId = ~state
          )
        
        # Hide Cluster Plot output
        output$cluster_plot <- renderPlot({
          ggplot()
        })
        
        # Hide Elbow Plot output
        output$elbow_plot <- renderPlot({
          ggplot()
        })
      }
    }
  )
  
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    output$vis <- renderPlot(getPlot(state_name = click$id, plot_type = input$plot))
  })
  
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)
