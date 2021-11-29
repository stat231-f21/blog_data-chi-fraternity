# Load necessary packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(sf)
library(leaflet)
library(leafpop)
library(rnaturalearth)
library(sp)

## Data sets
areas_of_study <- read.csv("data/undergrad_degree_majors_per_state.csv") %>%
  select(-X) %>%
  rename(education = edutation) %>%
  mutate(
    total = as.numeric(gsub(",", "", total)),
    humanities = as.numeric(gsub(",", "", humanities)),
    psychology = as.numeric(gsub(",", "", psychology)),
    social_sci_hist = as.numeric(gsub(",", "", social_sci_hist)),
    natural_sci_math = as.numeric(gsub(",", "", natural_sci_math)),
    computer_sci = as.numeric(gsub(",", "", computer_sci)),
    engineering = as.numeric(gsub(",", "", engineering)),
    education = as.numeric(gsub(",", "", education)),
    business = as.numeric(gsub(",", "", business)),
    healthcare = as.numeric(gsub(",", "", healthcare)),
    other = as.numeric(gsub(",", "", other))
  ) %>%
  rename(
    "Total" = total,
    "Humanities" = humanities,
    "Psychology" = psychology,
    "Social Sciences\n& History" = social_sci_hist,
    "Natural Sciences\n& Mathematics" = natural_sci_math,
    "Computer Science" = computer_sci,
    "Engineering" = engineering,
    "Education" = education,
    "Business" = business,
    "Healthcare" = healthcare,
    "Other" = other
  ) %>%
  pivot_longer(-state, names_to = "major", values_to = "count")

admission_enrollment_data <- read.csv("data/admission_enrollment_data.csv") %>%
  select(state, year, admission_rate, enrollment_rate) %>%
  mutate(enrollment_rate = round(enrollment_rate, 1))

institution_types <- read.csv("data/institutions_per_state.csv") %>%
  select(-X) %>% 
  mutate(private_np_2yr = as.numeric(private_np_2yr)) %>%
  rename("Total" = total,
         "Public 4-Year" = public_4year,
         "Public 2-Year" = public_2year,
         "Private Nonprofit 4-Year" = private_np_4yr,
         "Private Nonprofit 2-Year" = private_np_2yr,
         "Private For-Profit 4-Year" = private_fp_4yr,
         "Private For-Profit 2-Year" = private_fp_2yr) %>%
  pivot_longer(-state, names_to = "institution_type", values_to = "count")

student_demographics <- read.csv("data/student_demographics_per_state.csv") %>%
  mutate(
    "Women" = as.numeric(str_extract(women, ".+(?=%)")),
    "Men" = 100-Women,
    "Native\nAmerican" = as.numeric(str_extract(native_american, ".+(?=%)")),
    "Asian" = as.numeric(str_extract(asian, ".+(?=%)")),
    "Black" = as.numeric(str_extract(black, ".+(?=%)")),
    "Hispanic" = as.numeric(str_extract(hispanic, ".+(?=%)")),
    "Pacific\nIslander" = as.numeric(str_extract(pacific_islander, ".+(?=%)")),
    "White" = as.numeric(str_extract(white, ".+(?=%)")),
    "2 or more\nRaces" = as.numeric(str_extract(X2._races, ".+(?=%)"))
  ) %>%
  select(-c(X, minority, nonresident, total, women, native_american, asian, black, hispanic, pacific_islander, white, X2._races)) %>%
  pivot_longer(-state, names_to = "group", values_to = "percent")

student_aid <- read.csv("data/finance_table.csv") %>%
  select(state, need_based_grants, non_need_based_grants, non_grant_aid, total_aid) %>% 
  rename("Need-Based Grants" = need_based_grants,
         "Non-Need-Based Grants" = non_need_based_grants, 
         "Non-Grant Aid" = non_grant_aid, 
         "Total Aid" = total_aid) %>%
  pivot_longer(-state, names_to = "aid_type", values_to = "amount")

## state geometries
states_sf_rne <- ne_states(country = "united states of america", returnclass = "sf") %>%
  select(name, geometry)

plot_choice_values <- c("student_aid", "institutions", "majors", "demographics", "admissions", "enrollments")
plot_choice_names <- c("Student Aid Awarded", "Types of Institutions", "Areas of Study",
                       "Student Demographics","Admission Rates Over Time", "Enrollment Rates Over Time"
)

names(plot_choice_values) <- plot_choice_names

ui <- fluidPage(
  selectInput(inputId = "plot",
              label = "Choose the data you are interested in:",
              choices = plot_choice_values,
              selected = "Student Aid Awarded"),
  leafletOutput("map"),
  
  plotOutput("vis")
)

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
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(
        data = states_sf_rne,
        layerId = ~name
    )
  })
  
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    output$vis <- renderPlot(getPlot(state_name = click$id, plot_type = input$plot))
  })
  
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)
