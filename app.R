#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(sas7bdat)

# Global Options

options(stringsAsFactors = FALSE)

# Load Data 

load(file = "clinical_data.RData")

# Gender Map 

gender_options = c("Male", "Female")
gender_options_mapped = c("M", "F")

# Therapy Map 

therapy_options = c("Placebo", "Drug")
therapy_options_mapped = c("PLACEBO", "DRUG")

# Simulations

simulations = rnorm(5e5) #makd note

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Design 
    
    theme = shinytheme("flatly"),
    
    # Application title
    titlePanel("Clinical Data App"),
    
    tabsetPanel(
        tabPanel("Welcome",
                 h3("Clinical Data Summary"),
                 p("This data is from an antidepressant clinical trial with four treatments; two doses of an experimental medication, a positive control, and placebo."),
                 br(),
                 p("Hamilton 17-item rating scale for depression (HAMD17) was observed at baseline and weeks 1, 2, 4, 6, and 8."),
                 br(),
                 p("Two arms were created; the original placebo arm and a drug arm created by randomly selecting patients from the three non-placebo arms."),
                 uiOutput("tab")),
        tabPanel("Raw Data",
                 DT::dataTableOutput("table_output")),
        tabPanel("Plots",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(inputId = "gender_input", 
                                     label = "Gender", 
                                     choices = c("Male", "Female"), 
                                     selected = "Male", 
                                     multiple = TRUE),
                         selectInput(inputId = "therapy_input", 
                                     label = "Therapy", 
                                     choices = c("Placebo", "Drug"), 
                                     selected = "Placebo", 
                                     multiple = TRUE), 
                         sliderInput(inputId = "basval_range_input", 
                                     label = "Base Value Select", 
                                     min = min(clinical_data$basval),
                                     max = max(clinical_data$basval),
                                     value = c(min(clinical_data$basval), max(clinical_data$basval)))
                     ),
                     
                     mainPanel(plotOutput("scatter_plot"))
                             
                         )
                         
                     ),
        tabPanel("Statistical Test",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(inputId = "gender_input_statistical_test", 
                                     label = "Gender", 
                                     choices = c("Male", "Female"), 
                                     selected = "Male", 
                                     multiple = TRUE),
                         sliderInput(inputId = "p_value_input", 
                                     label = "P Value Threshold", 
                                     min = 0,
                                     max = 0.2,
                                     value = 0.05)
                     ),
                     mainPanel(plotOutput("statistical_test_plot"))
                 )
            )
    )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    url <- a("London School of Hygiene and Tropical Medicine", href="https://missingdata.lshtm.ac.uk/2017/04/28/example-data-set-from-an-antidepressant-clinical-trial/")
    output$tab <- renderUI({
        tagList("URL link:", url)
    })

    
    output$scatter_plot = renderPlot({
        
        clinical_data_filtered = clinical_data %>% 
            filter(GENDER %in% gender_options_mapped[match(input$gender_input, gender_options)], 
                   THERAPY %in% therapy_options_mapped[match(input$therapy_input, therapy_options)],
                   basval >= input$basval_range_input[1] & basval <= input$basval_range_input[2])
        
            
        data_select = clinical_data_filtered %>%
                      group_by(PATIENT) %>%
                      filter(RELDAYS == max(RELDAYS)) %>%
                      ungroup()
        
        plot_1 = ggplot(data_select, aes(x = basval, y = (change/basval)/RELDAYS, color = GENDER, shape = THERAPY)) + #assume drug acts linearly throughout time
            geom_point(size = 5.0) +
            geom_hline(yintercept = 0, linetype = "dashed", color = "blue2") +
            scale_x_continuous(labels = scales::comma, 
                               breaks = scales::pretty_breaks(n = 10)) +
            scale_y_continuous(labels = scales::percent, 
                               breaks = scales::pretty_breaks(n = 10)) +
            ggtitle("Average Daily Difference as a % of Base Value") +
            xlab("Base Value") + 
            ylab("Percentage Changed from Base Value") +
            theme_minimal()
        
        return(plot_1)
    })
    
    output$table_output = DT::renderDataTable({
        
        data_selected = clinical_data %>%
                        select(Patient = PATIENT,
                               Days = RELDAYS,
                               Visit = VISIT,
                               Therapy = THERAPY,
                               Gender = GENDER,
                               Pool = POOLINV,
                               `Base Value` = basval,
                               HAMDTL17,
                               Change = change)
        
        DT::datatable(data_selected)
    }) 
    
    
    output$statistical_test_plot = renderPlot ({
        
        data_for_test = clinical_data %>% 
                        group_by(PATIENT) %>%
                        filter(RELDAYS == max(RELDAYS)) %>%
                        ungroup() %>%
                        filter(GENDER %in% gender_options_mapped[match(input$gender_input_statistical_test, gender_options)])
        
        N_drug = nrow(data_for_test%>%filter(THERAPY == "DRUG"))
        N_placebo = nrow(data_for_test%>%filter(THERAPY == "PLACEBO"))
        
        mu_drug = mean((data_for_test%>%filter(THERAPY == "DRUG"))$change/(data_for_test%>%filter(THERAPY == "DRUG"))$basval)
        mu_placebo = mean((data_for_test%>%filter(THERAPY == "PLACEBO"))$change/(data_for_test%>%filter(THERAPY == "PLACEBO"))$basval)
        
        sd_drug =  sd((data_for_test%>%filter(THERAPY == "DRUG"))$change/(data_for_test%>%filter(THERAPY == "DRUG"))$basval)
        sd_placebo = sd((data_for_test%>%filter(THERAPY == "PLACEBO"))$change/(data_for_test%>%filter(THERAPY == "PLACEBO"))$basval)
        
        mu_difference = 0
        sd_difference = sqrt((sd_drug^2)/N_drug + (sd_placebo^2)/N_placebo)
        
        actual_difference = mu_drug - mu_placebo
        
        p_value = pnorm(actual_difference, mu_difference, sd_difference)
        
        final_plot = ggplot(data.frame(simulation = simulations*sd_difference + mu_difference), aes(x = simulation)) + #hdow many sd away from mean
                     geom_density(fill = "aquamarine1") + 
                     geom_vline(xintercept = qnorm(input$p_value_input, mu_difference, sd_difference), linetype = "dashed", color = "blue2", size = 1.5) + 
                     geom_vline(xintercept = actual_difference, linetype = "dashed", color = "red2", size = 1.5) +
                     scale_x_continuous(labels = scales::comma, 
                                        breaks = scales::pretty_breaks(n = 10)) +
                     scale_y_continuous(labels = scales::percent, 
                                        breaks = scales::pretty_breaks(n = 10)) +
                     ggtitle(paste("The P Value is ", paste(round(100*p_value, 3), "%", sep = ""))) +
                     xlab("Difference Under Null Hypothesis") + 
                     ylab("") +
                     theme_minimal()
                     
        return(final_plot)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

