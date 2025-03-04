# Load ICU cohort data
mimic_icu_cohort <- readRDS("~/203b-hw/hw4/mimiciv_shiny/mimic_icu_cohort.rds")

# Define variable groups
variable_groups <- list(
  "Demographics" = c("age_intime", "gender", "race", "insurance", 
                     "marital_status"),
  "Lab Measurements" = c("Creatinine", "Sodium", "Potassium", "Glucose", 
                         "Hematocrit", "White_Blood_Cells", "Bicarbonate"),
  "Vitals" = c("heart_rate", "systolic_non_invasive_blood_pressure", 
               "diastolic_non_invasive_blood_pressure", "respiratory_rate", 
               "temperature_fahrenheit")
)

# Define UI
ui <- fluidPage(
  titlePanel("ICU Cohort Data"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("var_group", "Variable Group:", 
                  choices = names(variable_groups),
                  selected = "Demographics"),
      
      selectInput("var_select", "Variable:", 
                  choices = variable_groups[[1]]), # Default group
      
      uiOutput("slider_ui")  # Dynamic slider for age & lab measurements
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", 
                 plotOutput("graph_plot"),  # Show graph at the top
                 DTOutput("num_summary"),   # Show numerical summary below
                 h3("Missing Values Summary"),  # Section Title
                 verbatimTextOutput("na_summary") # Show NA counts
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  observeEvent(input$var_group, {
    updateSelectInput(session, "var_select",
                      choices = variable_groups[[input$var_group]],
                      selected = variable_groups[[input$var_group]][1])
  })
  
  # Render slider input for `age_intime` and lab measurements
  output$slider_ui <- renderUI({
    if (input$var_select %in% c("age_intime", 
                                variable_groups[["Lab Measurements"]])) {
      sliderInput("value_range", paste("Select Range for", 
                                       input$var_select, ":"),
                  min = min(mimic_icu_cohort[[input$var_select]], na.rm = TRUE),
                  max = max(mimic_icu_cohort[[input$var_select]], na.rm = TRUE),
                  value = c(min(mimic_icu_cohort[[input$var_select]], 
                                na.rm = TRUE), 
                            max(mimic_icu_cohort[[input$var_select]], 
                                na.rm = TRUE)))
    }
  })
  
  # Generate graphical summaries
  output$graph_plot <- renderPlot({
    req(input$var_select)  # Ensure input is selected
    
    data_selected <- mimic_icu_cohort[[input$var_select]]
    
    # Apply filter if slider is active (for age_intime and lab measurements)
    if (input$var_select %in% c("age_intime", 
                                variable_groups[["Lab Measurements"]]) && !
        is.null(input$value_range)) {
      mimic_filtered <- mimic_icu_cohort %>%
        filter(!!sym(input$var_select) >= input$value_range[1] & 
                 !!sym(input$var_select) <= input$value_range[2])
    } else {
      mimic_filtered <- mimic_icu_cohort
    }
    
    if (is.numeric(data_selected)) {
      ggplot(mimic_filtered, aes(x = !!sym(input$var_select))) +
        geom_histogram(bins = 30, fill = "skyblue", alpha = 0.7) +
        labs(title = paste("Distribution of", input$var_select),
             x = input$var_select, y = "Count") +
        theme_minimal()
    } else {
      ggplot(mimic_filtered, aes(x = !!sym(input$var_select), 
                                 fill = !!sym(input$var_select))) +
        geom_bar() +
        labs(title = paste("Bar Plot of", input$var_select),
             x = input$var_select, y = "Count") +
        theme_minimal() +
        coord_flip()
    }
  })
  
  # Generate numerical summaries as a DataTable
  output$num_summary <- renderDT({
    req(input$var_select)
    
    if (is.numeric(mimic_icu_cohort[[input$var_select]])) {
      # Convert summary output into a clean data frame without row names
      summary_df <- data.frame(
        Statistic = names(summary(mimic_icu_cohort[[input$var_select]])),
        Value = as.vector(summary(mimic_icu_cohort[[input$var_select]]))
      )
    } else {
      # Generate frequency count for categorical variables
      summary_df <- mimic_icu_cohort %>%
        count(!!sym(input$var_select)) %>%
        arrange(desc(n))
      colnames(summary_df) <- c("Category", "Count")  
    }
    
    datatable(summary_df, options = list(pageLength = 10, scrollX = TRUE, 
                                         searching = FALSE))
  })
  
  # Display Missing Values Summary
  output$na_summary <- renderPrint({
    req(input$var_select)
    
    na_count <- sum(is.na(mimic_icu_cohort[[input$var_select]]))
    total_values <- nrow(mimic_icu_cohort)
    missing_percentage <- (na_count / total_values) * 100
    
    paste0("Missing values (NAs): ", na_count, " out of ", total_values,
           " (", round(missing_percentage, 2), "%)")
  })
}

# Run the application
shinyApp(ui = ui, server = server)