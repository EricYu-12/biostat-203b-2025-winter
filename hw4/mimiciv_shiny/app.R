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
  tabsetPanel(id = "tabselected",
              tabPanel("Summary", fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    selectInput("var_group", "Variable Group:", 
                                choices = names(variable_groups),
                                selected = "Demographics"),
                    
                    selectInput("var_select", "Variable:", 
                                choices = variable_groups[[1]]), 
                    
                    uiOutput("slider_ui")  
                  ),
                  mainPanel(
                    plotOutput("graph_plot"),  
                    DTOutput("num_summary"),   
                    h3("Missing Values Summary"),  
                    verbatimTextOutput("na_summary") 
                  )
                )
              )),
              
              tabPanel("Patient Info", fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    textInput("subject_id", "Subject ID", ""),
                    actionButton("submit", "Submit"),
                    selectInput("plot_type", "Select a plot:", 
                                choices = c("ADT", "ICU"))
                  ),
                  mainPanel(
                    plotOutput("patient_plot")
                  )
                )
              ))
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Render slider input for `age_intime` and lab measurements
  output$slider_ui <- renderUI({
    if (input$var_select %in% c("age_intime", 
                                variable_groups[["Lab Measurements"]])) {
      sliderInput("value_range", 
                  paste("Select Range for", input$var_select, ":"),
                  min = min(mimic_icu_cohort[[input$var_select]], na.rm = TRUE),
                  max = max(mimic_icu_cohort[[input$var_select]], na.rm = TRUE),
                  value = c(min(mimic_icu_cohort[[input$var_select]], 
                                na.rm = TRUE), 
                            max(mimic_icu_cohort[[input$var_select]], 
                                na.rm = TRUE)))
    }
  })
  
  # Reactive data filtered by slider input
  filtered_data <- reactive({
    req(input$var_select)  # Ensure input exists
    
    if (is.numeric(mimic_icu_cohort[[input$var_select]])) {
      mimic_icu_cohort %>%
        filter(!!sym(input$var_select) >= input$value_range[1] & 
                 !!sym(input$var_select) <= input$value_range[2])
    } else {
      mimic_icu_cohort  
    }
  })
  
  # Generate graphical summaries
  output$graph_plot <- renderPlot({
    req(input$var_select)
    
    data <- filtered_data()
    
    if (is.numeric(data[[input$var_select]])) {
      ggplot(data, aes(x = !!sym(input$var_select))) +
        geom_histogram(bins = 30, fill = "skyblue", alpha = 0.7) +
        labs(title = paste("Distribution of", input$var_select),
             x = input$var_select, y = "Count") +
        theme_minimal()
    } else {
      ggplot(data, aes(x = !!sym(input$var_select), 
                       fill = !!sym(input$var_select))) +
        geom_bar() +
        labs(title = paste("Bar Plot of", input$var_select),
             x = input$var_select, y = "Count") +
        theme_minimal() +
        coord_flip()
    }
  })
  
  # Generate numerical summaries as a Data Table
  output$num_summary <- renderDT({
    req(input$var_select)
    
    data <- filtered_data()
    
    if (is.numeric(data[[input$var_select]])) {
      summary_df <- data.frame(
        Statistic = names(summary(data[[input$var_select]])),
        Value = as.vector(summary(data[[input$var_select]]))
      )
    } else {
      summary_df <- data %>%
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
    
    data <- filtered_data()
    
    na_count <- sum(is.na(data[[input$var_select]]))
    total_values <- nrow(data)
    missing_percentage <- (na_count / total_values) * 100
    
    paste0("Missing values (NAs): ", na_count, " out of ", 
           total_values, " (", round(missing_percentage, 2), "%)")
  })
  
  observeEvent(input$submit, {
    req(input$subject_id)
    subject_id <- as.numeric(input$subject_id)
    
    output$error_message <- renderText({ "" })  
    
    # Connect to BigQuery
  satoken <- "biostat-203b-2025-winter-4e58ec6e5579.json"
  bq_auth(path = satoken)
  
  con_bq <- dbConnect(
    bigrquery::bigquery(),
    project = "biostat-203b-2025-winter",
    dataset = "mimiciv_3_1",
    billing = "biostat-203b-2025-winter"
  )
    
    # Load Data from BigQuery dynamically
    demographics_data <- tbl(con_bq, "patients") %>%
      filter(subject_id == !!subject_id) %>%
      select(subject_id, gender, anchor_age) %>%
      left_join(tbl(con_bq, "admissions") %>%
                  filter(subject_id == !!subject_id) %>%
                  select(subject_id, race), by = "subject_id") %>%
      distinct(subject_id, .keep_all = TRUE) %>%
      collect()
    
    diagnoses_data <- tbl(con_bq, "diagnoses_icd") %>%
      filter(subject_id == !!subject_id) %>%
      left_join(tbl(con_bq, "d_icd_diagnoses"), by = c("icd_code", 
                                                       "icd_version")) %>%
      collect() %>%  # Collect everything first before filtering
      group_by(subject_id, long_title) %>%
      summarise(freq = n(), .groups = "drop") %>%  # Count occurrences
      arrange(desc(freq)) %>%  
      group_by(subject_id) %>%
      slice(1:3) %>% 
      pull(long_title)
    
    adt_data <- tbl(con_bq, "transfers") %>%
      filter(subject_id == !!subject_id) %>%
      select(subject_id, intime, outtime, careunit) %>%
      collect() %>%
      mutate(intime = as.POSIXct(intime, format = "%m/%d/%Y %H:%M", tz = "UTC"),
             outtime = as.POSIXct(outtime, format = "%m/%d/%Y %H:%M", 
                                  tz = "UTC"), 
             segment_thickness = if_else(str_detect(careunit, "Care Unit"), 
                                         10, 8))
    
    labevents_data <- tbl(con_bq, "labevents") %>%
      filter(subject_id == !!subject_id) %>%
      select(subject_id, charttime) %>%
      distinct() %>%
      collect() %>%
      mutate(charttime = as.POSIXct(charttime, format = "%Y-%m-%d %H:%M:%S"))
    
    patient_procedures <- tbl(con_bq, "procedures_icd") %>%
      filter(subject_id == !!subject_id) %>%
      left_join(tbl(con_bq, "d_icd_procedures"), 
                by = c("icd_code" = "icd_code", 
                       "icd_version" = "icd_version")) %>%
      collect() %>%
      mutate(chartdate = as.POSIXct(chartdate, format = "%Y-%m-%d"))
    
    if (input$plot_type == "ADT") {
      # Construct Demographic Title & Diagnoses Subtitle
      patient_title <- paste("Patient", 
                             demographics_data$subject_id[1], ", ",
                             demographics_data$gender[1], ", ",
                             demographics_data$anchor_age[1], "years old, ",
                             demographics_data$race[1])
      
      patient_subtitle <- paste(diagnoses_data[1], diagnoses_data[2], 
                                diagnoses_data[3], sep = "
")
      
      output$patient_plot <- renderPlot({
        ggplot() +
          geom_segment(data = adt_data,
                       aes(x = intime, xend = outtime, y = "ADT", yend = "ADT", 
                           color = careunit, linewidth = segment_thickness)) +
          geom_point(data = labevents_data,
                     aes(x = charttime, y = "Lab"),
                     shape = 3, size = 2, color = "black") +
          geom_point(data = patient_procedures,
                     aes(x = chartdate, y = "Procedure", 
                         shape = sub(",.*", "", long_title)),  
                     size = 3, color = "black", fill = "black") +
          
          scale_x_datetime(name = "Calendar Time") +
          scale_y_discrete(name = NULL, 
                           limits = c("Procedure", "Lab", "ADT")) +
          scale_shape_manual(name = "Procedure", 
                             values = c(1:n_distinct(patient_procedures$long_title))) + 
          guides(linewidth = "none") +
          theme_bw() +
          labs(title = patient_title, subtitle = patient_subtitle, x = "Time", 
               y = "")
      })
    }  
    
    else if (input$plot_type == "ICU") {
      icu_data <- tbl(con_bq, "chartevents") %>%
        filter(subject_id == !!subject_id, itemid %in% c(220045, 220180, 220179, 
                                                         223761, 220210)) %>%
        select(subject_id, stay_id, itemid, charttime, valuenum) %>%
        collect() %>%
        left_join(data.frame(itemid = c(220045, 220180, 220179, 223761, 220210),
                             abbreviation = c("HR", "NBPd", "NBPs", 
                                              "Temperature F", "RR")),
                  by = "itemid") %>%
        mutate(abbreviation = factor(abbreviation, 
                                     levels = c("HR", "NBPd", "NBPs", 
                                                "RR", "Temperature F")))
      
      output$patient_plot <- renderPlot({
        ggplot(icu_data, aes(x = charttime, y = valuenum, 
                             color = abbreviation)) +
          geom_point() +
          geom_line() +
          facet_grid(abbreviation ~ stay_id, scales = "free") +
          labs(title = paste("Patient", subject_id, "ICU stays - Vitals"), 
               x = "Time", y = "Measurement") +
          theme_minimal()
      })
    }
  })  
}

shinyApp(ui = ui, server = server)