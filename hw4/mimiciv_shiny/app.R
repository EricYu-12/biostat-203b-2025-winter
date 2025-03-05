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

# Connect to BigQuery
satoken <- "biostat-203b-2025-winter-4e58ec6e5579.json"
bq_auth(path = satoken)

con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2025-winter",
  dataset = "mimiciv_3_1",
  billing = "biostat-203b-2025-winter"
)

## Process & Save Patient Demographics 
demographics_data <- tbl(con_bq, "patients") %>%
  select(subject_id, gender, anchor_age) %>%
  left_join(
    tbl(con_bq, "admissions") %>% select(subject_id, race),
    by = "subject_id"
  ) %>%
  distinct(subject_id, .keep_all = TRUE) %>%  # Ensure one row per patient
  collect()
saveRDS(demographics_data, "demographics_data.rds")

## Process & Save Top 3 Diagnoses 
diagnoses_data <- tbl(con_bq, "diagnoses_icd") %>%
  left_join(tbl(con_bq, "d_icd_diagnoses"), by = c("icd_code", 
                                                   "icd_version")) %>%
  collect() %>%  # Collect everything first before filtering
  group_by(subject_id, long_title) %>%
  summarise(freq = n(), .groups = "drop") %>%  # Count occurrences
  arrange(desc(freq)) %>%  
  group_by(subject_id) %>%
  slice(1:3)  # Take the top 3 diagnoses per subject

saveRDS(diagnoses_data, "diagnoses_data.rds")

## Process & Save ADT Data
adt_data <- tbl(con_bq, "transfers") %>%
  select(subject_id, intime, outtime, careunit) %>%
  collect() %>%
  mutate(intime = as.POSIXct(intime, format = "%Y-%m-%d %H:%M:%S"),
         outtime = as.POSIXct(outtime, format = "%Y-%m-%d %H:%M:%S"))
saveRDS(adt_data, "adt_data.rds")

## Process & Save Labevents
labevents_data <- tbl(con_bq, "labevents") %>%
  select(subject_id, charttime) %>%
  distinct() %>%
  collect() %>%
  mutate(charttime = as.POSIXct(charttime, format = "%Y-%m-%d %H:%M:%S"))
saveRDS(labevents_data, "labevents_data.rds")

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
    req(input$var_select)
    
    if (is.numeric(mimic_icu_cohort[[input$var_select]])) {
      ggplot(mimic_icu_cohort, aes(x = !!sym(input$var_select))) +
        geom_histogram(bins = 30, fill = "skyblue", alpha = 0.7) +
        labs(title = paste("Distribution of", input$var_select),
             x = input$var_select, y = "Count") +
        theme_minimal()
    } else {
      ggplot(mimic_icu_cohort, aes(x = !!sym(input$var_select), 
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
      summary_df <- data.frame(
        Statistic = names(summary(mimic_icu_cohort[[input$var_select]])),
        Value = as.vector(summary(mimic_icu_cohort[[input$var_select]]))
      )
    } else {
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
  
  observeEvent(input$submit, {
    req(input$subject_id)
    subject_id <- as.numeric(input$subject_id)
    
    output$error_message <- renderText({ "" })  # Clear error message
    
    # Load Pre-Processed Data
    demographics_data <- readRDS("demographics_data.rds")
    diagnoses_data <- readRDS("diagnoses_data.rds")
    adt_data <- readRDS("adt_data.rds")
    labevents_data <- readRDS("labevents_data.rds")
    icu_data <- readRDS("icu_data.rds")
    
    if (input$plot_type == "ADT") {
      # Get Patient Data
      patient_demographics <- demographics_data %>%
        filter(subject_id == !!subject_id)
      
      patient_diagnoses <- diagnoses_data %>%
        filter(subject_id == !!subject_id) %>%
        pull(long_title)
      
      patient_adt <- adt_data %>%
        filter(subject_id == !!subject_id)
      
      patient_labevents <- labevents_data %>%
        filter(subject_id == !!subject_id)
      
      patient_procedures <- tbl(con_bq, "procedures_icd") %>%
        filter(subject_id == !!subject_id) %>%
        left_join(tbl(con_bq, "d_icd_procedures"), 
                  by = c("icd_code" = "icd_code", 
                         "icd_version" = "icd_version")) %>%
        collect() %>%
        mutate(chartdate = as.POSIXct(chartdate, format = "%Y-%m-%d"))
      
      # Construct Demographic Title & Diagnoses Subtitle
      patient_title <- paste("Patient", 
                             patient_demographics$subject_id[1], ", ",
                             patient_demographics$gender[1], ", ",
                             patient_demographics$anchor_age[1], "years old, ",
                             patient_demographics$race[1])
      
      patient_subtitle <- paste(patient_diagnoses[1], patient_diagnoses[2], 
                                patient_diagnoses[3], sep = "
")
      
      output$patient_plot <- renderPlot({
        ggplot() +
          geom_segment(data = patient_adt,
                       aes(x = intime, xend = outtime, y = "ADT", yend = "ADT", 
                           color = careunit),
                       size = 4) +
          geom_point(data = patient_labevents,
                     aes(x = charttime, y = "Lab"),
                     shape = 3, size = 2, color = "black") +
          geom_point(data = patient_procedures,
                     aes(x = chartdate, y = "Procedure", 
                         shape = sub(",.*", "", long_title)),
                     size = 3, color = "black") +
          scale_x_datetime(name = "Calendar Time") +
          scale_y_discrete(name = NULL, limits = c("Procedure", "Lab", "ADT")) +
          theme_bw() +
          labs(title = patient_title, subtitle = patient_subtitle, x = "Time", 
               y = "")
      })
    } else if (input$plot_type == "ICU") {
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