library(shiny)
library(tidyverse)

# created in load_norms_tables.R
norms_tables <- readRDS("cdi_benchmarks/norms_tables.rds")

required_columns <- c('id', 'form', 'sex', 'age', 'sumscore')

valid_wg_ages = 12:18
valid_ws_ages = 16:30

ui <- fluidPage(
  titlePanel("CDI Percentile Score Calculator"),
  sidebarLayout(
    sidebarPanel(
      h3("Single Child Lookup"),
      p(HTML("To get norms-based percentile for a single child, choose the age (in months), sex (if sex-based norms desired), and form type.")),
      selectInput("form_type", "Form Type", 
                  choices = c("eng_ws_prod", "eng_wg_prod", "eng_wg_comp")),
      numericInput("child_age", "Age of Child", value = 20, min = 12, max = 30),
      selectInput("child_sex", "Sex of Child", 
                  choices = c("Male" = "m", "Female" = "f", "All" = "both")),
      numericInput("child_score", "CDI Summary Score", value = 151, min = 0),
      actionButton("calculate", "Calculate Percentile"),
      hr(),
      verbatimTextOutput("percentile_result"),
      verbatimTextOutput("error_message")
    ),
    mainPanel(
      h3("Bulk Lookup"),
      p(HTML("This tool is meant to help researchers look up percentiles for English CDI:WS / CDI:WG scores based on the 2022 American English norms. (It is only valid for the forms' intended age ranges, and for children from the United States.)")),
      p(HTML("Upload a CSV with 1 row per child. Columns must include: age, sex, form, and sumscore. Allowable values:")),
      p(HTML("<b>age</b>: (in months) valid range for Words & Gestures form: 8-18; valid range for Words & Sentences form: 16-30.")),
      p(HTML("<b>sex</b>: 'm' (male), 'f' (female), or 'both' (for non-sex-specific norms)")),
      p(HTML("<b>form</b>: must be one of ['eng_ws_prod','eng_wg_prod','eng_wg_comp'] (eng=English; prod=production; comp=comprehension")),
      p(HTML("<b>sumscore</b>: total words known (produced / understood; see <b>form</b>); should be 0-396 for WG forms, and 0-680 for WS")),
      fileInput("file_input", "Upload CSV File", 
                accept = c(".csv")),
      tableOutput("uploaded_data"),
      uiOutput("download_button")
    )
  )
)

# Core function that looks up the percentile for a given table, the child's age, and the child's score
get_percentile <- function(lookup_table_mat, child_age, child_score) {
  new_row = matrix(c(1, rep(0, ncol(lookup_table_mat) - 1)), #1, then 0s
                   1,
                   ncol(lookup_table_mat)) #length of real table
  colnames(new_row) <- colnames(lookup_table_mat)
  lookup_table_mat <- rbind(new_row, lookup_table_mat)
  
  lookup2 <- lookup_table_mat[, -1]
  rownames(lookup2) <- lookup_table_mat[, 1]
  
  if (child_age < min(as.numeric(colnames(lookup2))) | 
      child_age > max(as.numeric(colnames(lookup2)))) {
    return(NA)
  }
  
  age_values <- lookup2[, as.character(child_age)]
  
  if (child_score > max(age_values)) {
    return(99)
  } else if (child_score < 0) {
    return(1)
  }
  
  smaller <- max(age_values[age_values <= child_score])
  larger <- min(age_values[age_values >= child_score])
  increment = (larger - smaller) / 5
  
  interp_scores <- seq(smaller, larger, increment)
  
  step = which(interp_scores == max(interp_scores[interp_scores <= child_score])) - 1
  
  max(as.numeric(names(which(age_values == smaller)))) + step
}

get_percentile_for_child <- function(form_type, child_age, 
                                     child_score, 
                                     child_sex = NA,
                                     percentile_matrix = norms_tables) {
  if (is.na(child_sex)) {
    child_sex = "both"
  }
  percentile_key = paste0(form_type, "_", child_sex)
  target_file = paste0(percentile_key, ".csv")
  
  if (!target_file %in% names(percentile_matrix)) {
    message(paste("Missing file for ", target_file))
    return(NA)
  }
  target_matrix <- percentile_matrix[[target_file]]
  get_percentile(target_matrix, child_age, child_score)
}

server <- function(input, output, session) {
  
  uploaded_data <- reactive({
    req(input$file_input)
    inFile <- input$file_input
    
    if(is.null(inFile)) return(NULL)
    
    raw_dat <- read.csv(inFile$datapath, header = T, sep=',')
    
    validate(
      need("id" %in% names(raw_dat), "Need 'id' column in uploaded CSV."),
      need("form" %in% names(raw_dat), "Error: Need 'AWC' or 'AWC_COUNT' in uploaded CSV."),
      need("sex" %in% names(raw_dat) , "Error: Need 'CTC' or 'CT_COUNT' in uploaded CSV."),
      need("age" %in% names(raw_dat) & is.numeric(raw_dat$age), "Error: Need 'age' (numeric age in months in range of 12-30) in uploaded CSV."),
      need("sumscore" %in% names(raw_dat) & is.numeric(raw_dat$sumscore), "Error: Need 'sumscore' (numeric, total words known on CDI) in uploaded CSV.")
    )
    
    dat <- raw_dat %>% select(all_of(required_columns)) 
  })
  
  output$percentile_result <- renderPrint({
    input$calculate
    isolate({
      percentile <- get_percentile_for_child(input$form_type, input$child_age, input$child_score, input$child_sex)
      if (is.na(percentile)) {
        return("Error: Child is too young or too old for this measure.")
      } else {
        paste("Percentile score:", percentile)
      }
    })
  })
  
  processed_data <- reactive({
    req(uploaded_data())
    data <- uploaded_data()
    
    data$percentile <- mapply(get_percentile_for_child, 
                              data$form, 
                              data$age, 
                              data$sumscore, 
                              data$sex)
    
    data
  })
  
  output$uploaded_data <- renderTable({
    req(processed_data())
    processed_data()
  })
  
  
  output$download_button <- renderUI({
    req(input$file_input, processed_data())
    downloadButton("download_data", "Download Data", class = "btn-xs")
  })
  
  output$download_data <- downloadHandler(
    filename = function() "processed_data.csv", 
    content = function(fname) {
      write.csv(processed_data(),
                fname, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)