library(shiny)
library(here)
library(tidyverse)

ui <- fluidPage(
  titlePanel("CDI Percentile Score Calculator"),
  sidebarLayout(
    sidebarPanel(
      selectInput("form_type", "Form Type", 
                  choices = c("eng_ws_prod", "eng_wg_prod", "eng_wg_comp")),
      numericInput("child_age", "Age of Child", value = 20, min = 16, max = 30),
      selectInput("child_sex", "Sex of Child", 
                  choices = c("Male" = "m", "Female" = "f", "All" = NA)),
      numericInput("child_score", "CDI Summary Score", value = 151, min = 0),
      actionButton("calculate", "Calculate Percentile"),
      hr(),
      verbatimTextOutput("percentile_result"),
      verbatimTextOutput("error_message")
    ),
    mainPanel(
      fileInput("file_input", "Upload CSV File", 
                accept = c(".csv")),
      downloadButton("download_data", "Download Processed Data"),
      tableOutput("uploaded_data"),
      plotOutput("percentile_plot")
    )
  )
)


read_percentile_table <- function(file_name) {
  as.matrix(read_csv(here("cdi_benchmarks", file_name), skip = 1))
}

# Generate list of matrices for each available benchmark file
available_benchmarks <- list.files(here("cdi_benchmarks"))
available_benchmarks_matrix <- lapply(available_benchmarks, read_percentile_table)
names(available_benchmarks_matrix) <- available_benchmarks

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
                                     percentile_matrix = available_benchmarks_matrix) {
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
  
  uploaded_data <- reactive({
    req(input$file_input)
    read_csv(input$file_input$datapath)
  })
  
  processed_data <- reactive({
    req(uploaded_data())
    data <- uploaded_data()
    
    data$percentile <- mapply(get_percentile_for_child, 
                              data$form_type, 
                              data$age, 
                              data$score, 
                              data$sex)
    
    data
  })
  
  output$uploaded_data <- renderTable({
    req(processed_data())
    processed_data()
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("processed_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(processed_data(), file)
    }
  )
  
  output$percentile_plot <- renderPlot({
    data <- percentile_data()
    
    # Melt data for plotting
    data_long <- data %>%
      pivot_longer(cols = -1, names_to = "Age", values_to = "Score") %>%
      mutate(Age = as.numeric(Age),
             Percentile = as.numeric(row.names(data)))
    
    ggplot(data_long, aes(x = Score, y = Percentile, color = as.factor(Age))) +
      geom_line() +
      labs(title = "Percentile Scores by Age",
           x = "CDI Summary Score",
           y = "Percentile",
           color = "Age") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)