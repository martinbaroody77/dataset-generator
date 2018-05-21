library(shiny)


generate_randoms <- function(columns, complexity) {
  coefficient_vec <- vector(length=columns)
  exponent_vec <- vector(length=columns)
  type_vec <- vector(length=columns)
  
  

  
  
  for (i in 1:columns) {
    coefficient_vec[i] <- sample(-50:50, 1)
    if (complexity == "Low complexity") {
      exponent_vec[i] <- 1
    }
    else if (complexity == "Medium complexity") {
      exponent_vec[i] <- sample(1:3, 1)
    }
    else if  (complexity == "High complexity") {
      exponent_vec[i] <- sample(1:10, 1)
    }
    type_vec[i] <- sample(1:3, 1)
  }
  
  
  
  list(coefficient_vec, exponent_vec, type_vec)
}

build_data_frame <- function(columns, rows, complexity, deviation, coefficient_vec, exponent_vec, type_vec) {
  variable_values <- list()
  dependent_values <- vector(length = rows)
  
  
  for (k in 1:columns) {
    column_values <- vector(length = rows)
    type2_upper <- sample(3:10, 1)
    type3_lower <- sample(0:3333, 1) 
    type3_upper <- sample(6666:10000, 1)
    type2_vals <- vector(length = type2_upper)
    type2_max_use <- ceiling(rows/type2_upper)
    for (q in 1:type2_upper) {
      type2_vals[q]  <- 0 
    }
    for (j in 1:rows) {
      value <- 0
      if (type_vec[k] == 1) {
        value <- sample(50:100, 1)
      }
      else if (type_vec[k] == 2) {
        flag = TRUE
        while (flag) {
          value <- sample(1:type2_upper, 1)
          if (type2_vals[value] <= type2_max_use) {
            flag = FALSE
            type2_vals[value] = type2_vals[value] + 1
          }
        }
      }
      else if (type_vec[k] == 3) {
        value <- sample(type3_lower:type3_upper, 1) / 100
      }
      
      column_values[j] <- value
      
    }
    
    variable_values[[as.character(k)]] <- column_values
  }
  
  for (w in 1:rows) {
    row_vector <- vector(length = columns) 
    for (e in 1:columns) {
      row_vector[e] <- variable_values[[as.character(e)]][w]
    }
    dependent_values[w] <- sum(coefficient_vec * (row_vector ^ exponent_vec))
  }
  
  if (deviation != "No deviation") {
    y_mean <- mean(dependent_values)
    for (r in 1:rows) {
      error_bound <- 0
      if (deviation == "Low deviation") {
        error_bound <- 0.05 * y_mean
      }
      else if (deviation == "Medium deviation") {
        error_bound <- 0.15 * y_mean
      }
      else if (deviation == "High deviation") {
        error_bound <- 0.35 * y_mean
      }
      error_bound_scaled <- ceiling(error_bound * 100) 
      if (error_bound_scaled > 10^10) {
        error_bound_scaled <- 10^10
      }
      
      dependent_values[r] <- dependent_values[r] + (sample(-error_bound_scaled:error_bound_scaled, 1) / 100)
      
    }
    
    
  }
  
  dataset <- data.frame("y"=dependent_values)
  
  for (t in 1:columns) {
    
    data_to_add <- data.frame(name = variable_values[[as.character(t)]])
    names(data_to_add)[names(data_to_add) == "name"] <- paste("x", as.character(t), sep="")
    dataset <- cbind(dataset, data_to_add)
  }
  
  print(coefficient_vec)
  print(exponent_vec)
  print(mean(dependent_values))
  
  dataset
  
}

ui <- fluidPage(
  
  titlePanel("Dataset Generator"),
  
  sidebarLayout(
    
    sidebarPanel(
      p("This app allows users to generate random datasets for which there exist underlying patterns in the data."),
      br(),
      p("For these datasets, we denote y to be the variable to predict and x", tags$sub("i"), " to be an attribute which is related to y in some way."),
      p("Once you customize the desired aspects of the dataset, two sets will be generated: a training set and a test set."),
      br(),
      p("This dataset generator can be used to test machine learning models to see how accurate they are.")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      h1("Customize dataset"),
      br(),
      numericInput("columns", label="Enter the number of attributes you would like y to depend on.", value=5, min=1),
      numericInput("rows", label="Enter the number of data instances you would like for the training data.", value=5, min=1),
      numericInput("rowsTest", label="Enter the number of data instances you would like for the test data.", value=5, min=1),
      selectInput("complexity", label="Enter the level of complexity you would like for the pattern in the data.", choices=list("Low complexity", "Medium complexity", "High complexity"), selected="Low complexity"),
      selectInput("deviation", label="Enter the amount of deviation allowed from the general pattern.", choices=list("No deviation", "Low deviation", "Medium deviation", "High deviation"), selected="No deviation"),
      actionButton("generate", label="Generate datasets"),
      p("Note: you must generate the datasets before attempting to download them."),
      fluidRow(
        downloadButton("download", label="Download train set"),
        downloadButton("downloadTest", label="Download test set")
      )
      
      
    )
  )
)

server <- function(input, output) {
  
  rv <- reactiveValues (coefficient_vec = NULL, exponent_vec = NULL, 
                        type_vec = NULL)
  
  observeEvent(input$generate, {
    random_list <- generate_randoms(columnsInput(), complexityInput())
    
    rv$coefficient_vec <- random_list[[1]]
    rv$exponent_vec <- random_list[[2]]
    rv$type_vec <- random_list[[3]] 
  })
  
  columnsInput <- reactive({
    input$columns
  })
  
  rowsInput <- reactive({
    input$rows
  })
  
  complexityInput <- reactive({
    input$complexity
  })
  
  deviationInput <- reactive({
    input$deviation
  })
  
  rowsTestInput <- reactive({
    input$rowsTest
  })
  
  output$download <- downloadHandler(
    
    filename = function() {
      "train_data.csv"
    },
    
    content = function(file) {
      
      if (!is.null(rv$coefficient_vec) & !is.null(rv$coefficient_vec) & 
          !is.null(rv$coefficient_vec)) {
        write.csv(build_data_frame(columnsInput(), rowsInput(),
                  complexityInput(), deviationInput(), 
                  rv$coefficient_vec, rv$exponent_vec, 
                  rv$type_vec), file)
      }
      else {
        write.csv(data.frame())
      }
      
    }
  )
  
  output$downloadTest <- downloadHandler(
    
    filename = function() {
      "test_data.csv"
    },
    
    content = function(file) {
      if (!is.null(rv$coefficient_vec) & !is.null(rv$coefficient_vec) & 
          !is.null(rv$coefficient_vec)) {
      write.csv(build_data_frame(columnsInput(), rowsTestInput(),
                                 complexityInput(), deviationInput(), 
                                 rv$coefficient_vec, rv$exponent_vec, 
                                 rv$type_vec), file)
      }
      else {
        write.csv(data.frame())
      }
    }
  )
  
  
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
