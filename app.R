library(shiny)
library(DT)
library(httr2)
library(jsonlite)
library(tidyverse)
library(bslib)
library(shinycssloaders)

options(shiny.maxRequestSize = 10 * 1024^2)

# UI
ui <- navbarPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "quartz",
    primary = "#12a79d"
  ),
  "CDISC SDTM-IG Web Portal",

  # First Tab - Data Viewer
  tabPanel(
    "Domains",
    sidebarLayout(
      sidebarPanel(
        selectInput("product", "Select Product:",
          choices = "sdtmig",
          selected = "sdtmig"
        ),
        selectInput("endpoint", "Select Endpoint:",
          choices = c("3-4"),
          selected = c("3-4")
        ),
        tagAppendAttributes(onclick = "alert('Button clicked!');", actionButton("submit_btn", "Submit"))
        # actionButton("submit_btn", "Submit")
      ),
      mainPanel(
        withSpinner(DTOutput("table"))
      )
    )
  ),

  # Second Tab - Controlled Terminology
  tabPanel(
    "Controlled Terminology",
    sidebarLayout(
      sidebarPanel(
        h4("Terminology Reference"),
        helpText("This section provides controlled terminologies used in the dataset."),
        selectInput("ctversion", "Select Product:",
          choices = "sdtmct-2024-09-27",
          selected = "sdtmct-2024-09-27"
        ),
        tagAppendAttributes(onclick = "alert('Button clicked!');", actionButton("submit_ctversion", "Submit"))
        # actionButton("submit_ctversion", "Submit")
      ),
      mainPanel(
        fluidRow(
          column(
            12,
            textInput("filter_val", tags$span("Filter Expression (e.g., Age > 30 & Gender == 'M')", style = "font-size: 12px; font-weight: bold; color: orange;")),
          ),
          fluidRow(
            column(
              3,
              actionButton("apply_filter", "Apply Filter")
            ),
            column(
              3,
              actionButton("clear_btn", "Clear"),
            )
          ),
          fluidRow(
            column(
              12,
              withSpinner(DTOutput("ct_table"))
            )
          )
        )
      )
    )
  )
)



# Server
server <- function(input, output, session) {
  endpoint_df <- readRDS("./data/endpoint_df_links.rds") |>
    filter(product == "SDTM") |>
    mutate(end = sapply(strsplit(.data[["endpoint"]], "/"), \(x) x[4]))

  # Initialize selectInput choices when app starts
  observe({
    updateSelectInput(session, "endpoint", choices = endpoint_df$end, selected = endpoint_df$end[1])
  })

  ct_endpoint_df <- readRDS("./data/endpoint_df_links.rds") |>
    filter(product == "CT" & str_detect(endpoint, "sdtmct")) |>
    mutate(end = sapply(strsplit(.data[["endpoint"]], "/"), \(x) x[5])) |>
    arrange(desc(end))

  observe({
    updateSelectInput(session, "ctversion", choices = ct_endpoint_df$end, selected = ct_endpoint_df$end[1])
  })

  # Reactive URL construction
  url_reactive <- reactive({
    req(input$product, input$endpoint) # Ensure both values are selected
    paste("https://api.library.cdisc.org/api/mdr", input$product, input$endpoint, sep = "/")
  }) |>
    bindEvent(input$submit_btn)


  # dataset_df <- readRDS("./data/dataset_df.rds")

  # Store dataset in a reactive value
  dataset_df_reactive <- reactiveVal(NULL) # Initialize as NULL

  # Observe when both inputs change and fetch data
  observeEvent(url_reactive(), {
    req(url_reactive()) # Ensure URL is not NULL

    # Construct the API request
    req <- request(url_reactive()) %>%
      req_headers(
        "Cache-Control" = "no-cache",
        "api-key" = "ba3d68879a224d8090406948f8155bae",
        "content-type" = "application/json"
      )

    # Send the request and fetch response
    resp <- req %>% req_perform()

    # resp <- 200

    # Check if response is successful
    if (resp_status(resp) == 200) {
      # if (resp == 200) {
      # Parse JSON response
      json_list <- resp %>% resp_body_json()




      # Convert list of lists into a data frame
      dataset_df <- map_dfr(1:length(json_list$classes), function(i) {
        class_data <- json_list$classes[[i]]

        map_dfr(1:length(class_data$datasets), function(j) {
          dataset <- class_data$datasets[[j]]

          if (is.null(dataset$datasetVariables)) {
            return(NULL) # Skip datasets with no variables
          }

          # Extract dataset variables
          variable_df <- map_dfr(1:length(dataset$datasetVariables), function(x) {
            var <- dataset$datasetVariables[[x]]

            # Extract codelist href if available, otherwise NA
            href_value <- if (!is.null(var$`_links`$codelist) && length(var$`_links`$codelist) > 0) {
              var$`_links`$codelist[[1]]$href %||% NA
            } else {
              NA
            }

            data.frame(
              dataset = dataset$name,
              Ordinal = as.numeric(var$ordinal) %||% NA,
              Name = var$name %||% NA,
              Label = var$label %||% NA,
              Description = var$description %||% NA,
              Datatype = var$simpleDatatype %||% NA,
              Role = var$role %||% NA,
              core = var$core %||% NA,
              Codelist = stringr::str_extract(href_value, "C\\d+$"),
              stringsAsFactors = FALSE
            )
          })
        })
      }) |> arrange(dataset, Ordinal)

      dataset_df_reactive(dataset_df)

      # print(json_list)  # Debugging: Print API response
    } else {
      print(paste("API request failed with status:", resp_status(resp)))
      dataset_df_reactive(NULL) # Reset the dataset on failure
    }
  })


  # Reactive filtered data based on selections
  filtered_data <- reactive({
    req(dataset_df_reactive()) # Ensure data exists before returning
    dataset_df_reactive()
  })

  # Render DataTable for Data Viewer
  output$table <- renderDT({
    datatable(filtered_data(),
      filter = "top", options = list(pageLength = 5, autoWidth = FALSE),
      style = "bootstrap",
      escape = FALSE,
      selection = "none",
      rownames = FALSE
    )
  })





  # Reactive URL construction
  ct_url_reactive <- reactive({
    req(input$ctversion) # Ensure both values are selected
    paste("https://api.library.cdisc.org/api/mdr/ct/packages", input$ctversion, sep = "/")
    print(paste("https://api.library.cdisc.org/api/mdr/ct/packages", input$ctversion, sep = "/"))
  }) |>
    bindEvent(input$submit_ctversion)

  # Store dataset in a reactive value
  ct_react_filtered_data <- reactiveVal(NULL) # Initialize as NULL

  # Observe when both inputs change and fetch data
  observeEvent(ct_url_reactive(), {
    req(ct_url_reactive()) # Ensure URL is not NULL

    # Construct the API request
    ctreq <- request(ct_url_reactive()) %>%
      req_headers(
        "Cache-Control" = "no-cache",
        "api-key" = "ba3d68879a224d8090406948f8155bae",
        "content-type" = "application/json"
      )

    # Send the request and fetch response
    ctresp <- ctreq %>% req_perform()

    # Check if response is successful
    if (resp_status(ctresp) == 200) {
      # Parse JSON response
      ct_list <- ctresp %>% resp_body_json()


      # Get length of codelists
      codelist_count <- length(ct_list$codelists)

      # Convert all nested lists into a data frame
      ct_codelist_df <- map_dfr(1:codelist_count, function(i) {
        # Extract current codelist
        ctcodelist <- ct_list$codelists[[i]]

        # Extract codelist-level details
        ct_codelist_info <- data.frame(
          codelist = ctcodelist$conceptId %||% NA,
          definition = ctcodelist$definition %||% NA,
          extensible = ctcodelist$extensible %||% NA,
          name = ctcodelist$name %||% NA,
          nci_preferred_Term = ctcodelist$preferredTerm %||% NA,
          submission_Value = ctcodelist$submissionValue %||% NA,
          stringsAsFactors = FALSE
        )

        # Get length of terms
        terms_count <- length(ctcodelist$terms)

        # Extract term-level details (if available)
        terms_df <- map_dfr(1:terms_count, function(j) {
          term <- ctcodelist$terms[[j]]
          data.frame(
            term = term$conceptId %||% NA,
            term_definition = term$definition %||% NA,
            term_nci_preferred_Term = term$preferredTerm %||% NA,
            term_submission_Value = term$submissionValue %||% NA,
            stringsAsFactors = FALSE
          )
        })

        # Merge codelist details with terms (repeat codelist info for each term)
        bind_cols(ct_codelist_info, terms_df)
      })

      ct_react_filtered_data(ct_codelist_df)

      # print(json_list)  # Debugging: Print API response
    } else {
      print(paste("API request failed with status:", resp_status(ctresp)))
      ct_react_filtered_data(NULL) # Reset the dataset on failure
    }
  })




  # ct_codelist_df <- readRDS("./data/ct_codelist_df.rds")


  # Reactive value to store filtered data, initially the full data
  # ct_react_filtered_data <- reactiveVal(ct_codelist_df) # Start with the original data

  # Render the DataTable (shows either full or filtered data)
  output$ct_table <- renderDT({
    req(ct_react_filtered_data()) # Ensure we have data before rendering
    datatable(ct_react_filtered_data(), filter = "top", options = list(pageLength = 5, autoWidth = FALSE))
  })

  # Apply filter when the Apply Filter button is clicked
  observeEvent(input$apply_filter, {
    req(input$filter_val) # Ensure filter input is available

    if (!is.null(input$filter_val) && input$filter_val != "") {
      tryCatch(
        {
          # Parse and apply the filter expression dynamically
          filter_expr <- input$filter_val
          filtered_data <- ct_react_filtered_data() %>% filter(eval(parse(text = filter_expr)))

          # Check if filtered data is a valid data frame
          if (is.data.frame(filtered_data)) {
            ct_react_filtered_data(filtered_data) # Update the reactive value
          } else {
            showNotification("No rows match the filter", type = "warning")
          }
        },
        error = function(e) {
          # Handle invalid filter expression
          showNotification("Invalid filter expression", type = "error")
        }
      )
    }
  })

  # Clear filter button functionality
  observeEvent(input$clear_btn, {
    updateTextInput(session, "filter_val", value = "") # Reset filter input
    ct_react_filtered_data() # Reset to original data
  })
}

# Run the application
shinyApp(ui = ui, server = server)
