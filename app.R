library(shiny)
library(DT)
library(httr2)
library(jsonlite)
library(tidyverse)
library(bslib)
library(shinycssloaders)

options(shiny.maxRequestSize = 10 * 1024^2)

# UI
ui <- tagList(
  # The main app content wrapped in navbarPage:
  navbarPage(
    theme = bs_theme(
      version = 5,
      bootswatch = "quartz",
      primary = "#12a79d"
    ),
    "CDISC Library Web Portal",

    # First Tab - Data Viewer
    tabPanel(
      "SDTM-IG",
      fluidRow(
        column(
          2,
          selectInput("product", "Select Product:", choices = "sdtmig", selected = "sdtmig")
        ),
        column(
          2,
          selectInput("endpoint", "Select Version:", choices = c("3-4"), selected = c("3-4"))
        ),
        column(
          2,
          br(),
          tagAppendAttributes(onclick = "alert('Button clicked!');", actionButton("submit_btn", "Submit"))
        )
      ),
      fluidRow(
        column(12, uiOutput("version_header"))
      ),
      fluidRow(
        column(12, uiOutput("listofdf"))
      ),
      fluidRow(
        # Wrap the table output in a div with bottom margin to avoid overlap with footer
        div(
          style = "margin-bottom: 50px;",
          withSpinner(DTOutput("table"))
        )
      )
    ),

    # Second Tab - Controlled Terminology
    tabPanel(
      "SDTM Controlled Terminology",
      fluidRow(
        column(
          2,
          selectInput("ctversion", "Select CT Version:", choices = "sdtmct-2024-09-27", selected = "sdtmct-2024-09-27")
        ),
        column(
          2,
          br(),
          tagAppendAttributes(onclick = "alert('Button clicked!');", actionButton("submit_ctversion", "Submit"))
        )
      ),
      fluidRow(
        column(
          6,
          textInput(
            "filter_val",
            tags$span("Filter Expression (e.g., Age > 30 & Gender == 'M')",
              style = "font-size: 12px; font-weight: bold; color: orange;"
            )
          )
        )
      ),
      fluidRow(
        column(2, actionButton("apply_filter", "Apply Filter")),
        column(1, actionButton("clear_btn", "Clear"))
      ),
      fluidRow(
        column(
          12,
          br(),
          uiOutput("version_ct_header"),
          # Add margin-bottom to avoid footer overlap
          div(
            style = "margin-bottom: 50px;",
            withSpinner(DTOutput("ct_table"))
          )
        )
      )
    ),
    tabPanel(
      "ADaM-IG",
      fluidRow(
        column(
          2,
          selectInput("adam_product", "Select Product:", choices = "adam-adae-1-0", selected = "adam-adae-1-0")
        ),
        column(
          2,
          selectInput("adam_endpoint", "Select Version:", choices = "ADAE", selected = "ADAE")
        ),
        column(
          2,
          br(),
          tagAppendAttributes(onclick = "alert('Button clicked!');", actionButton("submit_adam_btn", "Submit"))
        )
      ),
      fluidRow(
        column(12, uiOutput("version_adam_header"))
      ),
      fluidRow(
        # Wrap the table output in a div with bottom margin to avoid overlap with footer
        div(
          style = "margin-bottom: 50px;",
          withSpinner(DTOutput("adam_table"))
        )
      )
    ),
    tabPanel(
      "ADaM Controlled Terminology",
      fluidRow(
        column(
          2,
          selectInput("adamctversion", "Select CT Version:", choices = "adamct-2024-09-27", selected = "adamct-2024-09-27")
        ),
        column(
          2,
          br(),
          tagAppendAttributes(onclick = "alert('Button clicked!');", actionButton("submit_adamctversion", "Submit"))
        )
      ),
      fluidRow(
        column(
          6,
          textInput(
            "filter_adamval",
            tags$span("Filter Expression (e.g., Age > 30 & Gender == 'M')",
              style = "font-size: 12px; font-weight: bold; color: orange;"
            )
          )
        )
      ),
      fluidRow(
        column(2, actionButton("apply_adamfilter", "Apply Filter")),
        column(1, actionButton("clear_adambtn", "Clear"))
      ),
      fluidRow(
        column(
          12,
          br(),
          uiOutput("version_adamct_header"),
          # Add margin-bottom to avoid footer overlap
          div(
            style = "margin-bottom: 50px;",
            withSpinner(DTOutput("adamct_table"))
          )
        )
      )
    )
  ),


  # Fixed footer placed outside the navbarPage
  tags$footer(
    "Developed by Jagadish Katam",
    style = "position: fixed; bottom: 0; width: 100%; color: white; padding: 1px; text-align: center; border-top: 1px solid #e7e7e7;"
  )
)

# Server
server <- function(input, output, session) {
  # SDTM IG -----------------------------------------------------------------


  endpoint_df <- readRDS("./data/endpoint_df_links.rds") |>
    filter(toupper(product) == "SDTMIG") |>
    mutate(end = sapply(strsplit(.data[["endpoint"]], "/"), \(x) x[4]))


  # Initialize selectInput choices when app starts
  observe({
    updateSelectInput(session, "endpoint", choices = endpoint_df$end, selected = endpoint_df$end[1])
  })


  # Reactive version value
  selected_version <- reactiveVal(paste0("v", str_replace_all(endpoint_df$end[1], "-", "."))) # Default

  # Update the selected version when the button is clicked
  observeEvent(input$submit_btn, {
    selected_version(paste0("v", str_replace_all(input$endpoint, "-", ".")))
  })


  output$version_header <- renderUI({
    # Check if the submit button has been clicked
    if (is.null(input$submit_btn) || input$submit_btn == 0) {
      # Initially, display the header without version info
      h3("SDTM Implementation Guide")
    } else {
      # After the button is clicked, display the header with the version
      h3("SDTM Implementation Guide", selected_version())
    }
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
              Name = if (is.null(var$name)) NA else var$name,
              Label = if (is.null(var$label)) NA else var$label,
              Description = if (is.null(var$description)) NA else var$description,
              Datatype = if (is.null(var$simpleDatatype)) NA else var$simpleDatatype,
              Role = if (is.null(var$role)) NA else var$role,
              core = if (is.null(var$core)) NA else var$core,
              Codelist = stringr::str_extract(href_value, "C\\d+$"),
              Ordinal = if (is.null(var$ordinal)) NA else as.numeric(var$ordinal),
              stringsAsFactors = FALSE
            )
          })
        })
      }) |>
        arrange(dataset, Ordinal) |>
        rename_all(toupper)

      dataset_df_reactive(dataset_df)

      # print(json_list)  # Debugging: Print API response
    } else {
      print(paste("API request failed with status:", resp_status(resp)))
      dataset_df_reactive(NULL) # Reset the dataset on failure
    }
  })

  datasetslist <- reactiveVal(NULL)

  observeEvent(input$submit_btn, {
    datasetslist(dataset_df_reactive() |> distinct(DATASET) |> pull())
  })

  output$listofdf <- renderUI({
    req(dataset_df_reactive())
    dataset_string <- paste(datasetslist(), collapse = ", ")
    h5("List of Datasets:", dataset_string)
  })


  # Reactive filtered data based on selections
  filtered_data <- reactive({
    req(dataset_df_reactive()) # Ensure data exists before returning
    dataset_df_reactive()
  })

  # Render DataTable for Data Viewer
  output$table <- renderDT({
    datatable(
      filtered_data(),
      filter = "top",
      options = list(
        pageLength = 10,
        scrollY = 300,
        deferRender = TRUE,
        scrollCollapse = TRUE
      )
    )
  })

  # SDTM CT -----------------------------------------------------------------



  ct_endpoint_df <- readRDS("./data/endpoint_df_links.rds") |>
    filter(toupper(product) == "CT" & str_detect(endpoint, "sdtmct")) |>
    mutate(end = sapply(strsplit(.data[["endpoint"]], "/"), \(x) x[5])) |>
    arrange(desc(end))

  observe({
    updateSelectInput(session, "ctversion", choices = ct_endpoint_df$end, selected = ct_endpoint_df$end[1])
  })

  selected_ct_version <- reactiveVal(format(as.Date(str_extract(ct_endpoint_df$end[1], "\\d{4}-\\d{2}-\\d{2}")), "%d-%b-%Y"))

  observeEvent(input$submit_ctversion, {
    selected_ct_version(format(as.Date(str_extract(input$ctversion, "\\d{4}-\\d{2}-\\d{2}")), "%d-%b-%Y"))
  })

  output$version_ct_header <- renderUI({
    if (is.null(input$submit_ctversion) || input$submit_ctversion == 0) {
      h3("SDTM Controlled Terminology")
    } else {
      h3("SDTM Controlled Terminology as on ", selected_ct_version())
    }
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
        bind_cols(ct_codelist_info, terms_df) |> rename_all(toupper)
      })

      ct_codelist_df <<- ct_codelist_df

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
    datatable(
      ct_react_filtered_data(),
      filter = "top",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        scrollY = 300,
        deferRender = TRUE,
        scrollCollapse = TRUE
      )
    )
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
    ct_react_filtered_data(ct_codelist_df) # Reset to original data
  })



  # ADaM --------------------------------------------------------------------

  endpoint_adam_df <- readRDS("./data/adam_endpoint_df_links.rds") |>
    filter(toupper(product) == "ADAM")


  # Initialize selectInput choices when app starts
  observe({
    updateSelectInput(session, "adam_product", choices = endpoint_adam_df$endpoint, selected = endpoint_adam_df$endpoint[1])
  })

  adam_endpoint_datasets <- reactive({
    req(input$adam_product)
    endpoint_adam_df |>
      filter(endpoint == input$adam_product) |>
      pull(dataset)
  })

  observe({
    updateSelectInput(session, "adam_endpoint",
      choices = adam_endpoint_datasets(),
      selected = if (length(adam_endpoint_datasets()) > 0) adam_endpoint_datasets()[1] else NULL
    )
  })

  # Reactive version value
  selected_adam_version <- reactiveVal(paste0(str_replace_all(endpoint_adam_df$endpoint[1], "-", "."))) # Default

  # Update the selected version when the button is clicked
  observeEvent(input$submit_adam_btn, {
    selected_adam_version(paste0(str_replace_all(input$adam_endpoint, "-", ".")))
  })


  output$version_adam_header <- renderUI({
    # Check if the submit button has been clicked
    if (is.null(input$submit_adam_btn) || input$submit_adam_btn == 0) {
      # Initially, display the header without version info
      h3("ADaM Implementation Guide")
    } else {
      # After the button is clicked, display the header with the version
      h3("ADaM Implementation Guide", selected_adam_version())
    }
  })

  # Reactive URL construction
  url_adam_reactive <- reactive({
    req(input$adam_product, input$adam_endpoint) # Ensure both values are selected
    paste0("https://api.library.cdisc.org/api/mdr/adam/", input$adam_product, "/datastructures/", input$adam_endpoint)
    # print(paste("https://api.library.cdisc.org/api/mdr/adam/", input$adam_product,"/datastructures/" ,input$adam_endpoint))
  }) |>
    bindEvent(input$submit_adam_btn)


  # dataset_df <- readRDS("./data/dataset_df.rds")

  # Store dataset in a reactive value
  adam_dataset_df_reactive <- reactiveVal(NULL) # Initialize as NULL

  # Observe when both inputs change and fetch data
  observeEvent(url_adam_reactive(), {
    req(url_adam_reactive()) # Ensure URL is not NULL

    # Construct the API request
    req <- request(url_adam_reactive()) %>%
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
      endpoint <- resp %>% resp_body_json()


      # endpoint$analysisVariableSets[[1]]$analysisVariables[[1]]$simpleDatatype

      adam_df <- map_dfr(seq_along(endpoint$analysisVariableSets), \(x){
        map_dfr(seq_along(endpoint$analysisVariableSets[[x]]$analysisVariables), \(j){
          tibble(
            analysisdataset = input$adam_endpoint,
            name = endpoint$analysisVariableSets[[x]]$analysisVariables[[j]]$name,
            label = endpoint$analysisVariableSets[[x]]$analysisVariables[[j]]$label,
            description = endpoint$analysisVariableSets[[x]]$analysisVariables[[j]]$description,
            core = endpoint$analysisVariableSets[[x]]$analysisVariables[[j]]$core,
            simpleDatatype = endpoint$analysisVariableSets[[x]]$analysisVariables[[j]]$simpleDatatype,
            # ordinal = endpoint$analysisVariableSets[[x]]$ordinal,
            analysisVariableSets = endpoint$analysisVariableSets[[x]]$name,
            codelist = if (length(str_extract(endpoint$analysisVariableSets[[x]]$analysisVariables[[j]]$`_links`$codelist[[1]]$href, "C.*")) == 0) {
              NA_character_
            } else {
              str_extract(endpoint$analysisVariableSets[[x]]$analysisVariables[[j]]$`_links`$codelist[[1]]$href, "C.*")
            }
          )
        })
      }) |> rename_all(toupper)

      adam_dataset_df_reactive(adam_df)

      # print(json_list)  # Debugging: Print API response
    } else {
      print(paste("API request failed with status:", resp_status(resp)))
      adam_dataset_df_reactive(NULL) # Reset the dataset on failure
    }
  })

  # Render DataTable for Data Viewer
  output$adam_table <- renderDT({
    datatable(
      adam_dataset_df_reactive(),
      filter = "top",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        scrollY = 300,
        deferRender = TRUE,
        scrollCollapse = TRUE
      )
    )
  })


  # ADaM CT -----------------------------------------------------------------



  adam_ct_endpoint_df <- readRDS("./data/endpoint_df_links.rds") |>
    filter(toupper(product) == "CT" & str_detect(endpoint, "adamct")) |>
    mutate(end = sapply(strsplit(.data[["endpoint"]], "/"), \(x) x[5])) |>
    arrange(desc(end))

  observe({
    updateSelectInput(session, "adamctversion", choices = adam_ct_endpoint_df$end, selected = adam_ct_endpoint_df$end[1])
  })

  selected_adamct_version <- reactiveVal(format(as.Date(str_extract(adam_ct_endpoint_df$end[1], "\\d{4}-\\d{2}-\\d{2}")), "%d-%b-%Y"))

  observeEvent(input$submit_adamctversion, {
    selected_ct_version(format(as.Date(str_extract(input$ctversion, "\\d{4}-\\d{2}-\\d{2}")), "%d-%b-%Y"))
  })

  output$version_adamct_header <- renderUI({
    if (is.null(input$submit_adamctversion) || input$submit_adamctversion == 0) {
      h3("ADaM Controlled Terminology")
    } else {
      h3("ADaM Controlled Terminology as on ", selected_adamct_version())
    }
  })


  # Reactive URL construction
  adamct_url_reactive <- reactive({
    req(input$adamctversion) # Ensure both values are selected
    paste("https://api.library.cdisc.org/api/mdr/ct/packages", input$adamctversion, sep = "/")
    print(paste("https://api.library.cdisc.org/api/mdr/ct/packages", input$adamctversion, sep = "/"))
  }) |>
    bindEvent(input$submit_adamctversion)

  # Store dataset in a reactive value
  adamct_react_filtered_data <- reactiveVal(NULL) # Initialize as NULL

  # Observe when both inputs change and fetch data
  observeEvent(adamct_url_reactive(), {
    req(adamct_url_reactive()) # Ensure URL is not NULL

    # Construct the API request
    ctreq <- request(adamct_url_reactive()) %>%
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
      adamct_codelist_df <- map_dfr(1:codelist_count, function(i) {
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
        bind_cols(ct_codelist_info, terms_df) |> rename_all(toupper)
      })

      adamct_codelist_df <<- adamct_codelist_df

      adamct_react_filtered_data(adamct_codelist_df)

      # print(json_list)  # Debugging: Print API response
    } else {
      print(paste("API request failed with status:", resp_status(ctresp)))
      adamct_react_filtered_data(NULL) # Reset the dataset on failure
    }
  })




  # ct_codelist_df <- readRDS("./data/ct_codelist_df.rds")


  # Reactive value to store filtered data, initially the full data
  # ct_react_filtered_data <- reactiveVal(ct_codelist_df) # Start with the original data

  # Render the DataTable (shows either full or filtered data)
  output$adamct_table <- renderDT({
    req(adamct_react_filtered_data()) # Ensure we have data before rendering
    datatable(
      adamct_react_filtered_data(),
      filter = "top",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        scrollY = 300,
        deferRender = TRUE,
        scrollCollapse = TRUE
      )
    )
  })

  # Apply filter when the Apply Filter button is clicked
  observeEvent(input$apply_adamfilter, {
    req(input$filter_adamval) # Ensure filter input is available

    if (!is.null(input$filter_adamval) && input$filter_adamval != "") {
      tryCatch(
        {
          # Parse and apply the filter expression dynamically
          filter_expr <- input$filter_adamval
          filtered_data <- adamct_react_filtered_data() %>% filter(eval(parse(text = filter_expr)))

          # Check if filtered data is a valid data frame
          if (is.data.frame(filtered_data)) {
            adamct_react_filtered_data(filtered_data) # Update the reactive value
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
  observeEvent(input$clear_adambtn, {
    updateTextInput(session, "filter_adamval", value = "") # Reset filter input
    adamct_react_filtered_data(adamct_codelist_df) # Reset to original data
  })
}

# Run the application
shinyApp(ui = ui, server = server)
