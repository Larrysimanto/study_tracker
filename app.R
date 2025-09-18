#
# Shiny Dashboard for Tracking Clinical Programming Deliverables
#

# 1. LOAD LIBRARIES ----
library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(plotly)
library(shinyjs)

# 2. INITIAL SETUP ----
# Define status levels for consistency
status_levels <- c("Not Started", "In Progress", "QC Review", "Completed", "On Hold")

# Function to load data safely. If file doesn't exist, create an empty dataframe.
load_data <- function(file_path, col_names) {
  if (file.exists(file_path)) {
    read.csv(file_path, stringsAsFactors = FALSE)
  } else {
    data.frame(matrix(ncol = length(col_names), nrow = 0, dimnames = list(NULL, col_names)))
  }
}

# 3. UI (USER INTERFACE) ----
ui <- dashboardPage(
  dashboardHeader(title = "Stats Programming Tracker"),
  
  dashboardSidebar(
    useShinyjs(), # Initialize shinyjs
    # Study selector - dynamically populated from data
    selectInput("study_select", "Select Study:", choices = NULL),
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Dataset Tracker", tabName = "datasets", icon = icon("database")),
      menuItem("TFL Tracker", tabName = "tfls", icon = icon("table-list"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # --- Dashboard Tab ---
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("total_datasets", width = 3),
                valueBoxOutput("total_tfls", width = 3),
                valueBoxOutput("completed_perc", width = 3),
                valueBoxOutput("in_progress_box", width = 3)
              ),
              fluidRow(
                box(
                  title = "Overall Status", status = "primary", solidHeader = TRUE,
                  width = 6, plotlyOutput("status_donut_chart")
                ),
                box(
                  title = "Progress by Type", status = "primary", solidHeader = TRUE,
                  width = 6, plotlyOutput("progress_bar_chart")
                )
              )
      ),
      
      # --- Datasets Tab ---
      tabItem(tabName = "datasets",
              h2("Analysis Dataset Status"),
              fluidRow(
                box(
                  title = "Update Dataset Status", status = "warning", solidHeader = TRUE, width = 12, collapsible = TRUE,
                  selectInput("ds_status_update", "Status:", choices = status_levels),
                  textInput("ds_owner_update", "Owner:"),
                  dateInput("ds_actual_date_update", "Actual Completion Date:"),
                  actionButton("update_ds_btn", "Update Selected Dataset", icon = icon("save"))
                )
              ),
              fluidRow(
                box(
                  title = "Dataset Tracking Table", status = "info", solidHeader = TRUE, width = 12,
                  DTOutput("datasets_table")
                )
              )
      ),
      
      # --- TFLs Tab ---
      tabItem(tabName = "tfls",
              h2("TFL Status"),
              fluidRow(
                box(
                  title = "Update TFL Status", status = "warning", solidHeader = TRUE, width = 12, collapsible = TRUE,
                  selectInput("tfl_status_update", "Status:", choices = status_levels),
                  textInput("tfl_owner_update", "Owner:"),
                  dateInput("tfl_actual_date_update", "Actual Completion Date:"),
                  actionButton("update_tfl_btn", "Update Selected TFL", icon = icon("save"))
                )
              ),
              fluidRow(
                box(
                  title = "TFL Tracking Table", status = "info", solidHeader = TRUE, width = 12,
                  DTOutput("tfls_table")
                )
              )
      )
    )
  )
)


# 4. SERVER LOGIC ----
server <- function(input, output, session) {
  
  # --- Reactive Values for Data ---
  rv <- reactiveValues(
    datasets = load_data("datasets_tracker.csv", c("StudyID", "Dataset", "Type", "Status", "Owner", "PlannedDate", "ActualDate")),
    tfls = load_data("tfls_tracker.csv", c("StudyID", "TFL_ID", "Description", "Status", "Owner", "PlannedDate", "ActualDate"))
  )
  
  # --- Dynamic Study Selector ---
  observe({
    all_studies <- unique(c(rv$datasets$StudyID, rv$tfls$StudyID))
    updateSelectInput(session, "study_select", choices = all_studies, selected = ifelse(length(all_studies) > 0, all_studies[1], ""))
  })
  
  # --- Filtered Data based on Study Selection ---
  filtered_datasets <- reactive({
    req(input$study_select)
    filter(rv$datasets, StudyID == input$study_select)
  })
  
  filtered_tfls <- reactive({
    req(input$study_select)
    filter(rv$tfls, StudyID == input$study_select)
  })
  
  # --- Render Data Tables ---
  output$datasets_table <- renderDT({
    datatable(filtered_datasets(), selection = 'single', options = list(scrollX = TRUE), rownames = FALSE)
  })
  
  output$tfls_table <- renderDT({
    datatable(filtered_tfls(), selection = 'single', options = list(scrollX = TRUE), rownames = FALSE)
  })
  
  # --- Data Update Logic ---
  # Observe selection in dataset table and populate update controls
  observeEvent(input$datasets_table_rows_selected, {
    req(input$datasets_table_rows_selected)
    selected_row <- filtered_datasets()[input$datasets_table_rows_selected, ]
    updateSelectInput(session, "ds_status_update", selected = selected_row$Status)
    updateTextInput(session, "ds_owner_update", value = selected_row$Owner)
    
    current_date <- if(!is.na(selected_row$ActualDate) && selected_row$ActualDate != "") as.Date(selected_row$ActualDate) else Sys.Date()
    updateDateInput(session, "ds_actual_date_update", value = current_date)
  })
  
  # Observe selection in TFL table and populate update controls
  observeEvent(input$tfls_table_rows_selected, {
    req(input$tfls_table_rows_selected)
    selected_row <- filtered_tfls()[input$tfls_table_rows_selected, ]
    updateSelectInput(session, "tfl_status_update", selected = selected_row$Status)
    updateTextInput(session, "tfl_owner_update", value = selected_row$Owner)
    
    current_date <- if(!is.na(selected_row$ActualDate) && selected_row$ActualDate != "") as.Date(selected_row$ActualDate) else Sys.Date()
    updateDateInput(session, "tfl_actual_date_update", value = current_date)
  })
  
  
  # Update Dataset Button
  observeEvent(input$update_ds_btn, {
    req(input$datasets_table_rows_selected)
    selected_ds_name <- filtered_datasets()[input$datasets_table_rows_selected, "Dataset"]
    
    # Update the reactive dataframe
    rv$datasets <- rv$datasets %>%
      mutate(
        Status = ifelse(StudyID == input$study_select & Dataset == selected_ds_name, input$ds_status_update, Status),
        Owner = ifelse(StudyID == input$study_select & Dataset == selected_ds_name, input$ds_owner_update, Owner),
        ActualDate = ifelse(StudyID == input$study_select & Dataset == selected_ds_name, as.character(input$ds_actual_date_update), ActualDate)
      )
    
    # Save to CSV
    write.csv(rv$datasets, "datasets_tracker.csv", row.names = FALSE)
    shinyjs::alert("Dataset status updated and saved!")
  })
  
  # Update TFL Button
  observeEvent(input$update_tfl_btn, {
    req(input$tfls_table_rows_selected)
    selected_tfl_id <- filtered_tfls()[input$tfls_table_rows_selected, "TFL_ID"]
    
    # Update the reactive dataframe
    rv$tfls <- rv$tfls %>%
      mutate(
        Status = ifelse(StudyID == input$study_select & TFL_ID == selected_tfl_id, input$tfl_status_update, Status),
        Owner = ifelse(StudyID == input$study_select & TFL_ID == selected_tfl_id, input$tfl_owner_update, Owner),
        ActualDate = ifelse(StudyID == input$study_select & TFL_ID == selected_tfl_id, as.character(input$tfl_actual_date_update), ActualDate)
      )
    
    # Save to CSV
    write.csv(rv$tfls, "tfls_tracker.csv", row.names = FALSE)
    shinyjs::alert("TFL status updated and saved!")
  })
  
  # --- Dashboard Value Boxes ---
  output$total_datasets <- renderValueBox({
    valueBox(nrow(filtered_datasets()), "Total Datasets", icon = icon("database"), color = "blue")
  })
  output$total_tfls <- renderValueBox({
    valueBox(nrow(filtered_tfls()), "Total TFLs", icon = icon("table-list"), color = "purple")
  })
  output$completed_perc <- renderValueBox({
    total_items <- nrow(filtered_datasets()) + nrow(filtered_tfls())
    completed_items <- sum(filtered_datasets()$Status == "Completed") + sum(filtered_tfls()$Status == "Completed")
    perc <- if(total_items > 0) round((completed_items / total_items) * 100) else 0
    valueBox(paste0(perc, "%"), "Completed", icon = icon("check"), color = "green")
  })
  output$in_progress_box <- renderValueBox({
    in_progress_items <- sum(filtered_datasets()$Status == "In Progress") + sum(filtered_tfls()$Status == "In Progress")
    valueBox(in_progress_items, "In Progress", icon = icon("spinner"), color = "yellow")
  })
  
  # --- Dashboard Plots ---
  # Donut chart for overall status
  output$status_donut_chart <- renderPlotly({
    ds_summary <- filtered_datasets() %>% count(Status, name = "Count")
    tfl_summary <- filtered_tfls() %>% count(Status, name = "Count")
    
    combined_summary <- bind_rows(ds_summary, tfl_summary) %>%
      group_by(Status) %>%
      summarise(Total = sum(Count))
    
    plot_ly(combined_summary, labels = ~Status, values = ~Total, type = 'pie', hole = 0.8) %>%
      layout(title = paste("Overall Status for", input$study_select))
  })
  
  # Bar chart for progress by deliverable type
  output$progress_bar_chart <- renderPlotly({
    ds_data <- filtered_datasets() %>% select(Type, Status)
    tfl_data <- filtered_tfls() %>% mutate(Type = "TFL") %>% select(Type, Status)
    
    all_data <- bind_rows(ds_data, tfl_data)
    
    plot_ly(all_data, x = ~Type, color = ~Status, type = "histogram") %>%
      layout(barmode = 'stack', xaxis = list(title = "Deliverable Type"), yaxis = list(title = "Count"))
  })
  
}

# 5. RUN THE APP ----
shinyApp(ui, server)