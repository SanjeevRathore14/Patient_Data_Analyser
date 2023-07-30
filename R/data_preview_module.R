#####################################Module to show the preview of the dataset#####################################
#UI function
DataPreviewUI <- function(id, dataset) {
  ns <- NS(id)
  tagList(
    fluidRow(
      style = "margin-top:1rem;",
      column(width = 9,
             #heading
             h3("Data Preview")),
      column(width = 3,
             #uioutput for USUBJID filter
             uiOutput(ns("USUBJID_filter_ui")))),
    fluidRow(
      column(width = 12,
             #reactableOutput to show the data view
             shinycssloaders::withSpinner(reactable::reactableOutput(ns("data_preview_table")),type = 8)))
  )
}

#Server function
DataPreviewServer <- function(id,dataset) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    #defining the reactive list
    rv_data_preview <- reactiveValues()
    
    #rendering the filter for USUBJID
    output$USUBJID_filter_ui <- renderUI({
      tagList(
        #picker input to show the list of USUBJID
        shinyWidgets::pickerInput(inputId = ns("user_sub_id"),
                                  choices = sort(unique(dataset[["USUBJID"]])),
                                  multiple = TRUE,
                                  selected = NULL,
                                  label = "Select User Subject ID",
                                  width = "100%",
                                  options = list('actions-box' = TRUE,`live-search` = TRUE,
                                                 'none-selected-text' = "None selected"))
      )
    })
    
    #observe event of USUBJID filter to subset the dataset based on user selection
    observeEvent(input$user_sub_id,ignoreNULL = FALSE,{
      if(is.null(input$user_sub_id)){
        rv_data_preview$dataset <- dataset
      }else{
        rv_data_preview$dataset <- dataset %>% 
          dplyr::filter(USUBJID %in% input$user_sub_id)
      }
    })
    
    #generating the data preview using renderReactable
    output$data_preview_table <- reactable::renderReactable({reactable(data = rv_data_preview$dataset,
                                                                       defaultPageSize = 10,
                                                                       showPageSizeOptions = TRUE,
                                                                       pageSizeOptions = c(10,25,50,100,500,1000),
                                                                       bordered = TRUE, 
                                                                       striped = TRUE, 
                                                                       highlight = TRUE,
                                                                       defaultColDef = colDef(align = "center"))
    })
  })
}