library(ggplot2)
library(GEOquery)

get_pdata <- function(geo_id){
  granja <- getGEO(geo_id)
    # Get pData for each element and concatenate tables
  return(purrr::map(granja, pData) %>% dplyr::bind_rows())
}

srvTable <- function(id, dat) { shiny::moduleServer(id,
                                                    function(input, output, session) {
                                                      output$table <- renderDT({DT::datatable(dat)})
                                                    }
)}

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # values <- reactiveValues(setup = NULL, download=NULL, enableGEO=F)
  
  # Once the data downloading strategy has been selected, move to the next screen
  output$steptwo = renderUI({
    # print(input)
    
    condition <- input$doi!="" & input$alias!=""
    if(!condition | input$dataset=="na"){
      shinyjs::disable("downloadData")
      
    }else if(input$dataset=="impossible"){
      tagList(textInput("source", "data source (url link)"),
      textAreaInput("comment", "comment on how to download", paste0("... and store the adt data in `./supp_protein/",input$alias,
                                                                    "/`, the RNA data in `./supp_rna/",input$alias,
                                                                    "/`, the HTO data in `./supp_hto/",input$alias,
                                                                    "/`, and the metadata in `./metadata/`"), width = "100%"),
      shinyjs::enable("downloadData"))
    }else if(input$dataset=="geo"){
      tagList(textInput("id", labelMandatory("GEO id")),
              radioButtons('geodownload', 
                           label=labelMandatory("download type"),
                           choices = c("Nothing selected"="na",'download via GEOquery'="geo", "direct download"="wget"),
                           selected = "na")
             )
    }else{
      tagList(selectInput('columns2', 'Columns', c("a", "b")),
      shinyjs::enable("downloadData"))
    }
  })

  # GEO download  
  output$stepthree = renderUI({
    condition <- input$doi!="" & input$alias!=""
    
    if (!is.null(input$geodownload) & input$dataset!="na" & condition){
      if(input$geodownload=="na" | input$id==""){
      }else if(input$geodownload=="geo"){
        actionButton("geo_download_button", "load GEO")
      }else{
        renderText({"wget option"})
      }
    }
  })

  observeEvent(input$dataset,
               {
                 if (input$dataset=="na") {
                   output$selected_var <-
                     renderText({
                       "There is some basic information that one should fill in to characterize a particular dataset. Basic description of what this app is supposed to do"
                     })
                 }else{
                   output$selected_var <- NULL
                 }
               })
  
  observeEvent(input$geo_download_button, {
    message("CLICK!")
    pdata <- get_pdata(input$id)
    print(pdata)
    
    output$stepfour = renderUI({
      selectInput("columns", "select column", colnames(pdata))
    })
    srvTable("tablegeo", pdata)
    
    # output$geodata <- renderDataTable({#
    #   DT::datatable(pdata)
    # }) 
  })
    
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$alias, '.yaml', sep='')
    },
    content = function(con) {
      data <- list(download=list(setup=input$setup_type,
                                 download=input$download,
                                 id = input$id),
                   load=list(separate_samples=input$separate_samples),
                   metadata=list(doi=input$doi,
                                 description=input$description,
                                 tissue=input$tissue,
                                 species=input$species,
                                 alias=input$alias,
                                 genome_build=input$genome_build))
      yaml::write_yaml(data, con)
    }
  )
    
}
