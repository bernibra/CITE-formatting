library(ggplot2)
library(GEOquery)
library(markdown)
library(dplyr)

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

get_pdata <- function(geo_id){
  granja <- tryCatch(getGEO(geo_id), error=function(cond) {NULL})
    # Get pData for each element and concatenate tables
  if(is.null(granja)){
    return(NULL)
  }else{
    return(purrr::map(granja, pData) %>% dplyr::bind_rows())
  }
}

regfilter <- function(x, y, z){
  w <- c(x,y,z)
  w <- w[w!=""]
  if(length(w)==0){
    return("")
  }else{
    return(paste(w, collapse ="|"))
  }
}

##### Module for asking extra attributes to class load
askextraclass <- function(id){
  ns <- NS(id)
  uiOutput(ns("controls"))
}

askextraclassServer <- function(id, class, typ){
  moduleServer(
    id, 
    function(input, output, session) {
      if(is.null(class)){
        output$controls <-  NULL
      }else if(class=="h5" | class=="h5seurat"){
        output$controls <-  renderUI({
          ns <- session$ns
          tagList(
            textInput(ns("h5key"),  placeholder = paste(typ, "experiment key", sep=" "),tags$span(style="font-weight: normal;","h5 experiment key"))
          )
        })
      }else if(class=="Seurat"){
        output$controls <-  renderUI({
          ns <- session$ns
          tagList(
            textInput(ns("altexp"),  placeholder = paste(typ, "experiment key", sep=" "), label = tags$span(style="font-weight: normal;","Seurat experiment key"))
          )
        })
      }else if(class=="mtx"){
        output$controls <-  renderUI({
          ns <- session$ns
          tagList(
            textInput(ns("features"),  placeholder = "pattern for features file", label = tags$span(style="font-weight: normal;","features")),
            textInput(ns("cells"),  placeholder = "pattern for cell file", label = tags$span(style="font-weight: normal;","cells")),
            textInput(ns("replace"),  placeholder = "pattern for matrix file", label = tags$span(style="font-weight: normal;","matrix")),
            numericInput(ns("column"), label = tags$span(style="font-weight: normal;","column used in the features file"),   min = 1,value=1)
          )
        })
      }else if(class=="access"){
        output$controls <-  renderUI({
          ns <- session$ns
          tagList(
            textInput(ns("access"),  placeholder = paste(typ, "access key", sep=" "), label = tags$span(style="font-weight: normal;","R access key"))
          )
        })
      }else if(class=="rds" | class=="csv" | class=="fastcsv"){
        output$controls <-  renderUI({
          ns <- session$ns
          tagList(
            textInput(ns("drop"),  placeholder = "pattern used to drop rows", label = tags$span(style="font-weight: normal;","columns to drop")),
            textInput(ns("keep"),  placeholder = "pattern used to keep rows", label = tags$span(style="font-weight: normal;","columns to keep"))
          )
        })
      }else{
        output$controls <-  NULL
      }
    }
  )
}
#####


##### Module for loading protein, RNA or HTO
loaddata <- function(id){
  ns <- NS(id)
  uiOutput(ns("controls"))
}

loaddataServer <- function(id, on=T, typ="protein"){
  moduleServer(
    id, 
    function(input, output, session) {
      if(on){
        output$controls <- renderUI({
          ns <- session$ns
          tagList(div(class="loaddata", 
            h4(paste(typ, "data", sep=" ")),
            checkboxInput(ns("transpose"), "transpose", value = FALSE),
            checkboxInput(ns("separate_samples"), "separate samples", value = TRUE),
            textInput(ns("keywords_load"), "search file by pattern", placeholder = "e.g. 'CITE$'"),
            selectInput(ns('class'), 
                        label="data format",
                        choices = c("default", "csv", "mtx", "rds", "fastcsv", "h5", "h5seurat", "Seurat", "access", "h5ad"),
                        selected = "default"),
            askextraclass(ns('extra_class')),
            textInput(ns("coldata"), "rows moved to colData", placeholder = "separated by semicolon"),
           )
          )
        })
      }else{
        output$controls <- NULL
      }
    }
  )
}
#####
#####
addDownloadlink <-function(id, name, mandatory=FALSE){
  ns <- NS(id)
  if(mandatory){
    tagList(
      br(),
      br(),
      p(style="font-weight: bold;", labelMandatory(paste(name, "files", sep=" "))),
      actionButton(ns("show"), "add data"),
      br(),
      br(),
      p("added files:"),    
      verbatimTextOutput(ns("dataInfo"))
    )
  }else{
    tagList(
      br(),
      br(),
      p(style="font-weight: bold;", paste(name, "files", sep=" ")),
      actionButton(ns("show"), "add data"),
      br(),
      br(),
      p("added files:"),    
      verbatimTextOutput(ns("dataInfo"))
    )
  }
}

dataModal <- function(ns, failed = FALSE, withoutlink=F) {
  # Popup for download links addition
  
  if(withoutlink){
    modalDialog(
      
      textInput(ns("file"), "file name",
                placeholder = 'e.g. metadata.csv'
      ),
      span('make sure the file name has the right extension'),
      
      if (failed)
        div(tags$b("Invalid name file or url", style = "color: red;")),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("ok"), "OK")
      )
    )
  }else{
    modalDialog(
      
      textInput(ns("file"), "file name",
                placeholder = 'e.g. metadata.csv'
      ),
      span('make sure the file name has the right extension'),
      
      textInput(ns("url"), "url",
                placeholder = NULL
      ),
      span('make sure the url is public and working'),
      
      if (failed)
        div(tags$b("Invalid name file or url", style = "color: red;")),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("ok"), "OK")
      )
    )
  }
}

addDownloadlinkServer <-function(id, values, withoutlink=F){
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
    
      # Show modal when button is clicked.
      observeEvent(input$show, {
        showModal(dataModal(ns, failed = FALSE, withoutlink))
      })
      
      observeEvent(input$ok, {
        if(withoutlink){
          url <- "link"
        }else{
          url <- input$url
        }
        
        # Check that data object exists and is data frame.
        if (!is.null(input$file) && nzchar(input$file) && input$file!="" &&
            !is.null(url) && nzchar(url) && url!="") {
          values[[ns("url")]] <- rbind(values[[ns("url")]], url)
          values[[ns("data")]] <- rbind(values[[ns("data")]], input$file)
          removeModal()
        } else {
          showModal(dataModal(ns, failed = TRUE, withoutlink=withoutlink))
        }
      })
      
      # Display information about selected data
      output$dataInfo <- renderPrint({
        if (is.null(values[[ns("data")]]))
          "No data selected"
        else
          values[[ns("data")]]
      })
    }
  )
}
#####


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Values to save across elements
  values<-reactiveValues()
    
  # First reactive selection (what type of entry is it? GEO, ArrayExpress, direct download,...)
  output$download_steptwo = renderUI({
    # print(input)
    
    condition <- input$doi!="" & input$alias!=""
    if(!condition | input$dataset=="na"){
      shinyjs::disable("downloadData")
      
    }else if(input$dataset=="impossible"){
      tagList(
        h4("Download"),
        textInput("source", "data source (url link)"),
        textAreaInput("comment", "comment on how to download", paste0("... and store the adt data in `./supp_protein/",input$alias,
                                                                    "/`, the RNA data in `./supp_rna/",input$alias,
                                                                    "/`, the HTO data in `./supp_hto/",input$alias,
                                                                    "/`, and the metadata in `./metadata/`"), width = "100%"),
        addDownloadlink("Impossiblefiles", name="expected files"),
      shinyjs::enable("downloadData"))
    }else if(input$dataset=="geo"){
      tagList(
              h4("Download data"),
              textInput("id", labelMandatory("GEO id"), placeholder = "e.g. GSE139369"),
              radioButtons('geodownload', 
                           label=labelMandatory("download type"),
                           choices = c("Nothing selected"="na",'download via GEOquery'="geo", "direct download"="wget"),
                           selected = "na"),
              addDownloadlink("GEOmetadata", name="metadata")
             )
    }else{
      tagList(selectInput('columns2', 'Columns', c("a", "b")),
      shinyjs::enable("downloadData"))
    }
  })

  # GEO Load metadata
  output$download_stepthree = renderUI({
    condition <- input$doi!="" & input$alias!=""
    
    if (!is.null(input$geodownload) & input$dataset!="na" & condition){
      if(input$geodownload=="na" | input$id==""){
      }else if(input$geodownload=="geo"){ 
        actionButton("geo_download_button", "load GEO")
      }else{
        actionButton("geo_download_button_2", "loading info")
      }
    }
  })

  # Select description and keyword from metadata
  observeEvent(input$geo_download_button, {
    shinyjs::disable("geodownload")
    shinyjs::disable("dataset")
    shinyjs::disable("alias")
    shinyjs::disable("doi")
    values$pdata <- get_pdata(input$id)
    if(is.null(values$pdata)){
      output$download_stepfour = renderUI({p(style="color: red;", "The GEO id can't be found")})
    }else{
      shinyjs::disable("id")
      output$download_stepfour = renderUI({
        tagList(
          selectInput("columns", labelMandatory("select column"), c("Not selected"="na", colnames(values$pdata)), selected = "na"),
          p("To filter out the different files, we need to select a column of the GEO metadata."),
          checkboxInput("include_hto", "Include HTO data", value = TRUE),
          textInput("keyword_protein", placeholder = "regex for ADT files", label = NULL),
          textInput("keyword_rna", placeholder = "regex for RNA files", label = NULL),
          textInput("keyword_hto", placeholder = "regex for HTO files", label = NULL),
          actionButton("geodone", "done", width = "30%")
        )
      })
    }
  })
  
  # Select description and keyword from metadata
  observeEvent(input$geo_download_button_2, {
    shinyjs::disable("geodownload")
    shinyjs::disable("dataset")
    shinyjs::disable("alias")
    shinyjs::disable("doi")
    shinyjs::disable("id")
    output$download_stepfour = renderUI({
      tagList(
        checkboxInput("include_hto2", "Include HTO data", value = FALSE),
        p("One needs to explicitely check the HTO box if HTO data needs to be processed (ingnoring HTO information otherwise)."),
        addDownloadlink("GEOproteindata", name="protein", mandatory=T),
        addDownloadlink("GEOrnadata", name="RNA"),
        addDownloadlink("GEOhtodata", name="HTO"),
        actionButton("geodone2", "done", width = "30%")
      )
    })
  })
  
  
  # Filtering of Protein files for GEO database
  observeEvent(c(input$columns, input$keyword_protein), {
    if (input$columns!="na") {
       output$selected_var <- NULL
       output$load_steptwo <- NULL
       output$load_stepone <- NULL
       output$load_line <- NULL
       output$tablegeo_protein <- renderTable({
         values$pdata %>% select(input$columns) %>% dplyr::rename_with(function(x) "Protein data") %>%  filter_at(1, all_vars(grepl(input$keyword_protein, .)))
     })
    }else{
      output$tablegeo_protein <- NULL
    }
  })

  # Filtering of rna files for GEO database
  observeEvent(c(input$columns, input$keyword_rna), {
    if (input$columns!="na") {
      output$selected_var <- NULL
      output$load_line <- NULL
      output$load_stepone <- NULL
      output$load_steptwo <- NULL
      output$tablegeo_rna <- renderTable({
        values$pdata %>% select(input$columns) %>% dplyr::rename_with(function(x) "RNA data") %>%  filter_at(1, all_vars(grepl(input$keyword_rna, .)))
      })
    }else{
      output$tablegeo_rna <- NULL
    }
  })
  
  # Filtering of hto files for GEO database  
  observeEvent(c(input$columns, input$keyword_hto, input$include_hto), {
    if (input$columns!="na" & input$include_hto) {
      output$selected_var <- NULL
      output$load_stepone <- NULL
      output$load_line <- NULL
      output$load_steptwo <- NULL
      output$tablegeo_hto <- renderTable({
        values$pdata %>% select(input$columns) %>% dplyr::rename_with(function(x) "HTO data") %>%  filter_at(1, all_vars(grepl(input$keyword_hto, .)))
      })
    }else if(input$columns!="na" & !input$include_hto){
      output$selected_var <- NULL
      output$tablegeo_hto <- NULL
      output$load_line <- NULL
      output$load_stepone <- NULL
      output$load_steptwo <- NULL
      output$htoregex <- NULL
    }else{
      output$tablegeo_hto <- NULL
    }
  })
  
  # Button to signal the end of data selection in GEO
  observeEvent(input$geodone, {
    output$tablegeo_protein <- NULL
    output$tablegeo_rna <- NULL
    output$load_line <- NULL
    output$tablegeo_hto <- NULL
    output$selected_var <- renderUI({includeMarkdown("../docu/geo-load.md")})
    if(input$columns!="na"){
      output$load_line = renderUI(hr())
      output$load_stepone = renderUI({
        tagList(
          h4("Load data"),
          includeMarkdown("../docu/geo-load2.md"),
          selectInput("download_one_file", "pick one", values$pdata %>% select(input$columns) %>%
                        filter_at(1, all_vars(grepl(regfilter(input$keyword_protein, input$keyword_rna,input$keyword_hto), .))) %>% pull(input$columns)
                      , selected = "na"),
          actionButton("geodownloadone", "download example", width = "40%")
        )
      })
      output$load_steptwo = renderUI({
        tagList(
          checkboxInput("separate_protocols", "separate protocols for ADT and RNA data", value = FALSE),
          loaddata("load_protein"),
          loaddata("load_rna"),
          loaddata("load_hto")
        )
      })
    }
  })
  
  observeEvent(input$separate_protocols, {
    if(input$separate_protocols & input$include_hto){
      loaddataServer("load_protein", on=T, typ="protein")
      loaddataServer("load_rna", on=T, typ="rna")
      loaddataServer("load_hto", on=T, typ="hto")
    }else if(input$separate_protocols & !input$include_hto){
      loaddataServer("load_protein", on=T, typ="protein")
      loaddataServer("load_rna", on=T, typ="rna")
      loaddataServer("load_hto", on=F, typ="hto")
    }else{
      loaddataServer("load_protein", on=T, typ="protein")
      loaddataServer("load_rna", on=F, typ="rna")
      loaddataServer("load_hto", on=F, typ="hto")
    }
  })
  
  observeEvent(c(input[["load_protein-class"]], input[["load_rna-class"]], input[["load_hto-class"]]), {
    askextraclassServer('load_protein-extra_class', class=input[["load_protein-class"]], typ="protein")
    askextraclassServer('load_rna-extra_class', class=input[["load_rna-class"]], typ="rna")
    askextraclassServer('load_hto-extra_class', class=input[["load_hto-class"]], typ="hto")
  })
  
  # Basic bottom documentation
  observeEvent(input$dataset,
               {
                 if (input$dataset=="na") {
                   output$selected_var <- renderUI({includeMarkdown("../docu/entry-page.md")})
                 }else if(input$dataset=="impossible"){
                   output$selected_var <- renderUI({includeMarkdown("../docu/manual-download.md")})
                   addDownloadlinkServer("Impossiblefiles", values, withoutlink=T)
                 }else if(input$dataset=="geo"){
                   output$selected_var <- renderUI({includeMarkdown("../docu/geo-download.md")})
                   addDownloadlinkServer("GEOproteindata", values)
                   addDownloadlinkServer("GEOrnadata", values)
                   addDownloadlinkServer("GEOhtodata", values)
                   addDownloadlinkServer("GEOmetadata", values)
                 }else{
                   output$selected_var <- NULL
                 }
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
