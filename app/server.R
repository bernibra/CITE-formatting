
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
            textInput(ns("h5key"),  placeholder = paste(typ, "experiment key", sep=" "),tags$span(style="font-weight: normal;","h5 experiment key")),
            textInput(ns("drop"),  placeholder = "pattern used to drop rows", label = tags$span(style="font-weight: normal;","columns to drop")),
            textInput(ns("keep"),  placeholder = "pattern used to keep rows", label = tags$span(style="font-weight: normal;","columns to keep"))
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
            textInput(ns("replace"),  placeholder = "matrix file name", label = tags$span(style="font-weight: normal;","matrix name")),
            numericInput(ns("column"), label = tags$span(style="font-weight: normal;","column used in the features file"),   min = 1,value=1),
            textInput(ns("drop"),  placeholder = "pattern used to drop rows", label = tags$span(style="font-weight: normal;","columns to drop")),
            textInput(ns("keep"),  placeholder = "pattern used to keep rows", label = tags$span(style="font-weight: normal;","columns to keep"))
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
            checkboxInput(ns("separate_samples"), "merge different files together", value = TRUE),
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
    tagList(div(class="loaddata",
      p(style="font-weight: bold;", labelMandatory(paste(name, "files", sep=" "))),
      actionButton(ns("show"), "add data"),
      br(),
      br(),
      p("added files:"),    
      verbatimTextOutput(ns("dataInfo")))
    )
  }else{
    tagList(div(class="loaddata",
      p(style="font-weight: bold;", paste(name, "files", sep=" ")),
      actionButton(ns("show"), "add data"),
      br(),
      br(),
      p("added files:"),    
      verbatimTextOutput(ns("dataInfo")))
    )
  }
}

dataModal <- function(ns, failed = FALSE, withoutlink=F) {
  # Popup for download links addition
  
  if(withoutlink){
    modalDialog(
      
      textInput(ns("file"), labelMandatory("file name"),
                placeholder = 'e.g. file.csv'
      ),
      span('make sure the file name has the right extension'),
      
      if (failed)
        div(tags$b("Invalid name file", style = "color: red;")),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("ok"), "OK")
      )
    )
  }else{
    modalDialog(
      
      textInput(ns("file"), labelMandatory("file name"),
                placeholder = 'e.g. file.csv'
      ),
      span('make sure the file name has the right extension'),
      
      textInput(ns("url"), labelMandatory("url"),
                placeholder = NULL
      ),
      span('make sure the url is public and working'),
      
      textInput(ns("group"), "grouping level",
                placeholder = "e.g. Mouse"
      ),
      span('optional field if you want to group different files'),
      
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
          values[[ns("group")]] <- rbind(values[[ns("group")]], input$group)
          
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
server <- function(input, output, session) {
  
  # Values to save across elements
  values<-reactiveValues()
    
  # First reactive selection (what type of entry is it? GEO, ArrayExpress, direct download,...)
  output$download_steptwo = renderUI({
    # print(input)
    
    condition <- input$doi!="" & input$alias!=""
    if(!condition | input$dataset=="na"){

    }else if(input$dataset=="impossible"){
      tagList(
        h4("Download"),
        textInput("id", labelMandatory("unique id"), placeholder = "e.g. Buus2021"),
        textInput("source", "data source (url link)"),
        textAreaInput("comment", "comment on how to download", paste0("... and store the adt data in `./supp_protein/",input$alias,
                                                                    "/`, the RNA data in `./supp_rna/",input$alias,
                                                                    "/`, the HTO data in `./supp_hto/",input$alias,
                                                                    "/`, and the metadata in `./metadata/`"), width = "100%"),
        br(),
        addDownloadlink("Impossiblefiles", name="expected files"))
    }else if(input$dataset=="geo"){
      tagList(
              h4("Download data"),
              textInput("id", labelMandatory("GEO id"), placeholder = "e.g. GSE139369"),
              radioButtons('geodownload', 
                           label=labelMandatory("download type"),
                           choices = c("Nothing selected"="na",'download via GEOquery'="geo", "direct download"="wget"),
                           selected = "na"),
              br(),
              addDownloadlink("GEOmetadata", name="metadata")
             )
    }else if(input$dataset=="wget"){
      tagList(
        h4("Download"),
        textInput("id", labelMandatory("unique id"), placeholder = "e.g. Buus2021"),
        br(),
        addDownloadlink("GEOmetadata", name="metadata"))
    }else{
      tagList(
        h4("Download data"),
        textInput("id", labelMandatory("ArrayExpress id"), placeholder = "e.g. MTAB-9357"),
        radioButtons('arraydownload', 
                     label=labelMandatory("download type"),
                     choices = c("Nothing selected"="na",'download via ArrayExpress'="array", "direct download"="wget"),
                     selected = "na"),
        br(),
        addDownloadlink("GEOmetadata", name="metadata")
      )
    }
  })

  # GEO Load metadata
  output$download_stepthree = renderUI({
    arraydownload <- ifelse(is.null(input$arraydownload), "other", input$arraydownload)
    geodownload <- ifelse(is.null(input$geodownload), "other", input$geodownload)
    
    id <- ifelse(is.null(input$id), "", input$id)
    condition <- input$doi!="" & input$alias!="" & input$dataset!="na" & id!="" & geodownload!="na" & arraydownload!="na"
    
    if (condition){
        if(input$dataset=="impossible"){
          actionButton("geo_download_button_4", "loading info")
        }else if(geodownload!="geo" & arraydownload!="array"){
          actionButton("geo_download_button_2", "loading info")
        }else if(geodownload=="geo"){ 
          actionButton("geo_download_button", "load GEO")
        }else if(arraydownload=="array"){ 
          actionButton("geo_download_button_3", "loading info")
        }else{
          NULL
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
          actionButton("geodone", "done")
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
        checkboxInput("include_hto", "Include HTO data", value = FALSE),
        p("One needs to explicitely check the HTO box if HTO data needs to be processed (ingnoring HTO information otherwise)."),
        addDownloadlink("GEOproteindata", name="protein", mandatory=T),
        addDownloadlink("GEOrnadata", name="RNA"),
        addDownloadlink("GEOhtodata", name="HTO"),
        br(),
        actionButton("geodone", "done")
        )
    })
  })
  
  
  # Select description and keyword from metadata
  observeEvent(input$geo_download_button_3, {
    shinyjs::disable("geodownload")
    shinyjs::disable("dataset")
    shinyjs::disable("alias")
    shinyjs::disable("doi")
    shinyjs::disable("id")
    output$download_stepfour = renderUI({
      tagList(
        checkboxInput("include_hto", "Include HTO data", value = TRUE),
        p("ArrayExpress will download all files associated to a dataset. To filter out these files, we need to use regexp to decide which ones to keep."),
        textInput("keyword_protein_array", placeholder = "regex for ADT files", label = NULL),
        textInput("keyword_rna_array", placeholder = "regex for RNA files", label = NULL),
        textInput("keyword_hto_array", placeholder = "regex for HTO files", label = NULL),
        actionButton("geodone", "done")
      )
    })
  
  })
  
  observeEvent(input$geo_download_button_4, {
    shinyjs::disable("geodownload")
    shinyjs::disable("dataset")
    shinyjs::disable("alias")
    shinyjs::disable("doi")
    shinyjs::disable("id")
    output$download_stepfour = renderUI({
      tagList(
        br(),
        p("The only thing to consider here is whether or not there is HTO data"),
        checkboxInput("include_hto", "Include HTO data", value = TRUE),
        actionButton("geodone", "done")
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
  
  
  # Button to signal the end of data selection in GEO
  observeEvent(input$geodone, {
    arraydownload <- ifelse(is.null(input$arraydownload), "other", input$arraydownload)
    geodownload <- ifelse(is.null(input$geodownload), "other", input$geodownload)
    output$Nothinguploaded <- NULL
    output$tablegeo_protein <- NULL
    output$tablegeo_rna <- NULL
    output$load_line <- NULL
    output$tablegeo_hto <- NULL
    condition <- ifelse(is.null(input$columns), "na", input$columns)
    if(geodownload!="geo" & arraydownload!="array" & input$dataset!="impossible" & is.null(values[["GEOproteindata-data"]])){
      shinyalert("Oops!", "You forgot to upload a file...", type = "error")
    }else if(condition=="na" & geodownload=="geo"){
      shinyalert("Oops!", "You forgot to pick a column...", type = "error")
    }else if(condition!="na"){
      output$load_line = renderUI(hr())
      shinyjs::disable("download_stepthree")
      shinyjs::disable("download_stepfour")
      output$selected_var <- renderUI({includeMarkdown("docu/geo-load.md")})
      output$load_stepone = renderUI({
        tagList(
          h4("Load data"),
          includeMarkdown("docu/geo-load-2.md"),
          selectInput("download_one_file", "pick one", values$pdata %>% select(input$columns) %>%
                        filter_at(1, all_vars(grepl(regfilter(input$keyword_protein, input$keyword_rna,input$keyword_hto), .))) %>% pull(input$columns)),
          actionButton("geodownloadone", "download example"),
          textOutput("downloadmessage")
        )
      })
      output$load_steptwo = renderUI({
        tagList(
          checkboxInput("separate_protocols", "separate protocols for ADT and RNA data", value = FALSE),
          loaddata("load_protein"),
          loaddata("load_rna"),
          loaddata("load_hto"),
          br(),
          actionButton("sampleinformation", "sample info")
        )
      })
    }else{
      shinyjs::disable("download_stepthree")
      shinyjs::disable("download_stepfour")
      output$load_line = renderUI(hr())
      output$load_stepone = renderUI({
        tagList(
          h4("Load data"),
          includeMarkdown("docu/geo-load-3.md"),
          output$selected_var <- NULL
        )
      })
      output$load_steptwo = renderUI({
        tagList(
          checkboxInput("separate_protocols", "separate protocols for ADT and RNA data", value = FALSE),
          loaddata("load_protein"),
          loaddata("load_rna"),
          loaddata("load_hto"),
          br(),
          actionButton("sampleinformation", "sample info")
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
  
  # Load sample information
  observeEvent(input$sampleinformation, {
    shinyjs::disable("load_steptwo")
    output$load_stepone = renderUI({
      tagList(
        h4("Sample information"),
        radioButtons('sampleoption', 
                     label=tags$span(style="font-weight: normal;","Adding sample information can be done in different ways:"),
                     choices = c("No sample information"="na",
                                 'if the sample information is already one of the columns, set as colData (see on the left), one can simply define the column name.'="sampleid",
                                 "if the sample information is found in an extermal file, the pipeline will assume that there are one or more columns that define the sample id. At first, it will look for the data in the metadata folder; if nothing is found, it will look in the folder of the experiment.
"="extfile",
                                 "if samples are separated by file, but there are different types of files (different tissues or experiments) that need to be separately considered, one can define groups of files that will be considered separately."="sample_group"),
                     selected = "na"),
        uiOutput("additional_samples"),
        fluidRow(
          column(6,
                 br(),
                  downloadButton('downloadData', 'Download', width = "60%")
          ),
          # column(6,
          #         actionButton("reset_button", "Reset", width = "60%")
          # ),
        )
      )
    })
  })

  observeEvent(input$sampleoption,{
    if(input$sampleoption=="na"){
      shinyjs::enable("downloadData")
      output$additional_samples <- NULL
    }else if(input$sampleoption=="sampleid"){
      output$additional_samples <- renderUI({
        textInput("sampleid", label=labelMandatory(tags$span(style="font-weight: normal;","column name")), placeholder = "column in colData")
      })
      shinyjs::disable("downloadData")
    }else if(input$sampleoption=="extfile"){
      output$additional_samples <- renderUI({
        tagList(
          textInput("samplefile", label=labelMandatory(tags$span(style="font-weight: normal;","regex for file in metadata"))),
          textInput("samplekey", label=tags$span(style="font-weight: normal;","column indicating the cell name"), placeholder = "row names as default"),
          textInput("samplevalue", label=labelMandatory(tags$span(style="font-weight: normal;","column/s indicating the sample id")), placeholder = "if multiple, separated by ;")
        )
      })
      shinyjs::disable("downloadData")
    }else{
      output$additional_samples <- renderUI({
        textInput("samplegroups", label=labelMandatory(tags$span(style="font-weight: normal;","regex for file groups")), placeholder = "separated by ;")
      })
      shinyjs::disable("downloadData")
    }
  })
  

  # enable dowload button
  observeEvent(c(input$sampleid, input$samplefile, input$samplevalue, input$samplegroups),{
    sampleid <- ifelse(is.null(input$sampleid), "", input$sampleid)
    samplefile <- ifelse(is.null(input$samplefile), "", input$samplefile)
    samplevalue <- ifelse(is.null(input$samplevalue), "", input$samplevalue)
    samplegroups <- ifelse(is.null(input$samplegroups), "", input$samplegroups)
    if(input$sampleoption=="sampleid" & sampleid!=""){
      shinyjs::enable("downloadData")
    }
    if(input$sampleoption=="extfile" & samplefile!="" & samplevalue!=""){
      shinyjs::enable("downloadData")
    }
    if(input$sampleoption=="sample_group" & samplegroups!=""){
      shinyjs::enable("downloadData")
    }
  })
  
  # Basic bottom documentation
  observeEvent(input$dataset,
               {
                 if (input$dataset=="na") {
                   output$selected_var <- renderUI({includeMarkdown("docu/entry-page.md")})
                 }else if(input$dataset=="impossible"){
                   output$selected_var <- renderUI({includeMarkdown("docu/manual-download.md")})
                   addDownloadlinkServer("Impossiblefiles", values, withoutlink=T)
                 }else if(input$dataset=="geo"){
                   output$selected_var <- renderUI({includeMarkdown("docu/geo-download.md")})
                   addDownloadlinkServer("GEOproteindata", values)
                   addDownloadlinkServer("GEOrnadata", values)
                   addDownloadlinkServer("GEOhtodata", values)
                   addDownloadlinkServer("GEOmetadata", values)
                 }else if(input$dataset=="wget"){
                   output$selected_var <- renderUI({includeMarkdown("docu/wget-download.md")})
                   addDownloadlinkServer("GEOproteindata", values)
                   addDownloadlinkServer("GEOrnadata", values)
                   addDownloadlinkServer("GEOhtodata", values)
                   addDownloadlinkServer("GEOmetadata", values)
                 }else{
                   output$selected_var <- renderUI({includeMarkdown("docu/geo-download.md")})
                   addDownloadlinkServer("GEOproteindata", values)
                   addDownloadlinkServer("GEOrnadata", values)
                   addDownloadlinkServer("GEOhtodata", values)
                   addDownloadlinkServer("GEOmetadata", values)
                 }
               })
  
  # Filtering of hto files for GEO database  
  observeEvent(c(input$columns, input$keyword_hto, input$include_hto), {
    condition <- ifelse(is.null(input$columns), "na", input$columns)
    if(input$include_hto){
      shinyjs::enable("keyword_hto")
    }else{
      shinyjs::disable("keyword_hto")
    }
    
    if (condition!="na" & input$include_hto) {
      output$selected_var <- NULL
      output$load_stepone <- NULL
      output$load_line <- NULL
      output$load_steptwo <- NULL
      output$tablegeo_hto <- renderTable({
        values$pdata %>% select(input$columns) %>% dplyr::rename_with(function(x) "HTO data") %>%  filter_at(1, all_vars(grepl(input$keyword_hto, .)))
      })
    }else if(condition!="na" & !input$include_hto){
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
  
  observeEvent(input$geodownloadone,{
      accession <- values$pdata$geo_accession[values$pdata %>% pull(input$columns) == input$download_one_file]
      if(length(accession)>1){
        accession <- accession[1]
      }
      GEOquery::getGEOSuppFiles(accession, baseDir = "../data/")
      output$downloadmessage <- renderText({
        "stored in './data/'"
      })
      }
  )
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$id, '.yaml', sep='')
    },
    content = function(con) {
      
      if (ifelse(is.null(input$geodownload), FALSE, input$geodownload=="geo")){
        output$xtra_line = renderUI({
          hr()
        })
        output$xtra_stepone = renderUI({
          tagList(
            h4("Extra metadata"),
            p("Extract any relevant columns from the GEO metadata"),
            varSelectInput("xtra_columns", "columns", data = values$pdata, multiple = TRUE),
            downloadButton('downloadExtra', 'Download', width = "60%"),
            br()
          )
        })
        output$selected_var <- NULL
      }
        
      data <- list(download=list(setup=ifelse__(input$dataset=="geo", "geo", NULL),
                                 download=ifelse__(input$dataset=="geo" | input$dataset=="array", 
                                                   ifelse__(input$dataset=="geo", input$geodownload, input$arraydownload),
                                                   input$dataset),
                                 id = input$id,
                                 description = input$columns,
                                 keyword = makelist(input),
                                 ignore_hto = ifelse__(is.null(input$include_hto), NULL, !input$include_hto),
                                 fname = makelist_4(values, input, type = "data"),
                                 wlink = makelist_4(values, input, type = "url"),
                                 fgroup = makelist_4(values, input, type = "group"),
                                 source = ifelse__(is.null(input$source), NULL, input$source),
                                 comments = ifelse__(is.null(input$comment), NULL, input$comment)
                                 ),
                  load=makelist_3(input),
                  metadata=list(doi=input$doi,
                        description=ifelse_(input$description,NULL, gsub("\r?\n|\r", " ", input$description)),
                        tissue=ifelse_(input$tissue,NULL, input$tissue),
                        species=ifelse_(input$species,NULL, input$species),
                        alias=input$alias,
                        genome_build=ifelse_(input$genome_build,NULL, input$genome_build))
      )
      yaml::write_yaml(data, con)
    }
  )
  
  observeEvent(input$xtra_columns, {
    if(length(input$xtra_columns)==0){
      shinyjs::disable("downloadExtra")
    }else{
      shinyjs::enable("downloadExtra")
      columns <- input$xtra_columns
      output$tablextra <- renderTable({
        values$pdata %>% select(!!!input$xtra_columns)
      })
    }
  })
  
  output$downloadExtra <- downloadHandler(
    filename = function() {
      paste(input$id, '.csv', sep='')
    },
    content = function(con) {
      xtra_data <- values$pdata %>% select(!!!input$xtra_columns)
      print(xtra_data)
      write.table(xtra_data, con)
    }
  )
  
  session$onSessionEnded(function() {
    stopApp()
  })
  # observeEvent(input$reset_button, {
  #   session$reload()
  #   })
}


# rsconnect::deployApp(appDir = ".", appName = "cite-formatting", appFiles = c( "global.R", "server.R", "ui.R", "functions.R", paste0("./docu/", list.files("./docu/"))))
