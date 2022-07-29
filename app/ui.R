library(shiny)
library(shinyjs)
library(DT)

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

tabTable <- function(id) {
  ns <- shiny::NS(id)
  tabPanel(
    "Table",
    DTOutput(ns("table"))
  )
}

# CSS to use in the app
appCSS <-
  ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(appCSS),
  title = "Prepare CITE-seq dataset",
  div(id = "header",
      fluidRow(
        column(12,
          h1("Prepare CITE-seq dataset"),
        )),
        fluidRow(
          column(9,
          h4("An app designed to help adding new datasets to ",
             a(href = "https://github.com/bernibra/CITE-wrangling",
               "CITE-wrangling")
          )),
          column(3, align="center",
                 downloadButton('downloadData', 'Download')
          )
        ),
  ),
  tags$head(tags$style(type="text/css", "
             #loadmessage {
               position: fixed;
               top: 0px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: #fde0dd;
               z-index: 105;
             }
          ")),

  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                   tags$div("Loading...",id="loadmessage")),
  fluidRow(
    column(12, 
      h4("Metadata"),
      fluidRow(
        column(6,
        
          textInput("doi", labelMandatory("doi of the experiment"), placeholder = "doi.org/10.1038/s41591-021-01329-2"),
          textInput("alias", labelMandatory("a unique alias"), placeholder ="Stephenson2021"),
          radioButtons('dataset', 
                      label = labelMandatory('choose Dataset'), 
                      choices = c("nothing selected"="na",'GEO'="geo", "ArrayExpress"="array", "direct download"="wget", "manual download"="impossible"),
                      selected = "na"),
        ),
        column(6,
          textInput("species", "species", placeholder = "Homo sapiens"),
          textInput("tissue", "tissue", placeholder = "PBMC"),
          textInput("genome_build", "genome build:", placeholder = "GRCh38"),
          textAreaInput("description", "experiment summary"),
        ),
      ),
      hr()
    ),
    column(12,
      fluidRow(
        column(6,
          uiOutput('download_steptwo')
        ),
        column(6,
          fluidRow(
            br(),
            uiOutput('download_stepthree'),
            uiOutput('download_stepfour')
          )
        ),
      )
    ),
    column(12,
           fluidRow(
             column(6,
                    uiOutput('load_stepone')
             ),
             column(6,
                    uiOutput('load_steptwo')
             ),
           )
    ),
    column(12, 
      uiOutput("selected_var"),
      uiOutput("main"),
      br(),
      hr(),
      br(),
      fluidRow(
        column(4, 
          tableOutput("tablegeo_protein")
        ),
        column(4, 
          tableOutput("tablegeo_rna")
        ),
        column(4,
          tableOutput("tablegeo_hto")
        )
      ),
    )
  )
)
