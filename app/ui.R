library(shiny)
library(shinyjs)
library(shinyalert)
# library(DT)

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
  "
   html{
       background-color: #bdbdbd;
   }
   .mandatory_star { 
       color: red;
       }
   .shiny-input-container { 
       margin-top: 25px;
       }
   #submit_msg { 
       margin-left: 15px; 
       }
   #error {
       color: red;
       }
   body {
       background: #bdbdbd;
       }
   #header {
       background: #f0f0f0;
       border-bottom: 1px solid #ddd;
       padding: 15px 15px 10px;
       }
   .loaddata {
       margin: 1px 1px 5px 1px;
       background: #ffffff;
       border: 0.5px solid #d9d9d9;
       padding: 10px 10px 5px;
       font-size: 100%;
       }
   .loaddata h4 {
       font-size: 100%;
       font-weight: bold;
       }
   .loadmessage {
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
   .container{
       width: 80%;
       border-radius: 5px;
       border: 1px;
       background-color: #f0f0f0;
       position: relative;
       box-shadow: 2px 2px 2px #737373;
       margin-top: 30px;
       padding-left: 30px;
       padding-right: 30px;
       padding-top: 5px;
       padding-bottom: 5px;
       margin-bottom: 20px;
    }
"

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(appCSS),
  title = "Prepare CITE-seq dataset",
  shiny::div(class="loadmessage",p("Loading...")),
  shiny::tags$script(sprintf(
    "	setInterval(function(){
  		 	 if ($('html').hasClass('shiny-busy')) {
  		    setTimeout(function() {
  		      if ($('html').hasClass('shiny-busy')) {
  		        $('div.loadmessage').show()
  		      }
  		    }, %d)  		    
  		  } else {
  		    $('div.loadmessage').hide()
  		  }
  		},100)
  		",500)),
  div(class="container",
    div(id = "header",
        fluidRow(
          h2("Prepare CITE-seq dataset"),
          ),
        fluidRow(
          h4("An app for adding new datasets to ",
                    a(href = "https://github.com/bernibra/CITE-wrangling",
                      "CITE-wrangling")
                 ),
        ),
    ),
    fluidRow(
      br(),
      column(12, 
        h4("Metadata"),
        fluidRow(
          column(6,
            textInput("doi", labelMandatory("doi of the experiment"), placeholder = "doi.org/10.1038/s41591-021-01329-2"),
            textInput("alias", labelMandatory("a unique alias"), placeholder ="Stephenson2021"),
            radioButtons('dataset', 
                        label = labelMandatory('choose Dataset'), 
                        choices = c("nothing selected"="na",'GEO'="geo", "direct download"="wget", "manual download"="impossible", "ArrayExpress"="array"),
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
    ),
    fluidRow(
      column(12,
        fluidRow(
          column(6,
            uiOutput('download_steptwo')
          ),
          column(6,
            br(),
            uiOutput('download_stepthree'),
            uiOutput('download_stepfour')
          ),
        )
      ),
    ),
    fluidRow(
      column(12,
             fluidRow(
               uiOutput("load_line"),
               column(6,
                      uiOutput('load_stepone')
               ),
               column(6,
                      uiOutput('load_steptwo')
               ),
             )
      ),
    ),
    fluidRow(
      column(12, 
        uiOutput("selected_var"),
        uiOutput("main"),
        br(),
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
)
