library(shiny)
library(shinyjs)
library(shinyalert)
library(bslib)

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
       background-color: #D9D9D9;
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
       background: #D9D9D9;
       }
   #header {
       background: #F7F7F7;
       border-bottom: 1px solid #bdbdbd;
       padding: 15px 15px 20px;
       font-family: 'Menlo', sans-serif;
   }
   .loaddata {
       margin: 0px 0px 5px 0px;
       background: #F4F1F2;
       border: 0.5px solid #DDDCDC;
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
       background-color: #C5DCD2;
       z-index: 105;
   }
   .container{
       width: 80%;
       border-radius: 5px;
       border: 1px;
       background-color: #F7F7F7;
       position: relative;
       box-shadow: 3px 3px 3px #737373;
       margin-top: 30px;
       padding-left: 40px;
       padding-right: 40px;
       padding-top: 10px;
       padding-bottom: 10px;
       margin-bottom: 20px;
       font-family: 'Menlo', sans-serif;
   }
   hr{
       border-bottom: 1px solid #bdbdbd;
   }
"
theme <- bs_theme(
  # Controls the default grayscale palette
  bg = "#FFF7FB", fg = "black",
  # Controls the accent (e.g., hyperlink, button, etc) colors
  primary = "#C9AFC4", secondary = " #C5DCD2",
  base_font = "Helvetica Neue",
  code_font = "Helvetica Neue",
  heading_font = "Helvetica Neue",
  # Can also add lower-level customization
  "input-border-color" = "#C9AFC4"
)
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  # theme = theme,
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
