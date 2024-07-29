
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(appCSS),
  bsplus::use_bs_tooltip(),
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
          h2("Prepare CITE-seq dataset")
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
             fluidRow(
               uiOutput('xtra_line'),
               column(12, uiOutput('xtra_stepone'))
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
        fluidRow(
          column(12, 
                 div(class="scrollable", tableOutput("tablextra")),
          )
        ),
      )
    )
  )
)
