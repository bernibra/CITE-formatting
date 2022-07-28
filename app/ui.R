library(shiny)

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
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
        column(9,
          h1("Prepare CITE-seq dataset"),
        ),
        column(3, align="center",
               downloadButton('downloadData', 'Download')
        )),
        fluidRow(
          column(12,
          h4("This is an app designed to help adding new datasets to the",
             a(href = "https://github.com/bernibra/CITE-wrangling",
               "CITE-wrangling pipeline")
          ))
        ),
  ),
  # Sidebar panel for inputs ----
  fluidRow(
    column(12, 
      fluidRow(
       column(6,
        
        # numericInput("lambda1", label = "lambda1", value = 3),
        # numericInput("lambda2", label = "lambda2", value = 5),
        # numericInput("n", label = "n", value = 1e4, min = 0),
        # actionButton("simulate", "Simulate!")
        
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
           uiOutput('steptwo')
    ),
    column(12,
           uiOutput('stepthree')
    ),
    # Main panel for displaying outputs ----
    # Output: Histogram ----
    # plotOutput("hist")
    column(12, 
           textOutput("selected_var")
           ),

  )
)
