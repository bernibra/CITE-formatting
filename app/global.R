library(shiny)
library(shinyalert)
library(bslib)
library(GEOquery)
library(markdown)
library(dplyr)
source("functions.R")

options(shiny.port = 3838, shiny.host = '0.0.0.0')

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
       padding: 10px 0px 0px;
       text-align: center;
       font-weight: bold;
       font-size: 100%;
       color: #000000;
       background-color: #C5DCD2;
       z-index: 105;
   }
   .container{
       width: 95%;
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
   
   @media all and (max-width: 1200px) and (min-width: 900px) {
     .container{
         width: 80%;
     }
   }
   @media all and (min-width: 1200px) {
     .container{
         width: 65%;
     }
   }
   
"