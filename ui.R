#
#library(ggridges)
library(ggridges)
library(nflfastR)
library(tidyverse)
library(shinydashboard)
library(shinythemes)
library(bslib)
library(reactable)
library(reactablefmtr)
library(tidyverse)
library(dplyr)
library(gtExtras)
library(rsconnect)
library(gtable)
library(gt)
library(magick)
library(ggplot2)
library(ggimage)
library(shiny)

# Define UI for application that draws a histogram
fluidPage(

  theme = shinytheme("cyborg"),
  
  navbarPage("FATRIX",
      navbarMenu("Player",
                 tabPanel("WAR",
                          column(1,
                            sliderInput("years", label = h6("Year"), min = 18, 
                                  max = 22, value = c(21, 22)),
                          ),
                          column(1,
                                 textInput("qbs", label = h6("# QBs"), value = "1", width = 50),
                          ),
                          column(2,
                             textInput("wrs", label = h6("# WRs"), value = "2", width = 50),
                          ),
                          column(2,
                                 textInput("rbs", label = h6("# RBs"), value = "2", width = 50),
                          ),
                          column(2,
                                 textInput("flex", label = h6("# FLEX"), value = "2", width = 60),
                          ),
                          column(2,
                                 textInput("min", label = h6("Min Games"), value = "3", width = 90),
                          ),
                          column(2,
                                 radioButtons("ppr", label = h6("PPR"),
                                              choices = list("None" = 1, "Half" = .5, "Full" = 0), 
                                              selected = 1),                          ),
                          column(2,
                                 actionButton("update", label = "UPDATE"),
                          ),
                          
                          br(),
                          br(),
                          print("Please Input the number of positional players your league requires you to start as well as PPR type and the minimum number of games played you want to filter players by. The visual may take some time to update, especially if the date range is large!"),
                          
                          br(),
                          br(),
                          reactableOutput("wartab"),
                 ),
                 tabPanel("FPOE")
                 ),#navbarmenu,
      tabPanel("Trade Tool")
      
             
  )#navbar
)
