
library(shiny)
library(ggplot2)



mydata <- read.csv("~/Documents/newdir/R file.csv")

## mydata <- read.csv("~/Documents/newdir/R file.csv")

ui <- fluidPage(     
  #title
  titlePanel(title = h1("HouseKey - Houses at Best Price!", align="center")),
  
  ##
  sidebarLayout(
    sidebarPanel(
     sliderInput("quality",
                 "Overallquality",
                min = 1,
               max = 10,
              value = 3),
     sliderInput("Year","Select the year built",1,150,50),
    sliderInput("Beds","Number of Bedrooms",1,8,2),
     sliderInput("LivingArea","Living Area",1000,2000,50),
      #selectInput("Airconditioninginput","Cebtral Air-Conditioning",choices = c("Yes","No")),
      
      selectInput("var","Selct data table column",choices=c("Bldg.Type" = 3,"Overall.Qual" = 4,"Bedroom.AbvGr" = 11,"Living Area"=10))
      
      #      radioButtons("plottype","Plots - Distribution of SalePrice by",choices = c("SalePrice by Bldg.Type", "SalePrice by Overall.Qual", "SalePrice by Bedroom.AbvGr"),selected = "Bedroom.AbvGr")
    ),
    
    mainPanel(
      tabsetPanel(type="tab",
                  tabPanel("Summary",verbatimTextOutput("summary")),
                  tabPanel("Density_Plot",plotOutput("plot1")),
                  tabPanel("Sales_YearBuilt",plotOutput("coolplot1")),
                  tabPanel("Sales_Quality",plotOutput("coolplot2")),
                  tabPanel("Sales_Bedrooms",plotOutput("coolplot3")),
                  tabPanel("Sales_LivingArea",plotOutput("coolplot4"))
                  
                  )
      
  ))
  
  )
    
    
    