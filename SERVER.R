mydata <- read.csv("~/Documents/newdir/R file.csv")
library(ggplot2)
server <- function(input, output) {
  
  ##Summary
  output$summary <- renderText({
    br()
    br()
    
    paste("Directions: 1. Select the house built in year  and click Sale Vs.Built year to see the plot",
         
          "2. Select the Quality required and click Sale Vs. Quality to see the plot",
        
          "3. Select the bedrooms required  and click Sale Vs. Bedroom to see the plot",
          
          "4. Select the Living area required  and click Sale Vs. Living Area to see the plot"
          
          
          )
    
  })
  
  ## data str
  output$plot1 <- renderPlot({
    colm<- as.numeric(input$var)
    qplot(mydata$SalePrice)
    qplot(SalePrice, data=mydata, geom="density", fill=Bldg.Type, alpha=I(.5), 
          main="Distribution of SalePrice by Bldg.Type", xlab="SalePrice", 
          ylab="Density")
  })
  
   ##Reactive
  #yr_reactive<-reactive({
  #  as.numeric(input$Year)
    
#  })
  
  
   ##plot
output$coolplot1 <-renderPlot(
   {
  temp_datasubset<-subset(mydata,mydata$Year.Built==input$Year)
   plot(temp_datasubset$SalePrice,temp_datasubset$Year.Built, main="Scatterplot ", xlab="SalePrice ", ylab="Year.Built")
   
}
)
  
#  output$coolplot1 <-renderPlot(
#  {
#  temp_datasubset<-subset(mydata,mydata$Year.Built==input$Year)
  #   plot(temp_datasubset$SalePrice,temp_datasubset$Year.Built, main="Scatterplot ", xlab="SalePrice ", ylab="Year.Built")
  
#  ggplot(mydata, aes(x = temp_datasubset$Year.Built, y = temp_datasubset$SalePrice, fill = Overall.Qual)) + geom_boxplot() +
#    facet_wrap(~ Overall.Qual, ncol = 10)
  
 # }
  #)
  
  
  
  
  
  
  output$coolplot2 <-renderPlot(
    {
      temp_datasubset<-subset(mydata,mydata$Overall.Qual==input$quality)
      plot(temp_datasubset$SalePrice,temp_datasubset$Overall.Qual, main="Scatterplot ", xlab="sales price ", ylab="Quality")
      

    }
  )
  
  output$coolplot3 <-renderPlot(
    {
      temp_datasubset<-subset(mydata,mydata$Bedroom.AbvGr==input$Beds)
      plot(temp_datasubset$SalePrice,temp_datasubset$Bedroom.AbvGr, main="Scatterplot ", xlab="sales price ", ylab="Number of bedrooms")
      
      
    }
  )
  
  output$coolplot4 <-renderPlot(
    {
      temp_datasubset<-subset(mydata,mydata$Gr.Liv.Area==input$LivingArea)
      plot(temp_datasubset$SalePrice,temp_datasubset$Gr.Liv.Area, main="Scatterplot ", xlab="sales price ", ylab="Living area")
      
      
    }
  )
  
  
  
}
  
  
 ##----- 
  
  
  
  
  
  
  
  
