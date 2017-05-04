#mortgage calculator shiny app 
#author: Gwendolyn Reynolds
#date: May 4, 2017

library(shiny)
library(expm)
library(ggplot2)
library(RColorBrewer)
library(plotly)
library(tibble)
library(plyr)
library(dplyr)
library(tidyr)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("cosmo"),
  headerPanel("Mortgage Monthly Payment Calculator"),
  sidebarPanel(
    
    numericInput(inputId = "amount",
               label = "Mortgage Amount $", 
              value = 100000, min = 0, max = 5000000, step = 50000),
   sliderInput(inputId = "interest", 
              label = "Interest Rate", 
              value = 4, min = 1, max = 6, step = .25),
   sliderInput(inputId = "years", 
              label = "Loan Period (years)", 
              value = 30, min = 0, max = 30, step = 5) 
   
   ),
  
  mainPanel(
    h3("Monthly Payment"),
    textOutput("payment")
    # dataTableOutput("amort")
  )

)


pay <- function(amount, interest, years) {

  r <- (interest/(100))/12  
  payment <- amount * r / (1 - (1 +r)^(-years * 12) )  
   
}

# amort <- function(amount, interest, years) {
#   pay <- pay(amount, interest, years)
#   data <- data_frame(month = seq(0, years*12))
#   data$amount <- 0
#   data$amount[ (data$month - 1) >= 0 & (data$month - 1) %% payfreq == 0 ] <- pay$payment
#   i <- which(data$payment != 0)
#   i <- i[length(i)]
#   data$amount [ i ] <- 0
#   data$amount [ i ] <- pay$amount * (years/12) * 12 / 12 -sum(data$amount)
#   data$totalPayed <- cumsum(data$amount)
#   
#   data$principal <- NA
#   data$principal[1] <- principal
#   idx <- (data$month - 1) >=0 & (data$month - 1)
#   idx.pr <- which(idx) [-length(idx)] + 12 - 1
#   if(any (idx.pr > max(data$month))) {
#     idx.pr <- idx.pr[-which(idx.pr > max(data$month))]
#   }
#   
#   if(firstpay > 1) {
#     data$principal[firstpay] <- pay$principal
#   }
#   data$principal[ idx.pr ] <- (1+ pay$r)^seq_len(length(idx.pr)) * pay$principal - ( (1 + pay$r)^seq_len(length(idx.pr)))
#   data$principal[ nrow(data) ] <-  0
#   
#   return(data)
# }

server <- function(input, output) {
  
  output$payment <- renderPrint({
    payment <- pay(input$amount, input$interest, input$years)
    cat("Your monthly payment will be $")
    cat(round(payment, 2))
    
  })
 
  # output$amort <- renderDataTable({
  #   data <- amort(input$amount, input$interest, input$years)
  #   data[-1, ]
  # }, options = list(iDisplayLength = 12))
  #  
  }


shinyApp(ui = ui, server = server)
