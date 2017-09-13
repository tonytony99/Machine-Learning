# Interactive information about collecting cards. There are N cards
# to collect which each appear with uniform probability 1/N
# Given an incomplete collection of cards, how many should
# we expect to buy before we get a new one

# Think of lots of geometric distributions, 1 after the other (1 for each new card)
# At the start we have 0 cards, the probability of getting a new card from x more cards is
# p(x) = N/N * (1-N/N)^(x-1)
# When we have all but the last card, the probability of getting the final card from x more cards is 
# p(x) = 1/N * (1-1/N)^(x-1)
# When we have n cards, the probability of getting a new card from x more cards is
# p(x) = (N-n)/N * (1-(N-n)/N)^(x-1)

# Knowing the expectation of x to be 1/p, we know that for the first card
# E(X) = 1/(N/N) = 1
# And for the very last (Nth) card
# E(X) = 1/(1/N) = N
# And for the nth card
# E(X) = 1/((1+N-n)/N) = N / (1+N-n)

library(shiny)

# Define UI for application
ui <- fluidPage(
   
   # Application title
   h1("How many more cards before I get a new one?"),
   
   # Sidebar with a slider input for number of cards collected so far
   # and number of possible unique cards
   sidebarLayout(
      sidebarPanel(
        sliderInput("cardsToCollect",
                    "Number of cards to collect :",
                    min = 0,
                    max = 200,
                    value = 140)
        ,
        sliderInput("cardsSoFar",
                    "Number of unique cards so far :",
                    min = 0,
                    max = 200,
                    value = 50)
        , 
        helpText(strong(verbatimTextOutput("myText")))
        , 
        helpText("Calculated using the geometric distribution", a("(info here)",href="https://en.wikipedia.org/wiki/Geometric_distribution"))
      )
      ,
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("distPlot")
      )
  )
   
)


# Define server logic required to draw the page
server <- function(input, output, session) {
  
  observeEvent(input$cardsSoFar,{
    updateSliderInput(session, "cardsToCollect", min =input$cardsSoFar+1, max=200)
  })
  
  observeEvent(input$cardsToCollect,{
    updateSliderInput(session, "cardsSoFar", min =0, max=input$cardsToCollect-1)
  })
  
   output$distPlot <- renderPlot({
      cardsToCollect = input$cardsToCollect 
      cardsSoFar = input$cardsSoFar
      x = 1:(cardsToCollect*10)
      probs = (cardsToCollect-cardsSoFar)/cardsToCollect * (1-(cardsToCollect-cardsSoFar)/cardsToCollect)^(x-1)
      # expectedValueEstimate = sum(x * probs)
      expectedValue = cardsToCollect / (cardsToCollect-cardsSoFar)
      plot(probs, type = "l", xlab="# Cards Bought", ylab = "P(Get New Card)", 
          main = paste("Starting with ",cardsSoFar," of ",cardsToCollect," unique cards"), xlim = c(1,expectedValue + 3 * expectedValue))
      abline(v = expectedValue, col="green")
      legend("topright",legend="Expected #Cards Before Next Unique",col="green",lty = 1)
   })
   
   output$myText <- renderText({
     paste("Expect a new card when you buy ",
           round(input$cardsToCollect / (input$cardsToCollect-input$cardsSoFar))
           ," more cards")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

