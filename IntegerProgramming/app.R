library(shiny)
library(lpSolve)

# Define UI for application that draws a histogram
ui <- shinyUI(
  fluidPage(align="center",
    h1("Integer Programming Bakery"),
    fluidRow(align="center",
             
       fluidRow(
         column(8,
                plotOutput("distPlot"),
                offset=2
         )
       ),
       
       fluidRow(
         column(8,
                verbatimTextOutput("optimumResult"),
                offset=2
         )
       ),
       
       hr(),
       
       fluidRow(
         column(3,h4("Flour"),offset=2
         ),
         column(3,h4("Butter")
         ),
         column(3,h4("Sugar")
         )
       ),
       
       fluidRow(
         column(2,
                h4("Doughnut")
         ),
         column(3,
                sliderInput("doughnutFlour", label = NULL, min = 5, max = 50, value = 40)
          ),
         column(3,
                sliderInput("doughnutButter", label = NULL, min = 5, max = 50, value = 10)
         ),
         column(3,
                sliderInput("doughnutSugar", label = NULL, min = 5, max = 50, value = 30)
         )
       ),
       
       fluidRow(
         column(2,
                h4("Cookie")
         ),
         column(3,
                sliderInput("cookieFlour", label = NULL, min = 5, max = 50, value = 10)
         ),
         column(3,
                sliderInput("cookieButter", label = NULL, min = 5, max = 50, value = 30)
         ),
         column(3,
                sliderInput("cookieSugar", label = NULL, min = 5, max = 50, value = 25)
         )
       ),
     
      hr(),
      
      p("Assume you have 500g of each ingredient to hand."),
      p("Assume also that doughnuts sell for 10p profit, whereas cookies sell for 15p profit."),
      p("In what ratio should you make doughnuts and cookies to make the most profit?"),
      p("In this demo, the solution is found using integer programming."),
      p("The lines on the plot illustrate constraints, i.e. you cannot make more than you have ingredients for."),
      p("The background colour represents total profit (increasing the further out you go)."),
      p("The black point illustrates the optimum number of cookies and doughnuts to produce, to maximise profit.")
    )
  )
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  output$distPlot <- renderPlot({
    # Items available in the cupboard :
    itemTypes = c("flour","butter","sugar")
    cupboard.flour = 500
    cupboard.butter = 500
    cupboard.sugar = 500
    
    # doughnut recipe :
    doughnut.flour = input$doughnutFlour
    doughnut.butter = input$doughnutButter
    doughnut.sugar = input$doughnutSugar
    doughnut.value = 0.1
    
    # Cookie recipe :
    cookie.flour = input$cookieFlour
    cookie.butter = input$cookieButter
    cookie.sugar = input$cookieSugar
    cookie.value = 0.15
    
    # Maximise the money made from selling doughnuts and cookies
    f.obj <- c(doughnut.value, cookie.value)
    # Subject to the constraint of only using resources in the cupboard 
    f.con <- matrix (c(doughnut.flour, cookie.flour,
                       doughnut.butter, cookie.butter,
                       doughnut.sugar, cookie.sugar,
                       1,   0,
                       0,   1), nrow=5, byrow=TRUE)
    f.dir <- c("<=","<=","<=",">=",">=")
    f.rhs <- c(cupboard.flour, cupboard.butter, cupboard.sugar,0,0)
    
    # In what proprtion should we bake each of the goods?
    # Suppose we can sell fractions of baked goods for a fraction of the price
    # bakeQuantities = lp ("max", f.obj, f.con, f.dir, f.rhs)$solution
    # Suppose we can only sell whole units of baked goods
    lpResult = lp("max", f.obj, f.con, f.dir, f.rhs,all.int = TRUE)
    bakeQuantities = lpResult$solution
    profit = lpResult$objval
    
    
    output$optimumResult <- renderPrint({
      cat("You should make", bakeQuantities[1], "doughnuts and", 
          bakeQuantities[2], "cookies to get a profit of £", format(profit,nsmall=2))
    })
    
    # How much sales can be made with the available resources?
    sum(f.obj * bakeQuantities)
    
    # Plot the constraints and value of each combination
    doughnuts = 0:20
    cookies = 0:20
    flour =   (cupboard.flour-doughnuts*doughnut.flour)/cookie.flour
    butter =  (cupboard.butter-doughnuts*doughnut.butter)/cookie.butter
    sugar =   (cupboard.sugar-doughnuts*doughnut.sugar)/cookie.sugar
    
    values = matrix(data = rep(0,length(doughnuts)^2),nrow=length(doughnuts))
    
    for (doughnut in 1:length(doughnuts)){
      for (cookie in 1:length(cookies)) {
        values[doughnut,cookie] = doughnuts[doughnut] * doughnut.value + cookies[cookie] * cookie.value
      }
    }
    
    # Show a heat map of the value of the combination of items
    colsFunc = colorRampPalette(c("blue", "green"))
    cols = colsFunc(300)
    image(doughnuts,cookies,values,col = cols)
    # Show the constraint on number of baked goods that can be produced
    lines(doughnuts,flour,type="l",col="dark grey",lwd=3)
    lines(doughnuts,butter,type="l",col="yellow",lwd=3)
    lines(doughnuts,sugar,type="l",col="light grey",lwd=3)
    # Show the most valuable possible combination of goods to bake
    points(bakeQuantities[1],bakeQuantities[2],lwd=5)
  
    legend("topright", legend=itemTypes, lty=1, lwd=3,
           col=c("dark grey", "yellow", "light grey"))
   
   })
})

# Run the application 
shinyApp(ui = ui, server = server)