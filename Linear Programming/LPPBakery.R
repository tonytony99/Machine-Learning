# Using LPSolve to decide what to bake to maximise
# sales from available resources
library(lpSolve)

# Items available in the cupboard :
itemTypes = c("flour","butter","sugar","eggs","chocolate")
cupboard.flour = 4000
cupboard.butter = 2500
cupboard.sugar = 3300
cupboard.eggs = 40
cupboard.chocolate = 1500

# Cake recipe :
cake.flour = 250
cake.butter = 200
cake.sugar = 200
cake.eggs = 4
cake.chocolate = 0
cake.value = 3

# Cookie recipe :
cookie.flour = 250
cookie.butter = 150
cookie.sugar = 250
cookie.eggs = 1
cookie.chocolate = 150
cookie.value = 2.4

# Maximise the money made from selling cakes and cookies
f.obj <- c(cake.value, cookie.value)
# Subject to the constraint of only using resources in the cupboard 
f.con <- matrix (c(cake.flour, cookie.flour,
                   cake.butter, cookie.butter,
                   cake.sugar, cookie.sugar,
                   cake.eggs,   cookie.eggs,
                   cake.chocolate, cookie.chocolate,
                   1,   0,
                   0,   1), nrow=7, byrow=TRUE)
f.dir <- c("<=","<=","<=","<=","<=",">=",">=")
f.rhs <- c(cupboard.flour, cupboard.butter, cupboard.sugar,
           cupboard.eggs, cupboard.chocolate,0,0)

# In what proprtion should we bake each of the goods?
# Suppose we can sell fractions of baked goods for a fraction of the price
# bakeQuantities = lp ("max", f.obj, f.con, f.dir, f.rhs)$solution
# Suppose we can only sell whole units of baked goods
bakeQuantities = lp("max", f.obj, f.con, f.dir, f.rhs,all.int = TRUE)$solution

# How much sales can be made with the available resources?
sum(f.obj * bakeQuantities)

# Plot the constraints and value of each combination
cakes = 0:15
flour =   (cupboard.flour-cakes*cake.flour)/cookie.flour
butter =  (cupboard.butter-cakes*cake.butter)/cookie.butter
sugar =   (cupboard.sugar-cakes*cake.sugar)/cookie.sugar
eggs =    (cupboard.eggs-cakes*cake.eggs)/cookie.eggs          
chocolate = rep(cupboard.chocolate/cookie.chocolate,length(cakes))

cookies = cakes

values = matrix(data = rep(0,length(cakes)^2),nrow=length(cakes))

for (cake in 1:length(cakes)){
  for (cookie in 1:length(cookies)) {
    values[cake,cookie] = cakes[cake] * 3 + cookies[cookie] * 2
  }
}


# Show a heat map of the value of the combination of items
colsFunc = colorRampPalette(c("blue", "green"))
cols = colsFunc(300)
image(cakes,cookies,values,col = cols, main="LPP Bakery")
# Show the constraint on number of baked goods that can be produced
lines(cakes,flour,type="l",col="grey",lwd=3)
lines(cakes,butter,type="l",col="yellow",lwd=3)
lines(cakes,sugar,type="l",col="grey",lwd=3)
lines(cakes,eggs,type="l",col="orange",lwd=3)
lines(cakes,chocolate,type="l",col="brown",lwd=3)
# Show the most valuable possible combination of goods to bake
points(bakeQuantities[1],bakeQuantities[2])

legend("topright", legend=itemTypes, lty=1, lwd=3,
       col=c("grey", "yellow", "grey", "orange", "brown"))

