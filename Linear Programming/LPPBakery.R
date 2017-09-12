# Using LPSolve to decide what to bake to maximise
# sales from available resources

# In the cupboard :
# flour - 5000g     # butter - 2000g
# sugar - 2000g     # eggs - 30
# chocolate - 1000g

# Cake :
# flour - 250g      # butter - 250g
# sugar - 200g      # eggs - 4
# chocolate - 0g    # unit value - 300p

# Cookie :
# flour - 250g      # butter - 150g
# sugar - 250g      # eggs - 1
# chocolate - 200g  # unit value - 200p

library(lpSolve)

# Maximise :
# 300 * Cake + 200 * Cookie
# Subject To :
# 250 * Cake + 250 * Cookie <= 2000
# 250 * Cake + 150 * Cookie <= 2000
# 200 * Cake + 250 * Cookie <= 2000
# 4   * Cake + 1   * Cookie <= 30
# 0   * Cake + 200 * Cookie <= 1000
# 1   * Cake + 0   * Cookie >= 0
# 0   * Cake + 1   * Cookie >= 0

f.obj <- c(300, 200)
f.con <- matrix (c(250, 250,
                   250, 150,
                   200, 250,
                   4,   1,
                   0,   200,
                   1,   0,
                   0,   1), nrow=7, byrow=TRUE)
f.dir <- c("<=","<=","<=","<=","<=",">=",">=")
f.rhs <- c(5000, 2000, 2000, 30, 1000,0,0)


# In what proprtion should we bake each of the goods?
bakeQuantities = floor(lp ("max", f.obj, f.con, f.dir, f.rhs)$solution)

# How much sales can be made with the available resources?
sum(f.obj * bakeQuantities)
