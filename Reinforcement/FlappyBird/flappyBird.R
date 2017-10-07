# Game set up

# length of map
reps = 1000
# lowest and highest allowed bird height
bottom = 0; top = 200
# distance (time) between each pipe
xDistBetweenPipes = 100
# distance between each pipe end
yDistBetweenPipes = 50
# how close to the top or bottom can the pipe ends be
minPipeCentreFromEdge = yDistBetweenPipes + 20
maxPipeCentreFromEdge = top - bottom - minPipeCentreFromEdge

pipeCentres = rep(0,reps)
# put a pipe centre at every xDistBetweenPipes
# pipesCentres[t] : the centre point of the pipe (if any) at time t
pipeCentres[(1:reps) %% xDistBetweenPipes == 0] = round(minPipeCentreFromEdge + runif(reps/xDistBetweenPipes) * (maxPipeCentreFromEdge - minPipeCentreFromEdge))
# the extremes of each pipe
highPipeBottoms = ifelse(pipeCentres>0, pipeCentres+ yDistBetweenPipes/2,top)
lowPipeTops = ifelse(pipeCentres>0, pipeCentres - yDistBetweenPipes/2,bottom)
# pipeDist[t] : horizontal distance to next nearest pipe at time t
pipeDist = rep(xDistBetweenPipes : 1, reps/xDistBetweenPipes)

# heights[t] : height of bird at time t
# taps[t] : whether or not tap at time t
# vs[t] : speed at time t
vs = taps = heights = rep(0,reps)


isAlive = function(height, bottom, lowPipeTop, highPipeBottom, top) {
  return (height > bottom & height > lowPipeTop & height < highPipeBottom & height < top)
}

# split the bird height into groups of 55
getHeightState = function(height) {
  return(floor((height)/(top-bottom+1)*55)+1)
}

# split the distance to next pipe into groups of 55
getPipeXDistState = function(pipeXDist) {
  return(floor((pipeXDist-1)/xDistBetweenPipes*55)+1)
}

# split the vertical distance between the bird and the next pipe into groups of 55
getPipeYDistState = function(pipeYDist) {
  minYDist = bottom - (top - minPipeCentreFromEdge)
  maxYDist = top - (bottom + minPipeCentreFromEdge)
  return(floor((pipeYDist - minYDist) / (maxYDist - minYDist) * 55)+1)
}

states = data.frame("heightState" = rep(0,reps),"pipeXDistState"=rep(0,reps),"pipeYDistState"=rep(0,reps))

# Learning setup
# height x pipeXDist x pipeYDist x tap
# it's generally better to not tap then tap
highReward = rep(2, 55*55*55); # for not tapping
lowReward = rep(1, 55*55*55); # for tapping
rewards = array(c(highReward,lowReward),c(55,55,55,2))
# how much reward do you get for each t you survive
survivalReward = 1
# punishment for loosing the game
lossPunishment = -15000

trainingIterations = 250
experimentHeights = rep(heights,trainingIterations)
experimentHeights = array(experimentHeights, c(length(heights),trainingIterations))
experimentScores = rep(0,trainingIterations)
punishmentDecay = 0.5
# if we loose, how many previous time steps are punished?
iterationsToGoBack = 50
longTermRewardDecay = 1
longTermPunishmentDecay = 1

for (trainingIteration in 1:trainingIterations) {
  
  survivalReward = survivalReward * longTermRewardDecay
  lossPunishment = lossPunishment * longTermPunishmentDecay
  
  vs = taps = heights = rep(0,reps)
  
  # START GAME
  v = 0     # velocity
  a = -0.2  # acceleration
  height = (top-bottom)/2 # height
  t = 1     # time
  vs[t] = v
  heights[t] = height
  taps[t] = 1
  
  # UPDATE CURRENT STATE
  heightState = getHeightState(height)
  pipeXDistState = getPipeXDistState(pipeDist[t+1])
  # what t value is the next pipe at
  nextPipeTime = t + pipeDist[t+1]
  # where is the centre of the next pipe
  nextPipeCentre = pipeCentres[nextPipeTime]
  pipeYDistState = getPipeYDistState(height-nextPipeCentre)
  # keep track of state
  states[t,] = c(heightState, pipeXDistState, pipeYDistState)
  
  # game loop
  while(isAlive(height, bottom, lowPipeTops[t], highPipeBottoms[t], top)) {
    t = t + 1
    
    # REWARD THE LAST MOVE
    lastHeightState = states[t-1,1]
    lastPipeXDistState = states[t-1,2]
    lastPipeYDistState = states[t-1,3]
    oldReward = rewards[lastHeightState,lastPipeXDistState,lastPipeYDistState,taps[t-1]]
    # rewards[lastHeightState,lastPipeXDistState,lastPipeYDistState,taps[t-1]] = oldReward + survivalReward
    rewards[lastHeightState,lastPipeXDistState,lastPipeYDistState,taps[t-1]] = ((t-1)/(t))* oldReward + (1/t) * survivalReward
    
    
    # print(paste("rewarding state : ",lastHeightState,lastPipeXDistState,lastPipeYDistState,taps[t-1]))
    
    
    # UPDATE CURRENT STATE
    heightState = getHeightState(height)
    pipeXDistState = getPipeXDistState(pipeDist[t+1])
    # what t value is the next pipe at
    nextPipeTime = t + pipeDist[t+1]
    # where is the centre of the next pipe
    nextPipeCentre = pipeCentres[nextPipeTime]
    pipeYDistState = getPipeYDistState(height-nextPipeCentre)
    # keep track of state
    states[t,] = c(heightState, pipeXDistState, pipeYDistState)
    
    # CHOOSE A MOVE
    # if the best reward given the current state comes from tapping
    if (which.max(rewards[heightState, pipeXDistState,pipeYDistState,]) == 2) {
      # then tap
      taps[t] = 2
      v = 4
    }  else {
      # do not tap
      taps[t] = 1
    }
    
#    print(paste("t :", t, " rewards :",rewards[heightState, pipeXDistState,pipeYDistState,1],rewards[heightState, pipeYDistState,pipeYDistState,2],
#                " states :",heightState, pipeXDistState,pipeYDistState," tap:",taps[t]))
  
    
    
    # UPDATE THE GAME
    v = v + a
    height = max(0,height+v) 
    vs[t] = v
    heights[t] = height
    
  }
  
  
  currentPunishment = lossPunishment
  for (i in 0:iterationsToGoBack) {
    # PUNISH THE MOVES THAT LEAD UP TO THE LOSS
    lastHeightState = states[t-i,1]
    lastPipeXDistState = states[t-i,2]
    lastPipeYDistState = states[t-i,3]
    oldReward = rewards[lastHeightState,lastPipeXDistState,lastPipeYDistState,taps[t-i]]
    # rewards[lastHeightState,lastPipeXDistState,lastPipeYDistState,taps[t-i]] = oldReward + currentPunishment
    rewards[lastHeightState,lastPipeXDistState,lastPipeYDistState,taps[t-i]] = ((t-1)/(t)) * oldReward + (1/t) * currentPunishment
    currentPunishment = currentPunishment * punishmentDecay
    # print(paste("punishing state : ",lastHeightState,lastPipeXDistState,lastPipeYDistState,taps[t]," for ",currentPunishment))
    
  }
  
  experimentHeights[,trainingIteration] = heights
  experimentScores[trainingIteration] = t
}

# Used to limit the number/size of plots reduced
mA = trainingIterations

# Plot each iteration so you can flick through them
for (j in seq(mA,1,by=-50)) {
  plot(main=paste("Iteration ",j),experimentHeights[,j],xlim=c(0,max(experimentScores)), type = "l", ylim=c(bottom,top),ylab="height")
  lines(highPipeBottoms, col = "green")
  lines(lowPipeTops, col = "green")
}

# Plot all of the performances against each other
plot(experimentHeights[,mA],xlim=c(0,experimentScores[mA]), type = "l", ylim=c(bottom,top),ylab="height",main="All Bird Paths")
lines(highPipeBottoms, col = "green")
lines(lowPipeTops, col = "green")
for (j in mA:1) {
  lines(experimentHeights[,j],col=rgb(1.0,0.0,0.0,0.5))
}


# how did the program do with each iteration?
plot(experimentScores,type="l",xlab="Iteration",ylab="Score",main="Score Improvement Over Time")


