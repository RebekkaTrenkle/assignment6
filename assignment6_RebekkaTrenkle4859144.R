# install package
source("https://cran.r-project.org/package=HMM")
install.packages("HMM")
# You must install the package HMM 
require(HMM)

# create the variable calles nSim and assign it the value 200. This is the number of
# simulations
nSim = 2000

# create the list calles States, which holds the two character strings "Fair" and "Unfair"
States = c("Fair", "Unfair")

# create the variable called symbols, which holds a range of integers from 1 to 6
# these are the possible numbers of a dice
Symbols = 1:6 

# creates the 2x2 Transitionsmatrix transProbs with the probabilities to use the different dices
# byrow = TRUE : the matrix is filled by rows
# https://www.rdocumentation.org/packages/Matrix/versions/1.2-17/topics/Matrix
transProbs = matrix(c(0.99, 0.01, 0.02, 0.98), c(length(States), length(States)), byrow = TRUE)


# creates the 2x6 Emissionsmatrix emissionProbs with the probabilities to get a specific number
# with the fair (every entry == 1/6) or the unfair dice (to get a 6 has probability of 1/2)
emissionProbs = matrix(c(rep(1/6, 6), c(rep(0.1, 5), 0.5)), c(length(States), length(Symbols)), byrow = TRUE)

# initiate HMMM,creates a list of length 5 with all the information
# https://www.rdocumentation.org/packages/HMM/versions/1.0/topics/initHMM
hmm = initHMM(States, Symbols, transProbs = transProbs, emissionProbs = emissionProbs)


# starts the simulation, creates a list that holds the states (fair or unfair) and
# the observables, meaning the number rolled with the dice
# https://www.rdocumentation.org/packages/HMM/versions/1.0/topics/simHMM
sim = simHMM(hmm, nSim)

# create a list of the most probable states from observations
# https://www.rdocumentation.org/packages/HMM/versions/1.0/topics/viterbi
vit = viterbi(hmm, sim$observation)

# compute the forward probabilites
# https://www.rdocumentation.org/packages/HMM/versions/1.0/topics/forward
f = forward(hmm, sim$observation)

# create the variables i and j
#
i <- f[1, nSim]
j <- f[2, nSim]

# create the variable probObservation
#
probObservations = (i + log(1 + exp(j - i)))

######################################### 
## NO MORE DOCUMENTATION BELOW THIS LINE 
######################################### 
x = list(hmm = hmm, sim = sim, vit = vit) 

# PLOT simulated throws at top ##################### 
mn = "Fair and unfair die" 
xlb = "Throw nr." 
ylb = ""

plot(x$sim$observation, ylim = c(-7.5, 6), pch = 3, main = mn, xlab = xlb, ylab = ylb, bty = "n", yaxt = "n") 
axis(2, at = 1:6)

# PLOT Simulated, which die was used (ground truth) ###########
text(0, -1.2, adj = 0, cex = 0.8, col = "black", "True: green = fair die") 
for (i in 1:nSim) {   
  if (x$sim$states[i] == "Fair")     
  rect(i, -1, i + 1, 0, col = "green", border = NA)   
  else rect(i, -1, i + 1, 0, col = "red", border = NA) 
}

# PLOT Most probable path (viterbi) ####################### 
text(0, -3.2, adj = 0, cex = 0.8, col = "black", "Most probable path") 
for (i in 1:nSim) {   
  if (x$vit[i] == "Fair")     
    rect(i, -3, i + 1, -2, col = "green", border = NA)   
  else rect(i, -3, i + 1, -2, col = "red", border = NA) 
}

# PLOT Differences #################### 
text(0, -5.2, adj = 0, cex = 0.8, col = "black", "Difference") 
differing = !(x$sim$states == x$vit) 
for (i in 1:nSim) {   
  if (differing[i])     
    rect(i, -5, i + 1, -4, col = rgb(0.3, 0.3, 0.3), border = NA)   
  else rect(i, -5, i + 1, -4, col = rgb(0.9, 0.9, 0.9), border = NA) 
}


######################################

# You must install the package HMM 
require(HMM) 
nSim = 2000 
States = c("coding", "non-coding") 
Symbols = 1:4
transProbs = matrix(c(0.99, 0.01, 0.02, 0.98), c(length(States), length(States)), byrow = TRUE) 
emissionProbs = matrix(c(rep(1/4, 4), c(rep(0.1, 2), 0.4, 0.4)), c(length(States), length(Symbols)), byrow = TRUE)
hmm = initHMM(States, Symbols, transProbs = transProbs, emissionProbs = emissionProbs) 
sim = simHMM(hmm, nSim) 
vit = viterbi(hmm, sim$observation) 
f = forward(hmm, sim$observation) 
i <- f[1, nSim] 
j <- f[2, nSim] 
probObservations = (i + log(1 + exp(j - i)))

######################################### 
## NO MORE DOCUMENTATION BELOW THIS LINE 
######################################### 
x = list(hmm = hmm, sim = sim, vit = vit) 

# PLOT simulated throws at top ##################### 
mn = "coding and non-coding sequence" 
xlb = "nr." 
ylb = ""

plot(x$sim$observation, ylim = c(-7.5, 6), pch = 3, main = mn, xlab = xlb, ylab = ylb, bty = "n", yaxt = "n") 
axis(2, at = 1:4)

# PLOT Simulated, which die was used (ground truth) ###########
text(0, -1.2, adj = 0, cex = 0.8, col = "black", "True: green = coding sequence") 
for (i in 1:nSim) {   
  if (x$sim$states[i] == "coding")     
    rect(i, -1, i + 1, 0, col = "green", border = NA)   
  else rect(i, -1, i + 1, 0, col = "red", border = NA) 
}

# PLOT Most probable path (viterbi) ####################### 
text(0, -3.2, adj = 0, cex = 0.8, col = "black", "Most probable path") 
for (i in 1:nSim) {   
  if (x$vit[i] == "coding")     
    rect(i, -3, i + 1, -2, col = "green", border = NA)   
  else rect(i, -3, i + 1, -2, col = "red", border = NA) 
}

# PLOT Differences #################### 
text(0, -5.2, adj = 0, cex = 0.8, col = "black", "Difference") 
differing = !(x$sim$states == x$vit) 
for (i in 1:nSim) {   
  if (differing[i])     
    rect(i, -5, i + 1, -4, col = rgb(0.3, 0.3, 0.3), border = NA)   
  else rect(i, -5, i + 1, -4, col = rgb(0.9, 0.9, 0.9), border = NA) 
}

