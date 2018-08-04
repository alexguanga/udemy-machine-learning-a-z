# Thomas Sampling

# Loading the dataset
df <- read.csv('archive/Ads_CTR_Optimisation.csv')

# Implementing Random Selection
N = 10000
d = 10
ads_selected = integer(0)
total_reward = 0
for (n in 1:N) {
  ad = sample(1:10, 1)
  ads_selected = append(ads_selected, ad)
  reward = df[n, ad]
  total_reward = total_reward + reward
}
# Total Reward with the random selection is 1243 points

# Visualising the results of the Random Selection
hist(ads_selected,
     col = 'blue',
     main = 'Histogram of ads selections',
     xlab = 'Ads',
     ylab = 'Number of times each ad was selected')


#------------------------------------------------------------
# Thomas Sampling Implementation
#------------------------------------------------------------
N = 10000
d = 10

# Creating two vectors of size 0 that have begin at 0. As we go through the programming
number.of.rewards.1 <- integer(d)
number.of.rewards.2 <- integer(d)
ads.selected <- integer(0)
total.reward = 0

# Assumptions
# Ad i gets rewards y from Bernoulli Distribution
# Theta i is the total # the ad got rewarded 1 / the total # of times the ad was shown (uniform distribution)


for (n in 1:N){
  max.random <- 0
  ads <- 0
  
  for (i in 1:d){
    random.beta <- rbeta(n = 1, 
                         shape1 = number.of.rewards.1[i] + 1,
                         shape2 = number.of.rewards.2[i] + 1)
    
    # Storing the random draws if it is the largest prob (or the highest possiblity of return)
    if (random.beta > max.random) {
      max.random <- random.beta
      ads = i # Tracking the index of the upper bound (whenever we find ourselves inside the if statement)
    }
  }
  
  ads.selected <- append(ads.selected, ads)
  reward <- df[n, ads] # Extracting whether or not the user will click on the ad (0 or 1)
  
  # Checking if the value is what the user was likely to make him click on!
  if (reward == 1) {
    number.of.rewards.1[ads] <- number.of.rewards.1[ads] + 1
  } else {
    number.of.rewards.2[ads] <- number.of.rewards.2[ads] + 1
  }

  # Calculating the total reward
  total.reward <- total.reward + reward
}

#------------------------------------------------------------
# Visualizing Thomas Sampling Implementation
#------------------------------------------------------------
hist(ads.selected, 
     col = 'blue',
     main = 'Histogram of ads selections',
     xlab = 'Ads',
     ylab = 'Frequency')

# The fifth ad demonstrated the best results



