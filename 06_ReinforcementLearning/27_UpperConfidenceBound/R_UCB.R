# UCB

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
# UCB Implementation
#------------------------------------------------------------
N = 10000
d = 10

# We are creating two vectors of size 0 that have begin at 0. As we go through the programming
# we will begin to see the most effective selectors
numbers.selections <- integer(d)
sums.rewards <- integer(d)
ads.selected <- integer(0)
total.reward = 0


for (n in 1:N){
  max.upper.bounds <- 0
  ads <- 0
  
  for (i in 1:d){
    if (numbers.selections[i] > 0) { 
      # For the first 10 cases, this would be false because none of them have been selected
      
      average.rewards <- sums.rewards[i] / numbers.selections[i]
      delta.i <- sqrt(3/2 * log(n) / numbers.selections[i])
      upper.bound <- average.rewards + delta.i
    }
   else {
     # We are using a large number because this can provide each init each ad to be chosen at least once!
     upper.bound <- 1e400
   }
    if (upper.bound > max.upper.bounds) {
      max.upper.bounds <- upper.bound
      ads = i # Tracking the index of the upper bound (whenever we find ourselves inside the if statement)
    }
  }
  ads.selected <- append(ads.selected, ads)
  numbers.selections[ads] <- numbers.selections[ads] + 1
  reward <- df[n, ads] # Extracting whether or not the user will click on the ad (0 or 1)
  sums.rewards[ads] <- sums.rewards[ads] + reward # Adding the reward for this given round to the previous rounds
  
  # Calculating the total reward
  total.reward <- total.reward + reward
}

#------------------------------------------------------------
# Visualizing UCB Implementation
#------------------------------------------------------------
hist(ads.selected, 
     col = 'blue',
     main = 'Histogram of ads selections',
     xlab = 'Ads',
     ylab = 'Frequency')

# The fifth ad demonstrated the best results



