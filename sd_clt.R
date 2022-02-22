# our population: 100 random numbers from 0 to 1
population <- runif(100, min = 0, max = 1)

# An empty matrix that will contain the 1,000 sample SDs
sample_sd <- matrix(nrow = 10000, ncol = 1)

# the size of each sample
# you can change this to see how the resulting sampling distribution changes
sample_size <- 50

# a for-loop that fills up the matrix above
# in each loop, it takes a sample of 20 numbers and computes the SD
for(i in 1:length(sample_sd)){
  sample_sd[i,1] <- sd(population[sample(1:length(population), sample_size)])
}

# The mean sample SD from the 1,000 samples
mean(sample_sd[,1])

# the true SD in the population
sd(population)

hist(sample_sd[,1], breaks = seq(0.10, 0.45, 0.01))
