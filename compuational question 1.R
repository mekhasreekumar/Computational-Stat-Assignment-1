
#Question1 part a
# Load required library
library(ggplot2)

# Step 1: To Generate uniform samples
n_samples <- 1000  
uniform_samples <- runif(n_samples) 

# Step 2: Parameters for Binomial(10, 1/3)
n <- 10  
p <- 1/3  

# Step 3: Compute the CDF for Binomial(10, 1/3)
cdf_values <- pbinom(0:n, size = n, prob = p)

# Step 4: Inversion method to sample from Binomial distribution
inverse_sample <- function(u, cdf_values) {
  for (k in 0:n) {
    if (u <= cdf_values[k + 1]) {
      return(k)
    }
  }
}

# Step 5: Apply the inverse transform to all uniform samples
binomial_samples <- sapply(uniform_samples, inverse_sample, cdf_values = cdf_values)

# Step 6: Plot the histogram of generated samples
binomial_data <- data.frame(samples = binomial_samples)
ggplot(binomial_data, aes(x = samples)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black", aes(y = after_stat(density))) +
  ggtitle("Histogram of 1000 samples from Binomial(10, 1/3)") +
  xlab("Number of successes") +
  ylab("Density") +
  scale_x_continuous(breaks = 0:n)














#part b 
# Load necessary library
library(ggplot2)

# Step 1: Define parameters for the Binomial distribution
n_trials <- 10      
p_success <- 1/3
n_samples <- 1000   

# Step 2: Inversion function for a single Bernoulli trial
sample_bernoulli_inversion <- function(p) {
  u <- runif(1)  # Generate a uniform random variable U ~ U(0, 1)
  return(as.integer(u <= p))  # Return 1 (success) if U <= p, else 0 (failure)
}

# Step 3: Function to generate Binomial samples using both inversion and transformation
sample_binomial_inversion_transformation <- function(n, p, size) {
  samples <- numeric(size)  # Vector to store the samples
  
  # For each sample, perform n Bernoulli trials and sum the results
  for (i in 1:size) {
    trials <- replicate(n, sample_bernoulli_inversion(p))  # Generate n Bernoulli trials
    samples[i] <- sum(trials)  # Sum the results to get a Binomial sample
  }
  
  return(samples)
}

# Step 4: Generate 1000 samples from Binomial(10, 1/3)
binomial_samples <- sample_binomial_inversion_transformation(n_trials, p_success, n_samples)

# Step 5: Plot the histogram of the samples with updated labels
ggplot(data = data.frame(successes = binomial_samples), aes(x = successes)) +
  geom_histogram(bins = 11, aes(y = after_stat(density)), fill = "skyblue", color = "black", boundary = 0) +
  labs(title = "Histogram of Samples from Binomial(10, 1/3)",
       x = "Total Number of Successes",
       y = "Probability Density") +
  theme_minimal()






















#part c 

set.seed(0)  # For reproducibility

# Define parameters
n_trials <- 10
p_success <- 1/3
n_samples <- 100

# Inversion method to generate binomial samples
u <- runif(n_samples)  # Uniform random samples
binomial_samples <- qbinom(u, size = n_trials, prob = p_success)

# Estimate expectation
estimated_mean <- mean(binomial_samples)

# Calculate standard deviation and standard error
sample_variance <- var(binomial_samples)
standard_error <- sqrt(sample_variance / n_samples)


# Confidence bounds (95% CI)
z_score <- qnorm(0.975)
lower_bound <- estimated_mean - z_score * standard_error
upper_bound <- estimated_mean + z_score * standard_error

# Print results
cat("Estimated Expectation:", estimated_mean, "\n")
cat("Standard Error:", standard_error, "\n")
cat("95% Confidence Interval:", lower_bound, "to", upper_bound, "\n")









#question 2 
# Function to generate Poisson(t) samples using the transformation method
generate_poisson_transformation <- function(t, n_samples) {
  poisson_samples <- numeric(n_samples)  # Vector to store samples
  
  for (i in 1:n_samples) {
    sum_exponential <- 0  # Initialize the cumulative sum of Exponential(1) RVs
    count <- 0            # Count the number of Exponential RVs sampled
    
    # Generate Exponential(1) RVs until the cumulative sum exceeds t
    repeat {
      u <- runif(1)                          # Sample from Uniform(0, 1)
      exp_sample <- -log(u)                  # Generate Exponential(1) from Uniform
      sum_exponential <- sum_exponential + exp_sample  # Update cumulative sum
      count <- count + 1                     # Increment count
      if (sum_exponential > t) break         # Exit if the sum exceeds t
    }
    
    poisson_samples[i] <- count - 1  # Record the number of events (count - 1)
  }
  
  return(poisson_samples)
}

# Set parameters and run the program with t = 1 and 1000 samples
set.seed(0)
t <- 1
n_samples <- 1000
poisson_samples <- generate_poisson_transformation(t, n_samples)

# Plot histogram with specific breaks for each Poisson value (0, 1, 2, etc.)
hist(poisson_samples, breaks = seq(-0.5, max(poisson_samples) + 0.5, 1), 
     probability = TRUE, main = "Histogram of Poisson(1) Samples", 
     xlab = "Value", ylab = "Probability")


# Function to calculate mean, standard error, and confidence interval
calculate_stats <- function(samples) {
  mean_est <- mean(samples)
  sample_sd <- sd(samples)
  standard_error <- sample_sd / sqrt(length(samples))
  z_score <- qnorm(0.975)
  lower_bound <- mean_est - z_score * standard_error
  upper_bound <- mean_est + z_score * standard_error
  return(list(mean = mean_est, se = standard_error, ci = c(lower_bound, upper_bound)))
}

# Generate Poisson(1) samples with varying sizes and calculate statistics
sample_sizes <- c(10, 100, 1000, 10000)
for (size in sample_sizes) {
  samples <- generate_poisson_transformation(t, size)
  stats <- calculate_stats(samples)
  
  cat("Sample size:", size, "\n")
  cat("Estimated Mean:", stats$mean, "\n")
  cat("Standard Error:", stats$se, "\n")
  cat("95% Confidence Interval:", stats$ci[1], "to", stats$ci[2], "\n\n")
}







#Question 3


#Question 3 part a
# Load necessary library
library(ggplot2)

# Parameters for the mixture distribution
alpha1 <- 0.2
mu1 <- 1
sigma1 <- sqrt(0.5)

alpha2 <- 0.8
mu2 <- 2
sigma2 <- sqrt(0.1)

# Proposal distribution parameters
mu_q <- 1.5
sigma_q <- sqrt(0.5)

# Number of samples
n_samples <- 10000

# Rejection sampling
samples <- numeric(n_samples)
accepted <- 0
c <- 0  # Placeholder for max ratio

for (i in 1:n_samples) {
  # Sample from the proposal distribution
  x <- rnorm(1, mean = mu_q, sd = sigma_q)
  
  # Mixture pdf
  p_x <- alpha1 * dnorm(x, mean = mu1, sd = sigma1) + alpha2 * dnorm(x, mean = mu2, sd = sigma2)
  
  # Proposal pdf
  q_x <- dnorm(x, mean = mu_q, sd = sigma_q)
  
  # Update c
  if (q_x > 0) {
    ratio <- p_x / q_x
    if (ratio > c) {
      c <- ratio
    }
  }
  
  # Acceptance criterion
  u <- runif(1)
  if (u < (p_x / (c * q_x))) {
    samples[accepted + 1] <- x
    accepted <- accepted + 1
  }
}

# Trim the samples array to the accepted length
samples <- samples[1:accepted]

# Calculate acceptance rate
acceptance_rate <- accepted / n_samples

# Output results
cat("Acceptance Rate:", acceptance_rate, "\n")

# Plotting the results
ggplot(data.frame(samples), aes(x = samples)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = 'blue', alpha = 0.5) +
  stat_function(fun = function(x) {
    alpha1 * dnorm(x, mean = mu1, sd = sigma1) + alpha2 * dnorm(x, mean = mu2, sd = sigma2)
  }, color = 'red') +
  labs(title = paste("Acceptance Rate:", round(acceptance_rate, 4))) +
  xlab("Value") + ylab("Density")







# Composition method
n_comp_samples1 <- round(n_samples * alpha1)
samples1 <- rnorm(n_comp_samples1, mean = mu1, sd = sigma1)

n_comp_samples2 <- round(n_samples * alpha2)
samples2 <- rnorm(n_comp_samples2, mean = mu2, sd = sigma2)

# Combined samples
comp_samples <- c(samples1, samples2)

# Plotting the composition method results
ggplot(data.frame(comp_samples), aes(x = comp_samples)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = 'green', alpha = 0.5) +
  stat_function(fun = function(x) {
    alpha1 * dnorm(x, mean = mu1, sd = sigma1) + alpha2 * dnorm(x, mean = mu2, sd = sigma2)
  }, color = 'red') +
  labs(title = "Composition Method") +
  xlab("Value") + ylab("Density")
