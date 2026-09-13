# AMR R Analysis
# 02 - Basic Statistics
# Author: Ogbole Michael Ogbole

# ----------------------------
# 1. AMR resistance data
# ----------------------------

# 1 = resistant
# 0 = susceptible

resistant <- c(1, 0, 1, 1, 0, 0, 1, 0, 1, 1)

# Display the data
resistant

# ----------------------------
# 2. Number of isolates
# ----------------------------

length(resistant)

# ----------------------------
# 3. Number of resistant isolates
# ----------------------------

sum(resistant)

# ----------------------------
# 4. Number of susceptible isolates
# ----------------------------

length(resistant) - sum(resistant)

# ----------------------------
# 5. Resistance proportion
# ----------------------------

mean(resistant)

# ----------------------------
# 6. Resistance percentage
# ----------------------------

mean(resistant) * 100

# ----------------------------
# 7. Colony count data
# ----------------------------

colony_counts <- c(12, 15, 18, 20, 22, 25, 28, 30, 35, 40)

# Mean
mean(colony_counts)

# Median
median(colony_counts)

# Variance
var(colony_counts)

# Standard deviation
sd(colony_counts)

# Standard error
sd(colony_counts) / sqrt(length(colony_counts))

# ----------------------------
# 8. Exact binomial confidence interval
# ----------------------------

binom_result <- binom.test(
  sum(resistant),
  length(resistant)
)

binom_result
