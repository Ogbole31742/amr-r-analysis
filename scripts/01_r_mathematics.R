# AMR R Analysis
# 01 - R Mathematics
# Author: Ogbole Michael Ogbole

# ----------------------------
# 1. Basic arithmetic
# ----------------------------

2 + 3
10 - 5
10 * 5
10 / 5
10^2

# ----------------------------
# 2. Variables
# ----------------------------

x <- 10
y <- 5

x
y

x + y
x * y
x^2 + 2*y

# ----------------------------
# 3. Algebra
# ----------------------------

# Solve: 2x + 5 = 15
x <- (15 - 5) / 2
x

# Verify the solution
2*x + 5

# Solve: 3x + 7 = 22
x <- (22 - 7) / 3
x

# Verify the solution
3*x + 7

# ----------------------------
# 4. Fractions and percentages
# ----------------------------

7 / 2

20 / 80
20 / 80 * 100

# ----------------------------
# 5. Basic statistics
# ----------------------------

counts <- c(10, 15, 20, 25, 30)

mean(counts)
median(counts)
min(counts)
max(counts)

# Range
max(counts) - min(counts)

# Variance
var(counts)

# Standard deviation
sd(counts)

# Standard error
sd(counts) / sqrt(length(counts))