## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----load-package-------------------------------------------------------------
library(heaping)

## ----create-example-data------------------------------------------------------
# Generate realistic age data using a log-normal distribution
# These parameters produce a realistic age distribution
set.seed(123)
age_true <- rlnorm(10000, meanlog = 2.466869, sdlog = 1.652772)
age_true <- round(age_true[age_true < 93])

# Create heaped version where 27% of respondents round their age to multiples of 5
# This heap_ratio is typical for survey data with moderate heaping
heap_ratio <- 0.27
n <- length(age_true)
indices_to_heap <- sample(n, round(heap_ratio * n))
age_heaped <- age_true
age_heaped[indices_to_heap] <- round(age_heaped[indices_to_heap] / 5) * 5

## ----visualize-heaping, fig.cap="Comparison of true ages vs heaped ages"------
oldpar <- par(mfrow = c(1, 2))

# Define breaks to cover the full range
age_breaks <- seq(0, max(age_true, age_heaped) + 1, by = 1)

# True ages
hist(age_true, breaks = age_breaks, col = "steelblue",
     main = "True Ages (No Heaping)", xlab = "Age",
     border = "white")

# Heaped ages
hist(age_heaped, breaks = age_breaks, col = "coral",
     main = "Heaped Ages", xlab = "Age",
     border = "white")
par(oldpar)

## ----whipple-index------------------------------------------------------------
# Standard Whipple index (100 = no heaping, 500 = maximum heaping)
whipple(age_true, method = "standard")
whipple(age_heaped, method = "standard")

# Modified Whipple index (0 = no heaping, 1 = maximum heaping)
whipple(age_true, method = "modified")
whipple(age_heaped, method = "modified")

## ----myers-index--------------------------------------------------------------
# Myers' index (0 = no heaping, 90 = maximum)
myers(age_true)
myers(age_heaped)

## ----bachi-index--------------------------------------------------------------
# Bachi's index (0 = no heaping, 90 = maximum)
bachi(age_true)
bachi(age_heaped)

## ----noumbissi-index----------------------------------------------------------
# Noumbissi's index for digit 0 (1.0 = no heaping)
noumbissi(age_true, digit = 0)
noumbissi(age_heaped, digit = 0)

# Noumbissi's index for digit 5
noumbissi(age_true, digit = 5)
noumbissi(age_heaped, digit = 5)

## ----all-indices--------------------------------------------------------------
# Calculate all indices
indices_true <- heaping_indices(age_true)
indices_heaped <- heaping_indices(age_heaped)

# Compare
data.frame(
  Index = names(indices_true),
  True = round(unlist(indices_true), 3),
  Heaped = round(unlist(indices_heaped), 3)
)

## ----weighted-indices---------------------------------------------------------
# Create example weights
weights <- runif(length(age_heaped), 0.5, 2)

# Weighted Whipple index
whipple(age_heaped, weight = weights)

## ----basic-correction---------------------------------------------------------
# Correct heaping using log-normal distribution
age_corrected <- correctHeaps(age_heaped,
                              heaps = "5year",  # heaps at 0, 5, 10, 15, ...
                              method = "lnorm",
                              seed = 42)        # for reproducibility

# Compare Whipple indices
c(Original = whipple(age_true),
  Heaped = whipple(age_heaped),
  Corrected = whipple(age_corrected))

## ----visualize-correction, fig.cap="Before and after heaping correction"------
oldpar <- par(mfrow = c(1, 2))

hist(age_heaped, breaks = age_breaks, col = "coral",
     main = "Before Correction", xlab = "Age", border = "white")

hist(age_corrected, breaks = age_breaks, col = "forestgreen",
     main = "After Correction", xlab = "Age", border = "white")

par(oldpar)

## ----correction-methods-------------------------------------------------------
# Log-normal (default) - good for right-skewed data like age
age_lnorm <- correctHeaps(age_heaped, method = "lnorm", seed = 42)

# Normal - good for symmetric data
age_norm <- correctHeaps(age_heaped, method = "norm", seed = 42)

# Uniform - simplest approach, baseline comparison
age_unif <- correctHeaps(age_heaped, method = "unif", seed = 42)

# Kernel - nonparametric, adapts to local data shape
age_kernel <- correctHeaps(age_heaped, method = "kernel", seed = 42)

# Compare results
data.frame(
  Method = c("Original", "Heaped", "Log-normal", "Normal", "Uniform", "Kernel"),
  Whipple = c(whipple(age_true), whipple(age_heaped),
              whipple(age_lnorm), whipple(age_norm),
              whipple(age_unif), whipple(age_kernel))
)

## ----verbose-output-----------------------------------------------------------
result <- correctHeaps(age_heaped, heaps = "5year",
                       method = "lnorm", seed = 42, verbose = TRUE)

# Number of values changed
result$n_changed

# Changes by heap age
head(result$changes_by_heap, 10)

# Heaping ratios (ratio > 1 indicates heaping)
head(result$ratios, 10)

## ----10year-heaping-----------------------------------------------------------
# Create data with 10-year heaping
age_heap10 <- age_true
heapers10 <- sample(c(TRUE, FALSE), length(age_true),
                    replace = TRUE, prob = c(0.3, 0.7))
age_heap10[heapers10] <- round(age_heap10[heapers10] / 10) * 10

# Correct 10-year heaping
age_corrected10 <- correctHeaps(age_heap10, heaps = "10year",
                                method = "lnorm", seed = 42)

c(Heaped = whipple(age_heap10),
  Corrected = whipple(age_corrected10))

## ----custom-heaps-------------------------------------------------------------
# Heaping at specific ages (e.g., 18, 21, 30, 40, 50, 65)
custom_positions <- c(18, 21, 30, 40, 50, 65)
age_custom <- correctHeaps(age_heaped,
                           heaps = custom_positions,
                           method = "lnorm", seed = 42)

## ----single-heap--------------------------------------------------------------
# Add artificial heap at age 40
age_with_40heap <- c(age_true, rep(40, 500))

# Correct only the heap at 40
age_fixed_40 <- correctSingleHeap(age_with_40heap,
                                   heap = 40,
                                   before = 3,  # range: 37-43
                                   after = 3,
                                   method = "lnorm",
                                   seed = 42)

# Check the counts at age 40
c(Before = sum(age_with_40heap == 40),
  After = sum(age_fixed_40 == 40))

## ----model-based-setup, message=FALSE-----------------------------------------
# Create a dataset with correlated variables following the paper's approach
set.seed(123)

# Generate age from log-normal distribution
age <- rlnorm(10000, meanlog = 2.466869, sdlog = 1.652772)
age <- round(age[age < 93 & age >= 18])
n <- length(age)

# Simulate covariates correlated with age
age_scaled <- scale(age)

# Income as a function of age with noise
income <- exp(3 + 0.5 * age_scaled + rnorm(n, 0, 0.5))

# Marital status influenced by age
marital_status <- ifelse(plogis(-3 + 0.05 * age_scaled + rnorm(n, 0, 0.5)) > 0.5,
                         "married", "single")

# Education level with age-dependent probabilities
get_education_prob <- function(a) {
  if (a < 25) return(c(0.5, 0.35, 0.15))
  else if (a < 40) return(c(0.3, 0.45, 0.25))
  else return(c(0.2, 0.4, 0.4))
}
education <- sapply(age, function(a) {
  sample(c("High School", "Bachelor", "Master"), 1, prob = get_education_prob(a))
})

data_example <- data.frame(
  age = age,
  income = income,
  marital_status = marital_status,
  education = education
)

# Introduce heaping with 27% heap ratio (as in the paper)
heap_ratio <- 0.27
indices_to_heap <- sample(n, round(heap_ratio * n))
data_example$age_heaped <- data_example$age
data_example$age_heaped[indices_to_heap] <- round(data_example$age_heaped[indices_to_heap] / 5) * 5

## ----model-based-correction, message=FALSE, warning=FALSE---------------------
# Model-based correction using income, education and marital status
if (requireNamespace("ranger", quietly = TRUE) &&
    requireNamespace("VIM", quietly = TRUE)) {

  # Also apply simple correction for comparison
  data_example$age_simple <- correctHeaps(
    data_example$age_heaped,
    heaps = "5year",
    method = "lnorm",
    seed = 42
  )

  # Model-based correction using covariates
  data_example$age_corrected <- correctHeaps(
    data_example$age_heaped,
    heaps = "5year",
    method = "lnorm",
    model = age_heaped ~ income + marital_status + education,
    dataModel = data_example,
    seed = 42
  )

  # Compare correlations with log(income)
  log_income <- log(data_example$income)
  cor_original <- cor(data_example$age, log_income)
  cor_heaped <- cor(data_example$age_heaped, log_income)
  cor_simple <- cor(data_example$age_simple, log_income)
  cor_corrected <- cor(data_example$age_corrected, log_income)

  cat("Correlation of age with log(income):\n")
  cat("  Original (true):", round(cor_original, 4), "\n")
  cat("  Heaped:", round(cor_heaped, 4), "\n")
  cat("  Simple correction:", round(cor_simple, 4), "\n")
  cat("  Model-based correction:", round(cor_corrected, 4), "\n")
}

## ----model-based-visualization, fig.cap="Comparison of heaped ages and model-based corrected ages", fig.height=6----
if (requireNamespace("ranger", quietly = TRUE) &&
    requireNamespace("VIM", quietly = TRUE)) {

  oldpar <- par(mfrow = c(2, 2))

  # Age distributions
  age_breaks <- seq(min(data_example$age) - 1, max(data_example$age) + 1, by = 1)
  hist(data_example$age_heaped, breaks = age_breaks, col = "coral",
       main = "Heaped Ages", xlab = "Age", border = "white")

  hist(data_example$age_corrected, breaks = age_breaks, col = "forestgreen",
       main = "Model-Based Corrected Ages", xlab = "Age", border = "white")

  # Age vs log(Income) relationships
  log_income <- log(data_example$income)
  plot(data_example$age_heaped, log_income,
       pch = 16, col = adjustcolor("coral", 0.3), cex = 0.5,
       main = "Heaped: Age vs log(Income)",
       xlab = "Age", ylab = "log(Income)")
  abline(lm(log_income ~ data_example$age_heaped), col = "darkred", lwd = 2)

  plot(data_example$age_corrected, log_income,
       pch = 16, col = adjustcolor("forestgreen", 0.3), cex = 0.5,
       main = "Corrected: Age vs log(Income)",
       xlab = "Age", ylab = "log(Income)")
  abline(lm(log_income ~ data_example$age_corrected), col = "darkgreen", lwd = 2)

  par(oldpar)
}

## ----multiple-imputation------------------------------------------------------
# Create 5 corrected datasets with different seeds
m <- 5
corrected_datasets <- lapply(1:m, function(i) {
  correctHeaps(age_heaped, heaps = "5year",
               method = "lnorm", seed = i * 100)
})

# Calculate Whipple index for each
whipple_values <- sapply(corrected_datasets, whipple)

cat("Whipple indices across imputations:\n")
cat("  Mean:", round(mean(whipple_values), 2), "\n")
cat("  SD:", round(sd(whipple_values), 2), "\n")
cat("  Range:", round(min(whipple_values), 2), "-",
    round(max(whipple_values), 2), "\n")

## ----fixed-observations-------------------------------------------------------
# Assume first 100 observations are verified
verified_indices <- 1:100

age_protected <- correctHeaps(age_heaped,
                              heaps = "5year",
                              method = "lnorm",
                              fixed = verified_indices,  # don't change these
                              seed = 42)

# Verify protected observations unchanged
all(age_heaped[verified_indices] == age_protected[verified_indices])

## ----sprague-example----------------------------------------------------------
# Example: population counts in 5-year groups
pop_5year <- c(
  1971990, 2095820, 2157190, 2094110, 2116580,  # 0-4, 5-9, ..., 20-24
  2003840, 1785690, 1502990, 1214170, 796934,   # 25-29, ..., 45-49
  627551, 530305, 488014, 364498, 259029,       # 50-54, ..., 70-74
  158047, 125941                                 # 75-79, 80+
)

# Disaggregate to single years
pop_single <- sprague(pop_5year)

# First 15 ages
head(pop_single, 15)

# Total is preserved
c(Sum_5year = sum(pop_5year), Sum_single = sum(pop_single))

## ----old-age-indices----------------------------------------------------------
# Create old-age data with heaping
set.seed(42)
old_ages <- c(sample(85:105, 3000, replace = TRUE),
              rep(c(90, 95, 100), each = 200))  # heaping at round ages

# Coale-Li index (designed for ages 60+)
coale_li(old_ages, digit = 0, ageMin = 85)

# Jdanov index (for very old ages like 95, 100, 105)
jdanov(old_ages, Agei = c(95, 100, 105))

# Kannisto index (for a single old age)
kannisto(old_ages, Agei = 90)
kannisto(old_ages, Agei = 95)

