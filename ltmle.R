
# 4th Summer Institute in Statistics for Clinical Research
# Module 14: Modern Statistical Learning Methods for Observational Biomedical Data and Applications to Comparative Effectiveness Research
# Thu, July 27
# Instructor(s):
#   David Benkeser, Marco Carone, Larry Kessler

#Modern Statistical Learning Methods for Observational Biomedical Data
#https://si.biostat.washington.edu/sites/default/files/modules/lab3.pdf

# set a seed for reproducibility
set.seed(212)
# sample size
n <- 300
# W1 has Normal distribution, W2 has Uniform distribution
W1 <- rnorm(n); W2 <- runif(n)
# make a data.frame of covariates
W <- data.frame(W1 = W1, W2 = W2)
# pr(A = 1 | W) is logistic linear in W
g1W <- plogis(-1 + W1 - W2 + W1*W2)
# generate binary treatment
A <- rbinom(n, 1, g1W)
# E[Y | A, W] is logistic linear in A, W
QAW <- plogis(W1 - W2 + A)
# generate outcome by adding random error
Y <- rbinom(n, 1, QAW)

cbind(W1, W, g1W, A, QAW, Y)

"save-est-sim-setting-1.rds"
"save-est-sim-setting-2 (2).rds"

readRDS("___prueba/TMLE_course_pph.rds")

readRDS("___prueba/save-est-sim-setting-1.rds")

readRDS("___prueba/save-est-sim-setting-2 (1).rds")
