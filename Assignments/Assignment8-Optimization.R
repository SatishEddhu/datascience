# Assignment 8: Problem 1 - section c) - Applying "batch gradient descent" algorithm

x1 = c(10,30,40,50,60,70)
y = c(40,60,70,90,80,70)

step_factor = 0.00007 # Do not increase step_factor (even to 0.00008). Otherwise, it does not converge.

w0 = 0
w1 = 0

error = 0
for (i in 1:length(y)) {
  error = error + (y[i] - w0 - w1*x1[i])^2
}
error = error/length(y)

printMetrics = sprintf("Initial: wo = %f, w1 = %f, error = %f", w0, w1, error)
printMetrics

minError = error
bestw0 = w0
bestw1 = w1

for (iteration in 1:80000) {
  gradient_w0 = gradient_w1 = 0
  for (i in 1:length(y)) {
    gradient_w0 = gradient_w0 + (-2)*(y[i] - w0 - w1*x1[i])
    gradient_w1 = gradient_w1 + (-2*x1[i])*(y[i] - w0 - w1*x1[i])  
  }
  
  w0 = w0 - (step_factor*gradient_w0)
  w1 = w1 - (step_factor*gradient_w1)
  
  error = 0
  for (i in 1:length(y)) {
    error = error + (y[i] - w0 - w1*x1[i])^2
  }
  error = error/length(y)
  
  printMetrics = sprintf("Iteration %d: wo = %f, w1 = %f, error = %f", iteration, w0, w1, error)
  printMetrics
  
  if (error < minError) {
    minError = error
    bestw0 = w0
    bestw1 = w1
  }
}

printMetrics = sprintf("Iteration %d: wo = %f, w1 = %f, error = %f", iteration, w0, w1, error)
printMetrics

# For the best-fit line in linear regression, the sum of residuals must be zero
y_estimate = sapply(x1, function(x) {bestw0 + bestw1*x})
residuals = y - y_estimate
residuals
sum(residuals) # sum of residuals is tending to zero. So, our approach is correct!!!
#######################################################################################

# Assignment 8: Problem 1 - section e) - Applying "stochastic gradient descent" algorithm

x1 = c(10,30,40,50,60,70)
y = c(40,60,70,90,80,70)

step_factor = 0.00007 # Do not increase step_factor (even to 0.00008). Otherwise, it does not converge.

w0 = 0
w1 = 0

error = 0
for (i in 1:length(y)) {
  error = error + (y[i] - w0 - w1*x1[i])^2
}
error = error/length(y)

printMetrics = sprintf("Initial: wo = %f, w1 = %f, error = %f", w0, w1, error)
printMetrics

# order in which the random samples are picked in stochastic gradient descent, one at a time
j = c(3,2,5,1)

for (iteration in 1:length(j)) {

  i = j[iteration]
  
  gradient_w0 = (-2)*(y[i] - w0 - w1*x1[i])
  gradient_w1 = (-2*x1[i])*(y[i] - w0 - w1*x1[i])  
  
  w0 = w0 - (step_factor*gradient_w0)
  w1 = w1 - (step_factor*gradient_w1)
  
  error = 0
  for (i in 1:length(y)) {
    error = error + (y[i] - w0 - w1*x1[i])^2
  }
  error = error/length(y)
  
  printMetrics = sprintf("Iteration %d: wo = %f, w1 = %f, error = %f", iteration, w0, w1, error)
  printMetrics
}

printMetrics = sprintf("Iteration %d: wo = %f, w1 = %f, error = %f", iteration, w0, w1, error)
printMetrics
################################################################################################

# Assignment 8: Problem 1 - section g) - Applying "mini-batch gradient descent" algorithm

x1 = c(10,30,40,50,60,70)
y = c(40,60,70,90,80,70)

step_factor = 0.00007 # Do not increase step_factor (even to 0.00008). Otherwise, it does not converge.

w0 = 0
w1 = 0

error = 0
for (i in 1:length(y)) {
  error = error + (y[i] - w0 - w1*x1[i])^2
}
error = error/length(y)

printMetrics = sprintf("Initial: wo = %f, w1 = %f, error = %f", w0, w1, error)
printMetrics

minError = error
bestw0 = w0
bestw1 = w1

for (iteration in 1:300000) {

  if(iteration %% 2 == 1) {
    j = c(1,4,5)
  } else {
    j = c(2,3,6)
  }
  
  gradient_w0 = gradient_w1 = 0
  for (index in 1:length(j)) {
    i = j[index]
    gradient_w0 = gradient_w0 + (-2)*(y[i] - w0 - w1*x1[i])
    gradient_w1 = gradient_w1 + (-2*x1[i])*(y[i] - w0 - w1*x1[i])  
  }
  
  w0 = w0 - (step_factor*gradient_w0)
  w1 = w1 - (step_factor*gradient_w1)
  
  error = 0
  for (i in 1:length(y)) {
    error = error + (y[i] - w0 - w1*x1[i])^2
  }
  error = error/length(y)
  
  printMetrics = sprintf("Iteration %d: wo = %f, w1 = %f, error = %f", iteration, w0, w1, error)
  printMetrics
}

printMetrics = sprintf("Iteration %d: wo = %f, w1 = %f, error = %f", iteration, w0, w1, error)
printMetrics
