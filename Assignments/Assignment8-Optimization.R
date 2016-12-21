# Assignment 8: Problem 1 - section c) - Applying "batch gradient descent" algorithm

display.trace = function(solution) {
  solution
}

error = function(b,m,data) {
  totalError = 0
  for(i in 1:nrow(data)) {
    x = data[i,1]
    y = data[i,2]
    totalError = totalError + (y - (m * x + b)) ^ 2
  }
  totalError / nrow(data)
}

batch_gradient = function(data, b_current, m_current) {
  b_gradient = 0
  m_gradient = 0
  N = nrow(data)
  for (i in 1:nrow(data)) {
    x = data[i, 1]
    y = data[i, 2]
    b_gradient = b_gradient - 2 * (y - ((m_current * x) + b_current))
    m_gradient = m_gradient - 2 * x * (y - ((m_current * x) + b_current))
  }
  c(b_gradient/N, m_gradient/N)
}

batch_gradient_descent = function(data, learningRate, iterations) {
  b_current = 0
  m_current = 0
  
  b_trace = b_current
  m_trace = m_current
  error_trace = error(b_current, m_current, data)
  
  for(iter in 1:iterations) {
    grad = batch_gradient(data, b_current, m_current)
    b_current = b_current - (learningRate * grad[1])
    m_current = m_current - (learningRate * grad[2])
    error_current = error(b_current, m_current, data)
    
    b_trace = c(b_trace, b_current)
    m_trace = c(m_trace, m_current)
    error_trace = c(error_trace,error_current)
  }
  
  data.frame(intercept = b_trace, slope = m_trace, error = error_trace)
}

x = c(10,30,40,50,60,70)
y = c(40,60,70,90,80,70)
data = data.frame(x, y)

iterations = 8
stepsize = 0.00042

solution = batch_gradient_descent(data, stepsize, iterations)
display.trace(solution)
#######################################################################################

# Assignment 8: Problem 1 - section e) - Applying "stochastic gradient descent" algorithm

stochastic_gradient = function(data, b_current, m_current, random_index) {
  N = nrow(data)
  
  if (random_index == -1) {
      random_point_ind = sample(1:N,1)
  } else {
      random_point_ind = random_index
  }

  x = data[random_point_ind, 1]
  y = data[random_point_ind, 2]
  b_gradient = - 2 * (y - ((m_current * x) + b_current))
  m_gradient = - 2 * x * (y - ((m_current * x) + b_current))
  
  c(b_gradient/N, m_gradient/N)
}

stochastic_gradient_descent = function(data, learningRate, iterations, randomOrder) {
  b_current = 0
  m_current = 0
  
  b_trace = b_current
  m_trace = m_current
  error_trace = error(b_current, m_current, data)
  
  rand_size = length(randomOrder)
  
  for(iter in 1:iterations) {
    
    if (iter <= rand_size) {
      random_index = randomOrder[iter]
    }
    else {
      random_index = -1
    }
    
    grad = stochastic_gradient(data, b_current, m_current, random_index)
    b_current = b_current - (learningRate * grad[1])
    m_current = m_current - (learningRate * grad[2])
    error_current = error(b_current, m_current, data)
    
    b_trace = c(b_trace, b_current)
    m_trace = c(m_trace, m_current)
    error_trace = c(error_trace,error_current)
  }
  
  data.frame(intercept = b_trace, slope = m_trace, error = error_trace)
}


x = c(10,30,40,50,60,70)
y = c(40,60,70,90,80,70)
data = data.frame(x, y)

iterations = 10
stepsize = 0.00042

# order in which the random samples are picked in stochastic gradient descent, one at a time
randomOrder = c(3,2,5,1)

solution = stochastic_gradient_descent(data, stepsize, iterations, randomOrder)
display.trace(solution)
################################################################################################

# Assignment 8: Problem 1 - section g) - Applying "mini-batch gradient descent" algorithm

stochastic_batch_gradient = function(data, b_current, m_current, batch_index) {
  N = nrow(data)
  
  b_gradient = 0
  m_gradient = 0
  
  if (batch_index[1] == -1) {
    #Lets take a random batch of size 3
    random_batch_ind = sample(1:N,3)
  } else {
    random_batch_ind = batch_index
  }

  for(i in 1:length(random_batch_ind)) {
    x = data[random_batch_ind[i], 1]
    y = data[random_batch_ind[i], 2]
    b_gradient = b_gradient - 2 * (y - ((m_current * x) + b_current))
    m_gradient = m_gradient - 2 * x * (y - ((m_current * x) + b_current))
  }
  c(b_gradient/N, m_gradient/N)
}

stochastic_batch_gradient_descent = function(data, learningRate, iterations, batchOrder) {
  b_current = 0
  m_current = 0
  
  b_trace = b_current
  m_trace = m_current
  error_trace = error(b_current, m_current, data)
  
  batch_size = length(batchOrder)
  
  for(iter in 1:iterations) {
    
    if (iter <= batch_size) {
      batch_index = batchOrder[[iter]]
    }
    else {
      batch_index = c(-1)
    }
    
    grad = stochastic_batch_gradient(data, b_current, m_current, batch_index)
    b_current = b_current - (learningRate * grad[1])
    m_current = m_current - (learningRate * grad[2])
    error_current = error(b_current, m_current, data)
    
    b_trace = c(b_trace, b_current)
    m_trace = c(m_trace, m_current)
    error_trace = c(error_trace,error_current)
  }
  
  data.frame(intercept = b_trace, slope = m_trace, error = error_trace)
}

x = c(10,30,40,50,60,70)
y = c(40,60,70,90,80,70)
data = data.frame(x, y)

iterations = 10
stepsize = 0.00042

# order in which the random batches are picked in stochastic min-batch gradient descent
randomOrder = list(c(1,4,5), c(2,1,6), c(3,4,5))

solution = stochastic_batch_gradient_descent(data, stepsize, iterations, randomOrder)
display.trace(solution)
