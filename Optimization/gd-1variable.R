f = function(x) {
  1.2 * (x-2)^2 + 3.2
}

# gradient is f'(x) or d/dx[f(x)]
grad = function(x) {
  1.2 *2* (x-2)
}

xs = seq(0,4,len=20)
x11()
plot(xs,f(xs), type="l", xlab="x", ylab=expression(1.2 (x-2)^2 + 3.2))


# x = sample(0:4,1) # initial guess in [0,4] range
x = 3 # initial guess in [0,4] range
xtrace = x
ftrace = f(x)
stepFactor = 0.01 # if stepFactor = 1, 'x' values will diverge and not converge [and thus, so will 'f(x)' values]
iterations = 2000
threshold = 0.000001


# Gradient descent algorithm to find the 'minima' of f(x)
# We use two ways to stop this algorithm - 'max # of iterations' & 'closeness of successive f(x) values'
for (iter in 1:iterations) {
  # 'constant multiplier' approach to refine 'x' value that gives minimum f(x)
  # To find 'x' for maximum f(x), use '+' instead of '-'
  # For our f(x), only minima exists; maxima is infinity
  x = x - stepFactor*grad(x)
  xtrace = c(xtrace, x)
  ftrace = c(ftrace, f(x))
  
  # if successive values of f(x) are close enough, they have converged sufficiently. So, stop.
  if (iter > 1 && (abs(ftrace[iter] - ftrace[iter-1]) < threshold)) break
}

df = data.frame(x=xtrace, f=ftrace, g=grad(xtrace))
tail(df)
# Note how 'x' converges to 2.0, the value that minimizes f(x). Also note the corresponding converging to 3.2
lines(df$x, df$f, type="b", col="blue")
X11()
# Note how f(x) converges to minimum f(x) value of 3.2
plot(1:(iterations+1),df$f[1:(iterations+1)],col="blue",xlab="Iterations",ylab="function",main="Convergence")
