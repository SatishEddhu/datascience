# install.packages("plot3D")
library(plot3D)

fun = function(x,y) {
  (x^2 + y^2)
}

# gradient is the vector {d/dx[f(x,y)], d/dy[f(x,y)]}
grad = function(x,y) {
  c(2*x, 2*y)
}

x = seq(-2, 4, 0.25) # length is 25
y = seq(-2, 4, 0.25) # length is 25
# Outer product of 2 arrays. Dimension is (25 x 25). f(x,y) = fun(x,y)
f = outer(x,y,fun)
x11()
persp3D(x, y, f, xlab="x", ylab="y", zlab="f",col = ramp.col(n = 50,col = c("#FF033E", "#FFBF00", "#FF7E00", "#08E8DE", "#00FFFF", "#03C03C"),alpha = .1), border = "#808080", theta = 30, phi = 10, colkey = TRUE)
# theta = horizontal angle, phi = vertical angle, alpha = color depth


xy = c(3, 2) # initial guess in [(-2,-2), (4,4)] range
xytrace = xy
ftrace = fun(xy[1], xy[2])
stepFactor = 0.01 # if stepFactor = 1, 'x' values will diverge and not converge [and thus, so will 'f(x)' values]
iterations = 2000
threshold = 0.000001

# Gradient descent algorithm to find the 'minima' of f(x,y)
# We use two ways to stop this algorithm - 'max # of iterations' & 'closeness of successive f(x,y) values'
for (iter in 1:iterations) {
  # 'constant multiplier' approach to refine 'x,y' value that gives minimum f(x,y)
  # To find 'x,y' for maximum f(x,y), use '+' instead of '-'
  # For our f(x,y), only minima exists; maxima is infinity
  xy = xy - stepFactor*grad(xy[1],xy[2])
  xytrace = rbind(xytrace, xy)
  ftrace = c(ftrace, fun(xy[1],xy[2]))
  
  # if successive values of f(x) are close enough, they have converged sufficiently. So, stop.
  if (iter > 1 && (abs(ftrace[iter] - ftrace[iter-1]) < threshold)) break
}

dim(xytrace)
length(ftrace)

df = data.frame(x=xytrace[,1], y=xytrace[,2], f=ftrace)
head(df)
tail(df)
# Note how '(x,y)' converges to '(0,0)' the value that minimizes f(x,y). Also note the corresponding converge to 0.
points3D(df$x, df$y, df$f, pch=20, col='red', add=TRUE)
X11()
# Note how f(x,y) converges to minimum f(x,y) value of 0
plot(1:(iterations+1),df$f[1:(iterations+1)],col="blue",xlab="Iterations",ylab="function",main="Convergence")
