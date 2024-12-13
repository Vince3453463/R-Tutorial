##############################################################
# R TUTORIAL ON USING THE optim FUNCTION AND OTHER OPTIMIZATION METHODS
#
# This tutorial provides an overview of how to use the built-in optimization 
# capabilities in R. We will cover:
# 1. The 'optim()' function for multivariate optimization.
# 2. Different optimization methods available in 'optim()' (Nelder-Mead, BFGS, CG, L-BFGS-B).
# 3. Additional optimization functions: 'optimize()', 'nlm()', and 'nlminb()'.
#
# All explanations are provided as comments (starting with #) so you can run 
# this entire file as is and inspect the results in the R console.
#
##############################################################


##############################################################
# 1. Introduction to Optimization Problems in R
#
# Optimization problems aim to find the parameter values (e.g., x) that 
# minimize (or maximize) a given objective function f(x).
#
# R provides multiple built-in functions for optimization:
# - optim(): general-purpose optimization for multi-parameter functions.
# - optimize(): for one-dimensional functions.
# - nlm(), nlminb(): alternative optimization routines.
#
##############################################################


##############################################################
# 2. The 'optim()' Function
#
# The 'optim()' function typically takes:
# - par: Initial guesses of the parameters.
# - fn: The objective function to minimize.
# - gr: (optional) The gradient function if available.
# - method: The optimization algorithm (e.g., "Nelder-Mead", "BFGS", "CG", "L-BFGS-B").
# - lower/upper: Bounds for "L-BFGS-B" method.
#
# We will demonstrate 'optim()' using a simple example:
# Suppose we want to find the minimum of the function:
# f(x) = (x[1] - 1)^2 + (x[2] - 2)^2
# The minimum is obviously at (1,2), where the value is 0.
#
##############################################################

# Define our two-dimensional function
f <- function(x) {
  (x[1] - 1)^2 + (x[2] - 2)^2
}

# Let's pick an initial guess, say c(0,0)
initial_par <- c(0,0)

# Use the default method (Nelder-Mead)
res_nelder <- optim(par = initial_par, fn = f)
res_nelder

# The result should be close to:
# $par: near (1,2)
# $value: near 0
# $convergence: 0 means success
# $counts: number of function evaluations


##############################################################
# 3. Using Different Methods in optim()
#
# 'optim()' supports several methods:
#
# - "Nelder-Mead": derivative-free, works well in many cases but can be slow.
# - "BFGS": uses gradient-based approach, can be faster if the function is smooth and differentiable.
# - "CG" (Conjugate Gradient): another gradient-based method.
# - "L-BFGS-B": allows for bounds on parameters.
#
##############################################################

# Let's define the gradient for our function:
gradient_f <- function(x) {
  # Gradient of (x1 - 1)^2 + (x2 - 2)^2 is:
  # df/dx1 = 2*(x1 - 1)
  # df/dx2 = 2*(x2 - 2)
  c(2*(x[1] - 1), 2*(x[2] - 2))
}

# Using the BFGS method with a gradient
res_bfgs <- optim(par = initial_par, fn = f, gr = gradient_f, method = "BFGS")
res_bfgs

# Using the CG method (also gradient-based)
res_cg <- optim(par = initial_par, fn = f, gr = gradient_f, method = "CG")
res_cg

# Using L-BFGS-B with bounds
# Let's impose bounds, say x1 >= 0, x2 >= 0
res_lbfgsb <- optim(par = initial_par, fn = f, gr = gradient_f, method = "L-BFGS-B",
                    lower = c(0,0), upper = c(10,10))
res_lbfgsb


##############################################################
# 4. One-Dimensional Optimization with 'optimize()'
#
# If your function depends on a single parameter (x is scalar), you can use 'optimize()'.
# 'optimize()' requires a continuous function and an interval [lower, upper].
#
# Example: minimize g(x) = (x - 3)^2
# We know the minimum is at x = 3.
#
##############################################################

g <- function(x) (x - 3)^2

# Use optimize on the interval [0,10]
res_optimize <- optimize(g, interval = c(0,10))
res_optimize
# This returns a list with the minimum point $minimum and the value $objective


##############################################################
# 5. Using 'nlm()' for Optimization
#
# 'nlm()' is another numeric minimization function that uses a Newton-type algorithm.
# It is best suited for unconstrained problems and requires a function that returns 
# just the value being minimized.
#
# Example: We'll reuse f(x) from above.
#
##############################################################

res_nlm <- nlm(f, p = c(0,0))
res_nlm
# $estimate should be near (1,2)
# $minimum near 0


##############################################################
# 6. Using 'nlminb()'
#
# 'nlminb()' is similar to 'optim()' with method "L-BFGS-B", but sometimes 
# it can give more stable results. It can also handle box constraints.
#
# Example: We'll try the same function with bounds (0,0) and (10,10).
#
##############################################################

res_nlminb <- nlminb(start = c(0,0), objective = f, lower = c(0,0), upper = c(10,10))
res_nlminb
# $par should be near (1,2)
# $objective near 0


##############################################################
# 7. Tips and Best Practices
#
# - Always provide good initial guesses to help optimization converge faster.
# - If you have analytic gradients, supply them to speed up optimization.
# - Check the 'convergence' status returned by these functions. A value of 0 
#   generally means successful convergence.
# - If the problem is multi-modal (many local minima), consider using multiple
#   starting points or global optimization methods (like 'GenSA', 'DEoptim', or 'GA' packages).
# - For bounded optimization, "L-BFGS-B" in 'optim()' or 'nlminb()' are often good choices.
#
##############################################################


##############################################################
# 8. Summary
#
# In this tutorial, we covered:
# - The 'optim()' function for multivariate optimization.
# - Different methods in 'optim()' and when to use them.
# - One-dimensional optimization with 'optimize()'.
# - Alternatives: 'nlm()' and 'nlminb()'.
#
# By understanding these functions, you can tackle a wide range of 
# optimization problems in R.
#
##############################################################
