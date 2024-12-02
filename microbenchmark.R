library(microbenchmark)


# Example functions
slow_function <- function(x) {
  for (i in 1:x) {
    sqrt(i)
  }
}

fast_function <- function(x) {
  sapply(1:x, sqrt)
}


benchmark_results <- microbenchmark(
  Slow = slow_function(1000),
  Fast = fast_function(1000),
  times = 100  # Number of iterations for benchmarking
)

benchmark_results
boxplot(benchmark_results)
summary(benchmark_results)
