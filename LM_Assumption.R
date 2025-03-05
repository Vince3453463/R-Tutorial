assumption_check <- function(data, model, alpha = 0.05) {
  
  # Required packages:
  # install.packages(c("car", "lmtest"))
  library(car)      # for VIF, ncvTest
  library(lmtest)   # for bptest, dwtest
  
  # Extract basic elements
  residuals <- resid(model)
  fitted_values <- fitted(model)
  model_df <- df.residual(model)    # degrees of freedom of residuals
  
  cat("=================================\n")
  cat("   1. MODEL SUMMARY (Descriptive)\n")
  cat("=================================\n\n")
  
  # 1a. Print the model call and summary
  cat("Call:\n")
  print(model$call)
  cat("\nSummary of the model:\n")
  print(summary(model))
  
  # 1b. Residuals descriptive statistics
  cat("\nResiduals Descriptive Statistics:\n")
  print(summary(residuals))
  cat("Standard Deviation of Residuals:", sd(residuals), "\n\n")
  
  cat("=================================\n")
  cat("   2. MULTICOLLINEARITY (VIF)\n")
  cat("=================================\n\n")
  
  # 2. Check for multicollinearity using Variance Inflation Factor
  #    Note: VIFs > 5 or 10 can indicate problematic multicollinearity.
  if (length(coef(model)) > 2) {
    vif_values <- vif(model)
    print(vif_values)
  } else {
    cat("Model has only 1 predictor (besides intercept). VIF not applicable.\n")
  }
  
  cat("\n=================================\n")
  cat("   3. RESIDUAL vs. FITTED (Linearity, Homoscedasticity)\n")
  cat("=================================\n\n")
  
  # 3. Plot residuals vs fitted values
  #    Ideally, we want no clear pattern (random scatter).
  op <- par(mfrow = c(2, 2))  # Display 4 standard diagnostic plots
  plot(model)                 # Base R's built-in diagnostic plots
  par(op)
  
  cat("Plots produced:\n")
  cat(" - Residuals vs Fitted\n")
  cat(" - Normal Q-Q\n")
  cat(" - Scale-Location\n")
  cat(" - Residuals vs Leverage\n\n")
  cat("Check these plots visually for violations of assumptions.\n\n")
  
  cat("=================================\n")
  cat("   4. NORMALITY OF RESIDUALS\n")
  cat("=================================\n\n")
  
  # 4. Statistical test for normality: Shapiro-Wilk test
  #    Null hypothesis: residuals are normally distributed.
  if (model_df < 3) {
    cat("Shapiro-Wilk test not valid for n < 3.\n")
  } else {
    shapiro_test <- shapiro.test(residuals)
    print(shapiro_test)
    if (shapiro_test$p.value < alpha) {
      cat("=> The residuals do NOT appear normal (reject H0 at alpha =", alpha, ")\n\n")
    } else {
      cat("=> The residuals appear normal (fail to reject H0 at alpha =", alpha, ")\n\n")
    }
  }
  
  cat("=================================\n")
  cat("   5. HOMOSCEDASTICITY (Constant Variance)\n")
  cat("=================================\n\n")
  
  # 5a. Breusch-Pagan test (lmtest package)
  #     Null hypothesis: homoscedasticity (constant variance).
  bp_test <- bptest(model)
  print(bp_test)
  if (bp_test$p.value < alpha) {
    cat("=> Heteroscedasticity detected (reject H0 at alpha =", alpha, ")\n\n")
  } else {
    cat("=> No strong evidence of heteroscedasticity (fail to reject H0)\n\n")
  }
  
  # 5b. Alternatively, ncvTest from car package
  #     Null hypothesis: error variance is constant.
  ncv_test <- ncvTest(model)
  print(ncv_test)
  if (ncv_test$p < alpha) {
    cat("=> Heteroscedasticity detected (reject H0 at alpha =", alpha, ")\n\n")
  } else {
    cat("=> No strong evidence of heteroscedasticity (fail to reject H0)\n\n")
  }
  
  cat("=================================\n")
  cat("   6. AUTOCORRELATION OF ERRORS\n")
  cat("=================================\n\n")
  
  # 6. Durbin-Watson test (lmtest package)
  #    Null hypothesis: no autocorrelation.
  dw_test <- dwtest(model)
  print(dw_test)
  if (dw_test$p.value < alpha) {
    cat("=> Autocorrelation in residuals (reject H0 at alpha =", alpha, ")\n\n")
  } else {
    cat("=> No significant autocorrelation (fail to reject H0)\n\n")
  }
  
  cat("========================================\n")
  cat("   ASSUMPTION CHECK COMPLETE\n")
  cat("========================================\n\n")
  
  # Return a list of test results for further inspection if needed
  invisible(list(
    shapiro_test = if (model_df >= 3) shapiro_test else NA,
    bp_test      = bp_test,
    ncv_test     = ncv_test,
    dw_test      = dw_test,
    vif_values   = if (length(coef(model)) > 2) vif_values else NULL
  ))
}

### notes
# For linearity and homoscedasticity, visual inspection of the Residuals vs Fitted and Scale-Location plots is critical.
# For normality of residuals in large samples, minor deviations are often not a big problem due to the Central Limit Theorem.
# For autocorrelation, the Durbin-Watson test is straightforward for typical regression data; 
# for time series or panel data, more specialized methods might be needed.
# For multicollinearity, if your model has more than one predictor, check the reported VIF values. (VIF > 5 or 10 is often considered a problem.)
# If any assumption is violated, consider using robust standard errors, transformations,
# additional predictors, or alternative modeling approaches as appropriate.


#### Example 
data(mtcars)

# Fit a simple linear model
mod <- lm(mpg ~ wt + hp, data = mtcars)
assumption_check(mtcars, mod)


modlog = lm(log10(mpg) ~ wt + hp, data = mtcars)
assumption_check(mtcards, modlog)



assumption_check <- function(data, model, alpha = 0.05) {
  
  # Required packages:
  # install.packages(c("car", "lmtest"))
  library(car)      # for VIF, ncvTest
  library(lmtest)   # for bptest, dwtest
  
  # Extract basic elements
  residuals <- resid(model)
  fitted_values <- fitted(model)
  model_df <- df.residual(model)    # degrees of freedom of residuals
  
  cat("=================================\n")
  cat("   1. MODEL SUMMARY (Descriptive)\n")
  cat("=================================\n\n")
  
  # 1a. Print the model call and summary
  cat("Call:\n")
  print(model$call)
  cat("\nSummary of the model:\n")
  print(summary(model))
  
  # 1b. Residuals descriptive statistics
  cat("\nResiduals Descriptive Statistics:\n")
  print(summary(residuals))
  cat("Standard Deviation of Residuals:", sd(residuals), "\n\n")
  
  cat("=================================\n")
  cat("   2. MULTICOLLINEARITY (VIF)\n")
  cat("=================================\n\n")
  
  # 2. Check for multicollinearity using Variance Inflation Factor
  #    Note: VIFs > 5 or 10 can indicate problematic multicollinearity.
  if (length(coef(model)) > 2) {
    vif_values <- vif(model)
    print(vif_values)
  } else {
    cat("Model has only 1 predictor (besides intercept). VIF not applicable.\n")
  }
  
  cat("\n=================================\n")
  cat("   3. RESIDUAL vs. FITTED (Linearity, Homoscedasticity)\n")
  cat("=================================\n\n")
  
  # 3. Plot residuals vs fitted values
  #    Ideally, we want no clear pattern (random scatter).
  op <- par(mfrow = c(2, 2))  # Display 4 standard diagnostic plots
  plot(model)                 # Base R's built-in diagnostic plots
  par(op)
  
  cat("Plots produced:\n")
  cat(" - Residuals vs Fitted\n")
  cat(" - Normal Q-Q\n")
  cat(" - Scale-Location\n")
  cat(" - Residuals vs Leverage\n\n")
  cat("Check these plots visually for violations of assumptions.\n\n")
  
  cat("=================================\n")
  cat("   4. NORMALITY OF RESIDUALS\n")
  cat("=================================\n\n")
  
  # 4. Statistical test for normality: Shapiro-Wilk test
  #    Null hypothesis: residuals are normally distributed.
  if (model_df < 3) {
    cat("Shapiro-Wilk test not valid for n < 3.\n")
  } else {
    shapiro_test <- shapiro.test(residuals)
    print(shapiro_test)
    if (shapiro_test$p.value < alpha) {
      cat("=> The residuals do NOT appear normal (reject H0 at alpha =", alpha, ")\n\n")
    } else {
      cat("=> The residuals appear normal (fail to reject H0 at alpha =", alpha, ")\n\n")
    }
  }
  
  cat("=================================\n")
  cat("   5. HOMOSCEDASTICITY (Constant Variance)\n")
  cat("=================================\n\n")
  
  # 5a. Breusch-Pagan test (lmtest package)
  #     Null hypothesis: homoscedasticity (constant variance).
  bp_test <- bptest(model)
  print(bp_test)
  if (bp_test$p.value < alpha) {
    cat("=> Heteroscedasticity detected (reject H0 at alpha =", alpha, ")\n\n")
  } else {
    cat("=> No strong evidence of heteroscedasticity (fail to reject H0)\n\n")
  }
  
  # 5b. Alternatively, ncvTest from car package
  #     Null hypothesis: error variance is constant.
  ncv_test <- ncvTest(model)
  print(ncv_test)
  if (ncv_test$p < alpha) {
    cat("=> Heteroscedasticity detected (reject H0 at alpha =", alpha, ")\n\n")
  } else {
    cat("=> No strong evidence of heteroscedasticity (fail to reject H0)\n\n")
  }
  
  cat("=================================\n")
  cat("   6. AUTOCORRELATION OF ERRORS\n")
  cat("=================================\n\n")
  
  # 6. Durbin-Watson test (lmtest package)
  #    Null hypothesis: no autocorrelation.
  dw_test <- dwtest(model)
  print(dw_test)
  if (dw_test$p.value < alpha) {
    cat("=> Autocorrelation in residuals (reject H0 at alpha =", alpha, ")\n\n")
  } else {
    cat("=> No significant autocorrelation (fail to reject H0)\n\n")
  }
  
  cat("========================================\n")
  cat("   ASSUMPTION CHECK COMPLETE\n")
  cat("========================================\n\n")
  
  # Return a list of test results for further inspection if needed
  invisible(list(
    shapiro_test = if (model_df >= 3) shapiro_test else NA,
    bp_test      = bp_test,
    ncv_test     = ncv_test,
    dw_test      = dw_test,
    vif_values   = if (length(coef(model)) > 2) vif_values else NULL
  ))
}
