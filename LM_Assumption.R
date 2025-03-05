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


#################### mixed models ##################################

check_mixed_model_assumptions <- function(model, 
                                          distribution = c("gaussian", "binomial", "poisson", "Gamma"),
                                          alpha = 0.05) {
  # Load necessary packages
  if (!requireNamespace("lme4", quietly = TRUE)) install.packages("lme4")
  if (!requireNamespace("DHARMa", quietly = TRUE)) install.packages("DHARMa")
  if (!requireNamespace("performance", quietly = TRUE)) install.packages("performance")
  if (!requireNamespace("car", quietly = TRUE)) install.packages("car")
  
  library(lme4)
  library(DHARMa)
  library(performance)
  library(car)
  
  distribution <- match.arg(distribution)
  
  cat("\n===========================================\n")
  cat("   Mixed Model Assumption Check\n")
  cat("===========================================\n\n")
  
  # Model summary
  cat("1. MODEL SUMMARY\n")
  cat("-----------------\n")
  print(summary(model))
  cat("\n")
  
  # Extract residuals
  residuals_raw <- residuals(model)
  fitted_vals <- fitted(model)
  
  # Check residuals normality (for LMMs)
  if (distribution == "gaussian") {
    cat("2. NORMALITY OF RESIDUALS (for LMMs)\n")
    cat("-----------------------------------\n")
    shapiro_test <- shapiro.test(residuals_raw)
    print(shapiro_test)
    
    if (shapiro_test$p.value < alpha) {
      cat("=> Residuals deviate significantly from normality (p < ", alpha, ")\n\n")
    } else {
      cat("=> Residuals appear normally distributed.\n\n")
    }
    
    # QQ plot and histogram
    par(mfrow = c(1, 2))
    qqnorm(residuals_raw, main = "QQ Plot of Residuals")
    qqline(residuals_raw, col = "red")
    hist(residuals_raw, main = "Histogram of Residuals", xlab = "Residuals", col = "lightblue", border = "black")
    par(mfrow = c(1, 1))
    
  } else {
    # Use DHARMa for GLMM residual checks
    cat("2. RESIDUAL DIAGNOSTICS (for GLMMs)\n")
    cat("-----------------------------------\n")
    sim_res <- simulateResiduals(model, plot = FALSE)
    
    par(mfrow = c(2, 2))
    plot(sim_res)
    par(mfrow = c(1, 1))
    
    # Test uniformity of residuals
    uniform_test <- testUniformity(sim_res)
    print(uniform_test)
    if (uniform_test$p.value < alpha) {
      cat("=> Possible violation of model assumptions (non-uniform residuals)\n\n")
    } else {
      cat("=> No significant deviation from uniform residuals.\n\n")
    }
    
    # Test for outliers
    outlier_test <- testOutliers(sim_res)
    print(outlier_test)
    if (outlier_test$p.value < alpha) {
      cat("=> Potential outliers detected.\n\n")
    } else {
      cat("=> No significant outliers detected.\n\n")
    }
  }
  
  # Homoscedasticity check (only for LMMs)
  if (distribution == "gaussian") {
    cat("3. HOMOSCEDASTICITY (Constant Variance) CHECK\n")
    cat("----------------------------------------------\n")
    plot(fitted_vals, residuals_raw, main = "Residuals vs Fitted", 
         xlab = "Fitted Values", ylab = "Residuals", col = "blue", pch = 20)
    abline(h = 0, col = "red", lwd = 2)
    
    levene_test <- leveneTest(residuals_raw ~ as.factor(fitted_vals > median(fitted_vals)))
    print(levene_test)
    if (levene_test$`Pr(>F)`[1] < alpha) {
      cat("=> Evidence of heteroscedasticity (p < ", alpha, ")\n\n")
    } else {
      cat("=> Residual variance appears homoscedastic.\n\n")
    }
  }
  
  # Overdispersion check for Poisson & Binomial GLMMs
  if (distribution %in% c("binomial", "poisson")) {
    cat("4. OVERDISPERSION CHECK\n")
    cat("-----------------------\n")
    
    # Compute dispersion ratio
    deviance_value <- deviance(model)
    df_residual <- df.residual(model)
    dispersion_ratio <- deviance_value / df_residual
    cat("Residual Deviance:", deviance_value, "\n")
    cat("Degrees of Freedom:", df_residual, "\n")
    cat("Dispersion Ratio: ", round(dispersion_ratio, 2), "\n")
    
    if (dispersion_ratio > 1.5) {
      cat("=> Potential overdispersion detected (Ratio > 1.5). Consider negative binomial or quasi-likelihood models.\n\n")
    } else {
      cat("=> No strong evidence of overdispersion.\n\n")
    }
  }
  
  # Zero-inflation check (for Poisson & binomial)
  if (distribution %in% c("binomial", "poisson")) {
    cat("5. ZERO-INFLATION CHECK\n")
    cat("------------------------\n")
    
    zero_test <- testZeroInflation(sim_res)
    print(zero_test)
    
    if (zero_test$p.value < alpha) {
      cat("=> Excess zeros detected. Consider a zero-inflated model.\n\n")
    } else {
      cat("=> No strong evidence of zero inflation.\n\n")
    }
  }
  
  # Random effects summary
  cat("6. RANDOM EFFECTS SUMMARY\n")
  cat("-------------------------\n")
  print(VarCorr(model))
  cat("\n")
  
  cat("===========================================\n")
  cat("    ASSUMPTION CHECK COMPLETE\n")
  cat("===========================================\n\n")
  
  # Return results
  return(invisible(list(
    shapiro_test = if (distribution == "gaussian") shapiro_test else NULL,
    uniform_test = if (distribution != "gaussian") uniform_test else NULL,
    outlier_test = if (distribution != "gaussian") outlier_test else NULL,
    levene_test = if (distribution == "gaussian") levene_test else NULL,
    dispersion_ratio = if (distribution %in% c("binomial", "poisson")) dispersion_ratio else NULL,
    zero_test = if (distribution %in% c("binomial", "poisson")) zero_test else NULL
  )))
}

