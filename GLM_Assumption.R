glm_assumption_check <- function(data,
                                 model,
                                 distribution = c("binomial", "poisson", "gaussian", "Gamma"),
                                 alpha = 0.05) {
  # Make sure the distribution argument is recognized
  distribution <- match.arg(distribution)
  
  # Load required packages
  # install.packages("DHARMa")  # for residual diagnostics in GLMs
  # install.packages("lmtest")  # for LR tests
  # install.packages("car")     # for some additional tests
  library(DHARMa)
  library(lmtest)
  library(car)
  
  cat("========================================================\n")
  cat("GLM ASSUMPTION CHECK for:", distribution, "distribution\n")
  cat("========================================================\n\n")
  
  # 1. Basic model summary
  cat("1. MODEL SUMMARY\n")
  cat("----------------\n")
  cat("Call:\n")
  print(model$call)
  cat("\nCoefficients:\n")
  print(coef(summary(model)))
  cat("\n")
  
  # 2. Residual diagnostics (using DHARMa for simulated residuals)
  cat("2. RESIDUAL DIAGNOSTICS (DHARMa)\n")
  cat("--------------------------------\n")
  
  # Simulate residuals
  sim_res <- simulateResiduals(fittedModel = model, plot = FALSE)
  
  # Plot DHARMa residual diagnostics
  # This produces multiple diagnostic plots in one window
  par(mfrow = c(2, 2))
  plot(sim_res)
  par(mfrow = c(1, 1))
  cat("Plotted DHARMa residual diagnostics.\n\n")
  
  # Test for overall uniformity of residuals
  uniform_test <- testUniformity(sim_res)
  cat("Uniformity test (DHARMa):\n")
  print(uniform_test)
  if (uniform_test$p.value < alpha) {
    cat("=> Possible violation of distributional assumptions (non-uniform residuals)\n\n")
  } else {
    cat("=> No significant deviation from uniform residuals\n\n")
  }
  
  # Test for outliers
  outlier_test <- testOutliers(sim_res)
  cat("Outlier test (DHARMa):\n")
  print(outlier_test)
  if (outlier_test$p.value < alpha) {
    cat("=> Potential outliers detected\n\n")
  } else {
    cat("=> No significant outliers detected\n\n")
  }
  
  # 3. Check independence of observations (serial correlation) with Durbin-Watson (if makes sense)
  # Note: This is primarily for linear or continuous-time data; for GLMs, might be less typical.
  cat("3. INDEPENDENCE OF OBSERVATIONS\n")
  cat("--------------------------------\n")
  # Durbin-Watson test is not always relevant for discrete distributions, but let's show it.
  # It also requires numeric residuals (so let's use raw or Pearson).
  dw <- tryCatch(
    dwtest(residuals(model, type = "pearson") ~ fitted(model)),
    error = function(e) NA
  )
  if (inherits(dw, "htest")) {
    print(dw)
    if (dw$p.value < alpha) {
      cat("=> Possible autocorrelation (reject H0 of no autocorrelation)\n\n")
    } else {
      cat("=> No significant autocorrelation detected\n\n")
    }
  } else {
    cat("Durbin-Watson test not available for this model.\n\n")
  }
  
  # 4. Overdispersion check (important for Poisson or Binomial)
  cat("4. OVERDISPERSION CHECK\n")
  cat("-----------------------\n")
  
  # A common quick check is the ratio of the residual deviance to the degrees of freedom:
  deviance_value <- deviance(model)
  df_residual <- df.residual(model)
  dispersion_ratio <- deviance_value / df_residual
  cat("Residual deviance:", deviance_value, "\n")
  cat("Degrees of freedom:", df_residual, "\n")
  cat("Dispersion ratio:  ", round(dispersion_ratio, 2), "\n")
  
  if (distribution %in% c("binomial", "poisson")) {
    # For binomial/poisson, if ratio >> 1 => overdispersion
    # This is a rough rule of thumb.
    if (dispersion_ratio > 1.5) {
      cat("=> Potential overdispersion (ratio > 1.5). Consider quasi- or NB/Beta-Binomial.\n\n")
    } else {
      cat("=> No strong evidence of overdispersion based on ratio.\n\n")
    }
  } else {
    cat("=> Overdispersion check is most relevant for binomial/poisson.\n\n")
  }
  
  # 5. Distribution-specific checks or notes
  cat("5. DISTRIBUTION-SPECIFIC NOTES\n")
  cat("-----------------------------\n")
  if (distribution == "binomial") {
    cat("* Check that data are truly binomial (e.g. 0/1 or # of successes out of n).\n")
    cat("* Consider overdispersion or correlation if repeated measures.\n\n")
  } else if (distribution == "poisson") {
    cat("* Check if there is an excess of zeros or overdispersion.\n")
    cat("* If overdispersion is present, consider quasi-Poisson, negative binomial, or zero-inflated models.\n\n")
  } else if (distribution == "gaussian") {
    cat("* This is essentially a linear model with possibly a non-identity link.\n")
    cat("* Check normality of residuals (DHARMa does a simulation-based check).\n\n")
  } else if (distribution == "Gamma") {
    cat("* Data must be strictly positive.\n")
    cat("* Check the link function (often 'log' or 'inverse').\n")
    cat("* Overdispersion can sometimes be addressed with quasi-likelihood or other families.\n\n")
  }
  
  cat("========================================================\n")
  cat("ASSUMPTION CHECK COMPLETE\n")
  cat("========================================================\n\n")
  
  # Return a list of test objects in case the user wants them
  invisible(list(
    uniform_test = uniform_test,
    outlier_test = outlier_test,
    dw_test = dw,
    dispersion_ratio = dispersion_ratio
  ))
}

### notes
# DHARMa is a simulation-based approach to residual diagnostics in (G)LMs. It provides a unified way to
# detect issues like overdispersion, zero inflation, or non-uniform residual patterns without relying too heavily
# on distribution-specific residual definitions.
# Overdispersion checks are particularly important for Poisson and Binomial models. If the ratio of deviance to residual
# degrees of freedom is significantly greater than 1 (say, >1.5 or 2), it indicates potential overdispersion.
# For zero inflation, you might look at specific zero-inflation tests (DHARMa also has a testZeroInflation function).
# For binomial data, ensure that your input is either a factor for a binary outcome (0/1) or a two-column matrix with
# successes and failures, or a proportion plus a weights argument indicating the total number of trials.
# If observations are not independent (e.g., repeated measures, panel data), you would need to consider mixed models (GLMM) 
# or GEE for correlated data structures, rather than a standard GLM.


### Example
data("mtcars")
# Convert cyl to a count-like variable just for a demonstration
# (Though 'cyl' in mtcars is not truly count data in a Poisson sense)
mod_pois <- glm(cyl ~ wt + hp, data = mtcars, family = poisson)


glm_assumption_check(data = mtcars, 
                     model = mod_pois, 
                     distribution = "poisson", 
                     alpha = 0.05)
