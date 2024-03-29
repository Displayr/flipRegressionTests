context("Regression with 100,000 cases")

vehicles <- readRDS("vehicles100kNoNA.rds")
vehicles$region_weight <- vapply(as.character(vehicles$region),
                                 function(x) switch(x,
                                                    "Midwest" = 1.02,
                                                    "Southeast" = 0.52,
                                                    "West" = 1.09,
                                                    "Southwest" = 1.68,
                                                    "East" = 1.18),
                                 0.0)
set.seed(111)
p.noise <- 15
noise <- as.data.frame(replicate(p.noise, rnorm(nrow(vehicles))))
colnames(noise) <- paste0("noise", 1:p.noise)
vehicles <- cbind(vehicles, noise)

test_that("Weighted logistic regression", {
    vehicles$great.condition <- factor(as.integer(vehicles$condition == "excellent" |
                                           vehicles$condition == "like new"))
    expect_is(Regression(great.condition~.-region_weight-condition,
                         weight = region_weight,
                         data = vehicles, type = "Binary Logit"),
              "BinaryLogitRegression")
    expect_is(Regression(great.condition~drive+region+year,
                         weight = region_weight, output = "Jaccard Coefficient",
                         data = vehicles, type = "Linear"),
              "LinearRegression")

    vehicles$great.condition <- NULL
})

test_that("Weighted regression with count response", {
    vehicles$count <- rpois(n = nrow(vehicles), lambda = exp(as.numeric(vehicles$condition)))
    expect_is(Regression(count~.-region_weight,
                         weight = region_weight,
                         data = vehicles, type = "Poisson"),
              "PoissonRegression")
    expect_is(Regression(count~.-region_weight,
                         weight = region_weight,
                         data = vehicles, type = "Quasi-Poisson"),
              "QuasiPoissonRegression")
    vehicles$count <- MASS::rnegbin(n = nrow(vehicles),
                                    mu = exp(as.numeric(vehicles$condition)),
                                    theta = 2)
    expect_is(Regression(count~.-region_weight,
                         data = vehicles, type = "NBD"),
              "NBDRegression")  ## unweighted
    expect_is(Regression(count~.-region_weight,
                         weight = region_weight,
                         data = vehicles, type = "NBD"),
              "NBDRegression")

    vehicles$count <- NULL
})

test_that("Multinomial response",
{
    n <- nrow(vehicles)
    nlev <- nlevels(vehicles[["year"]])
    expect_is(Regression(year~.-region_weight,
                         data = vehicles, type = "Multinomial Logit"),
              "MultinomialLogitRegression")
    fit <- Regression(year~.-region_weight, weights = region_weight,
                         data = vehicles, type = "Multinomial Logit")
    expect_is(fit, "MultinomialLogitRegression")
    expect_equal(dim(flipData::Probabilities(fit)), c(n, nlev))
    expect_equal(nlevels(predict(fit)), nlev)

    fit <- Regression(year~.-region_weight,  ## unweighted
                         data = vehicles, type = "Ordered Logit")
    expect_is(fit, "OrderedLogitRegression")
    expect_equal(dim(flipData::Probabilities(fit)),
                 c(n, nlev))
    expect_equal(nlevels(predict(fit)), nlev)
    expect_is(Regression(year~.-region_weight, weights = region_weight,
                         data = vehicles, type = "Ordered Logit"),
              "OrderedLogitRegression")

})

test_that("Relative Importance",
{
    ## Relative Importance Analysis code does not like "." in formulas
    rformula <- paste(setdiff(names(vehicles), c("log.price", "region_weight")),
                      collapse = "+")
    rformula <- formula(paste0("log.price~", rformula))
    warnings <- capture_warnings(Regression(rformula, data = vehicles,
                         weights = region_weight,
                         output = "Relative Importance Analysis"))
    expect_match(warnings[1], "Negative signs in Relative Importance scores")
    expect_match(warnings[2], "treated as categorical")

    expect_equal(suppressWarnings(
        Regression(log.price ~ drive+condition+log.odometer+region+year,
                   data = vehicles,
                   weights = region_weight,
                   output = "Shapley Regression"))$importance.type,
        "Shapley Regression")

    expect_warning(print(Regression(rformula,
                   data = vehicles,
                   weights = region_weight,
                   output = "Correlation")), "automatically converted to numeric")
})

test_that("Diagnostics",
{
    vehicles$great.condition <- factor(as.integer(vehicles$condition == "excellent" |
                                           vehicles$condition == "like new"))
    fit <- suppressWarnings(Regression(great.condition~.-region_weight-condition,
                         weight = region_weight,
                         data = vehicles, type = "Binary Logit"))

    ## Regression - Save Variable(s) - Probabilities of Each Response
    expect_equal(dim(flipData::Probabilities(fit)), c(nrow(vehicles), 2))
    ## Regression - Save Variable(s) - Predicted Values
    expect_is(predict(fit), "factor")
    ## Regression - Save Variable(s) - Fitted Values
    expect_is(fitted(fit), "numeric")
    ## Regression - Save Variable(s) - Residuals
    expect_is(resid(fit), "numeric")
    ## Regression - Diagnostic - Multicollinearity Table (VIF)
    expect_is(car::vif(fit), "matrix")
    ## Regression - Diagnostic - Plot - Cook's Distance
    expect_error(plot(fit, which = 4), NA)
    ## Regression - Diagnostic - Plot - Cook's Distance vs Leverage
    expect_error(plot(fit, which = 6), NA)
    ## Regression - Diagnostic - Plot - Goodness of Fit
    expect_error(suppressWarnings(GoodnessOfFitPlot(fit, max.points = 1000)), NA)
    ## Regression - Diagnostic - Plot - Influence Index
    if (is.null(names(fit$original$residuals)))
        names(fit$original$residuals) <- seq_along(fit$original$residuals)
    if (is.null(names(fit$original$y)))
        names(fit$original$y) <- seq_along(fit$original$residuals)
    expect_error(car::influenceIndexPlot(fit,
                                         id = list(method = 'y', n = 5, cex = 1,
                                                   location = 'lr'),
                                         vars = c('Studentized', 'hat', 'Cook')),
                 NA)
    ## Regression - Diagnostic - Plot - Normal Q-Q
    expect_error(plot(fit, which = 2), NA)
    ## Regression - Diagnostic - Plot - Residuals vs Fitted
    expect_error(plot(fit, which = 1), NA)
    ## Regression - Diagnostic - Plot - Residuals vs Leverage
    expect_error(plot(fit, which = 5), NA)
    ## Regression - Diagnostic - Plot - Scale-Location
    expect_error(plot(fit, which = 3), NA)
    ## Regression - Diagnostic - Prediction-Accuracy Table
    expect_error(print(ConfusionMatrix(fit)), NA)
    ## Regression - Diagnostic - Test Residual Normality (Shapiro-Wilk)
    ##   stats::shapiro.test errors if there are more than 5,000
    ##    this is because the approximate p-values are not known to be accurate
    ##    beyond this size (and also b/c the test almost always rejects the null
    ##   hypothesis with so many samples

    ## Regression - Diagnostic - Test Residual Serial Correlation (Durbin-Watson)
    expect_equal(DurbinWatson(fit)$p.value, .574, tolerance = 1e-3)

    vehicles$great.condition <- NULL

    ## Regression - Diagnostic - Test Residual Heteroscedasticity
        ## only for unweighted linear regression, tested above
    fit <- suppressWarnings(Regression(log.price~.-region_weight,
                   data = vehicles))
    expect_equal(car::ncvTest(fit)$p, 0)
})

test_that("Crosstab interaction",
{
    rformula <- paste(setdiff(names(vehicles),
                              c("log.price", "region_weight", "year")),
                      collapse = "+")
    rformula <- formula(paste0("log.price~", rformula))
    fit <- Regression(rformula, data = vehicles, interaction = year)
    expect_true(fit$test.interaction)
})
