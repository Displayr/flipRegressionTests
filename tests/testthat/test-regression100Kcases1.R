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

test_that("Linear regression",
{
    ## Effects Plot doesn't like "." in formula
    expect_warning(print(Regression(log.price ~ drive+condition+log.odometer+region+year,
                   data = vehicles,
                   weights = region_weight,
                   output = "Effects Plot")),
                   "Unusual observations detected")

    ## Add 15 white noise predictors
    fit <- Regression(log.price~.-region_weight,
                   data = vehicles,
                   weights = region_weight,
                   output = "ANOVA")
    expect_is(fit, "LinearRegression")
    expect_is(predict(fit), "numeric")
    expect_warning(print(fit), "Unusual observations detected")

    ## Regression - Diagnostic - Test Residual Heteroscedasticity

})

test_that("Stepwise",
{
    rformula <- paste(setdiff(names(vehicles), c("log.price", "region_weight")),
                      collapse = "+")
    rformula <- formula(paste0("log.price~", rformula))
    fit <- Regression(rformula, data = vehicles)
    expect_is(Stepwise(fit, direction = "Backward"), "Stepwise")
    expect_is(Stepwise(fit, direction = "Forward"), "Stepwise")

})

test_that("Missing data: Dummy variable adjustment",
{
    rformula <- paste(setdiff(names(vehicles),
                              c("log.price", "region_weight", "year")),
                      collapse = "+")
    rformula <- formula(paste0("log.price~", rformula))
    pred.names <- c("drive", "condition", "log.odometer", "region", "year")
    n <- nrow(vehicles)
    prop.missing <- .1
    set.seed(999)
    for (i in pred.names)
        vehicles[[i]][sample(n, ceiling(prop.missing*n))] <- NA
    expect_is(Regression(rformula, data = vehicles, weight = region_weight,
                         missing = "Dummy variable adjustment"),
              "LinearRegression")
})

test_that("Missing data: Multiple imputation",
{
    skip_on_ci()  # Tested by long-running blog test
    rformula <- paste(setdiff(names(vehicles),
                              c("log.price", "region_weight", "year")),
                      collapse = "+")
    rformula <- formula(paste0("log.price~", rformula))
    pred.names <- c("drive", "condition", "log.odometer", "region", "year")
    n <- nrow(vehicles)
    prop.missing <- .1
    set.seed(9999)
    for (i in pred.names)
        vehicles[[i]][sample(n, ceiling(prop.missing*n))] <- NA
    expect_is(Regression(rformula, data = vehicles,
                         missing = "Multiple imputation"),
              "LinearRegression")

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
