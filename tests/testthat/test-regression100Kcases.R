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
    expect_warning(print(fit), "Unusual observations detected")
})

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
    expect_is(Regression(year~.-region_weight,
                         data = vehicles, type = "Multinomial Logit"),
              "MultinomialLogitRegression")
    expect_is(Regression(year~.-region_weight, weights = region_weight,
                         data = vehicles, type = "Multinomial Logit"),
              "MultinomialLogitRegression")

    expect_is(Regression(year~.-region_weight,  ## unweighted
                         data = vehicles, type = "Ordered Logit"),
              "OrderedLogitRegression")
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

test_that("Stepwise",
{
    rformula <- paste(setdiff(names(vehicles), c("log.price", "region_weight")),
                      collapse = "+")
    rformula <- formula(paste0("log.price~", rformula))
    fit <- Regression(rformula, data = vehicles)
    expect_is(Stepwise(fit, direction = "Backward"), "Stepwise")
    expect_is(Stepwise(fit, direction = "Forward"), "Stepwise")

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
    skip_on_travis()  # Tested by long-running blog test
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
