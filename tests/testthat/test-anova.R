context("Anova")
require(flipRegression)

data(bank, package = "flipExampleData")
zformula <- formula("Overall ~ Fees + Interest + Phone + Branch + Online + ATM")
sb <- bank$ID > 100
attr(sb, "label") <- "ID greater than 100"
wgt <- bank$ID
attr(wgt, "label") <- "ID"
bank$dep <- (unclass(bank$Overall) - 1) / 6
z <- bank$Phone
z[z == 1] <- 2
z[z == 7] <- 6
bank$Phone <- factor(z)
z <- bank$Online
z[z == 1] <- 2
z[z == 7] <- 6
bank$Online <- factor(z)
library(car)

test_that(paste("Robust se does something"),
          {
              # Treatment within linear regression
              type = "Linear"
              zreg <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, robust.se = FALSE, subset = TRUE,  weights = NULL, type = type))
              zregRobust <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, robust.se = TRUE, subset = TRUE,  weights = NULL, type = type))
              expect_false(zreg$summary$coefficients[2,4] == zregRobust$summary$coefficients[2,4])
              
              # Treatment within Anova - no effect regardless of regression specification
              expect_equal(Anova(zreg)[[3]][4], Anova(zregRobust)[[3]][4])
              # Robust works
              expect_equal(zregRobust$summary$coefficients[3,4], Anova(zregRobust, white.adjust = "hc3")[[3]][2])
              zregRobust <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, robust.se = "hc1", subset = TRUE,  weights = NULL, type = type))
              expect_equal(zregRobust$summary$coefficients[3,4], Anova(zregRobust, white.adjust = "hc1")[[3]][2])
              
          })

for(missing in c("Multiple imputation", "Imputation (replace missing values with estimates)", "Exclude cases with missing data"))
    for (type in c("Binary Logit", "Ordered Logit"))
        test_that(paste("Type by residual", missing, type),
      {
          # no weight, no filter
          z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = TRUE,  weights = NULL, type = type))
          expect_error(print(Anova(z)), NA)
          # Filter
          z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = sb,  weights = NULL, type = type))
          expect_error(print(Anova(z)), NA)
          # Weighted Ordered Logit error check
          anova.error <- if (type != "Ordered Logit") NA else "^'Anova' can not be computed for a 'Ordered Logit' model with weights"
          # weight,
          z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = TRUE,  weights = wgt, type = type))
          expect_error(print(suppressWarnings(Anova(z))), anova.error)
          # weight, filter
          z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = sb,  weights = wgt, type = type))
          expect_error(print(suppressWarnings(Anova(z))), anova.error)
      })

#### REDUCE DATA SIZE FOR TESTS WITHOUT NUMERICAL EQUALITY ###

data(bank, package = "flipExampleData")
bank <- bank[sample(nrow(bank), 300), ] # random sample of 300 rows to improve perfomance
zformula <- formula("Overall ~ Fees + Interest + Phone + Branch + Online + ATM")
sb <- bank$ID > 100
attr(sb, "label") <- "ID greater than 100"
wgt <- bank$ID
attr(wgt, "label") <- "ID"
bank$dep <- (unclass(bank$Overall) - 1) / 6
z <- bank$Phone
z[z == 1] <- 2
z[z == 7] <- 6
bank$Phone <- factor(z)
z <- bank$Online
z[z == 1] <- 2
z[z == 7] <- 6
bank$Online <- factor(z)


test_that(paste("Alternative ways of passing data in"),
{
      type = "Linear"
      # no weight, no filter
      z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = NULL, type = type))
      # Checking ANOVA works
      expect_error(suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM,output = "ANOVA", data = bank, weights = NULL, type = type)), NA)
      # filter and weight a part of the data frame.
      zbank <- cbind(bank, w = wgt, ff = sb)
      z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = zbank, subset = ff, weights = w, type = type))
      attach(zbank)
      z1 = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, weights = w, subset = ff, type = type))
      expect_error(print(Anova(z)), NA)
      detach(zbank)
      # filter and weight a part of the data frame and are formulas.
      zbank <- cbind(bank, w = wgt, ff = sb)
      z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = zbank, subset = ff == TRUE, weights = w, type = type))
      attach(zbank)
      z1 = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, weights = w, subset = ff == TRUE, type = type))
      expect_error(print(Anova(z)), NA)
      detach(zbank)
      expect_true(all.equal(z$coefficients, z1$coefficients))
      # filter and weight are not part of the data frame.
      z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = sb, weights = wgt, type = type))
      expect_error(print(Anova(z)), NA)
      attach(zbank)
      z1 = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, subset = sb, weights = wgt, type = type))
      expect_error(print(Anova(z)), NA)
      detach(zbank)
      # data frame referenced in formula.
      z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = sb, weights = wgt, type = type))
      z1 = suppressWarnings(Regression(bank$Overall ~ bank$Fees + bank$Interest + bank$Phone + bank$Branch + bank$Online + bank$ATM, subset = sb, weights = wgt, type = type))
      expect_error(print(Anova(z)), NA)
})

## Multiple imputation test moved to test-imputation.R for CircleCI test splitting
for(missing in c("Imputation (replace missing values with estimates)", "Exclude cases with missing data"))
    for (type in c("Multinomial Logit", "Linear","Poisson", "Quasi-Poisson", "NBD"))
        test_that(paste("Type by residual", missing, type),
      {
          # no weight, no filter
          z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = TRUE,  weights = NULL, type = type))
          expect_error(print(Anova(z)), NA)
          # Filter
          z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = sb,  weights = NULL, type = type))
          expect_error(print(Anova(z)), NA)
          # weight,
          z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = TRUE,  weights = wgt, type = type))
          expect_error(print(suppressWarnings(Anova(z))), NA)
          # weight, filter
          z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = sb,  weights = wgt, type = type))
          expect_error(print(suppressWarnings(Anova(z))), NA)
      })

