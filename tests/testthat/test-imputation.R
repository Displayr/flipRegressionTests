context("Anova Imputation")
require(flipRegression)

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

missing <- "Multiple imputation"
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
