context("Regression Diagnostics")
suppressWarnings(RNGversion("3.5.3"))
zformula <- formula("Overall ~ Fees + Interest + Phone + Branch + Online + ATM")
data(bank, package = "flipExampleData")
# random sample of 200 rows to improve perfomance
# Hard code relevant cases
relevant.rows <- c("756", "383", "863", "56", "539", "377", "54", "206", "811",
                   "871", "603", "176", "293", "425", "285", "408", "599", "617",
                   "372", "433", "210", "219", "64", "338", "883", "413", "265",
                   "192", "658", "229", "767", "240", "67", "541", "796", "424",
                   "480", "458", "820", "25", "15", "359", "858", "227", "526",
                   "370", "17", "638", "287", "800", "436", "823", "214", "101",
                   "254", "690", "732", "200", "656", "244", "182", "253", "832",
                   "521", "587", "828", "446", "309", "702", "423", "467", "55",
                   "721", "70", "657", "585", "529", "365", "519", "513", "733",
                   "385", "87", "552", "542", "768", "840", "506", "294", "698",
                   "885", "757", "40", "93", "522", "805", "679", "877", "879",
                   "749", "731", "36", "806", "630", "258", "559", "614", "447",
                   "577", "683", "339", "346", "561", "89", "250", "720", "825",
                   "327", "625", "79", "129", "710", "164", "575", "49", "643",
                   "822", "428", "550", "787", "644", "329", "642", "95", "295",
                   "248", "146", "483", "517", "322", "463", "788", "361", "841",
                   "641", "417", "218", "314", "672", "619", "707", "580", "719",
                   "771", "812", "489", "452", "440", "251", "86", "718", "207",
                   "786", "426", "875", "379", "366", "414", "407", "664", "896",
                   "109", "624", "135", "35", "443", "734", "160", "9", "627", "449",
                   "442", "177", "114", "445", "53", "296", "315", "535", "337",
                   "534", "509", "399", "553", "261", "347", "847", "505", "833",
                   "637")
bank <- bank[row.names(bank) %in% relevant.rows, ]

sb <- bank$ID > 100
attr(sb, "label") <- "ID greater than 100"
wgt <- bank$ID
wgt[is.na(wgt)] = 0
attr(wgt, "label") <- "ID"
attr(bank$Overall, "label") <- "Overall satisfaction"
attr(bank$Fees, "label") <- "Fees paid"
attr(bank$Online, "label") <- "Online banking"


test_that(paste("Grand mean"),
{
    type  = "Linear"
    z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank[, c("Overall", "Fees", "Interest","Phone", "Branch", "Online", "ATM")], type = type, subset = sb, weights = wgt))
    subset <- sb & wgt > 0 & !is.na(sb) & !is.na(wgt)
    subset <- subset & complete.cases(cbind(sb, wgt, bank[, c("Overall", "Fees", "Interest","Phone", "Branch", "Online", "ATM")]))
    y <- bank$Overall[subset]
    w <- wgt[subset]
    mn <- sum(y * w) / sum(w)
    expect_equal(mn, GrandMean(z))
})

for (type in c("Linear", "Poisson", "Quasi-Poisson", "Binary Logit", "NBD", "Multinomial Logit", "Ordered Logit"))
    test_that(paste("extractAIC :", type),
    {
        z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, type = type, subset = sb, weights = wgt))
        expect_error(extractAIC(z), NA)
})


for (type in c("Linear", "Poisson", "Quasi-Poisson","Binary Logit",  "NBD"))#, "Multinomial Logit")) #"Ordered Logit",
    test_that(paste("Testing outliers:", type),
    {
        z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, type = type, subset = sb, weights = wgt))
        expect_error(outlierTest(z), NA)
        expect_error(capture.output(HatValues(z)), NA)
        expect_error(capture.output(CooksDistance(z)), NA)

})

#
# oldcon <- options(contrasts = c("contr.treatment", "contr.poly"))
# ## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
# ## Page 9: Plant Weight Data.
# ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
# trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
# group <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
# weight <- c(ctl, trt)
# wgt = runif(length(group))
# lm.D9 <- Regression(weight ~ group, weight = wgt)
# lm.D9
# summary(lm.D90 <- update(lm.D9, . ~ . - 1))
# update(lm.D9)
# getCall(lm.D90)  # "through the origin"
#
#
# update(lm.D9, NULL, method = "model.frame")


for (type in c("Linear", "Linear","Poisson", "Quasi-Poisson", "Binary Logit", "NBD"))
    test_that(paste("Cooks distance works:",type),
              {
                  missing = "Exclude cases with missing data"
                  z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, detail = FALSE, data = bank, weights = wgt, type = type))
                  expect_error((cooks.distance(z)), NA)
              })

for (type in c("Poisson", "Quasi-Poisson","Binary Logit", "Ordered Logit", "NBD", "Multinomial Logit"))
    test_that(paste("Goodness of fit statistics are well behaved:", type),
    {
        z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, type = type))
        expect_true(McFaddensRhoSquared(z) > 0.2)
        expect_true(GoodnessOfFit(z)$value > 0.2)
        expect_error(AIC(z), NA)
        z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, type = type, weights = wgt))
        expect_true(McFaddensRhoSquared(z) > 0.2)
        expect_true(GoodnessOfFit(z)$value > 0.2)
        expect_error(AIC(z), NA)
})


test_that("Durbin Watson",
{
    # Negative auto correlation
    set.seed(1)
    x = runif(100) * 2
    y = x + rep(1:2,50)
    dat = data.frame(x, y)
    reg = suppressWarnings(Regression(y ~ x, data = dat, type = "Binary Logit"))
    z = suppressWarnings(DurbinWatson(reg))
    expect_equal(z$statistic, c(d = 2.62), tolerance = 1.0e-3)
    expect_equal(z$p, 0, tolerance = 1.0e-5)
    reg = suppressWarnings(Regression(y ~ x, data = dat))
    z = DurbinWatson(reg)
    expect_equal(z$statistic, c(d = 3.94), tolerance = 1.0e-3)
    expect_equal(z$p, 0, tolerance = 1.0e-3)
    # Positive autocorrelation
    y = x + c(rep(0,50),rep(1,50))
    dat = data.frame(x, y)
    reg = suppressWarnings(Regression(y ~ x, data = dat, type = "Binary Logit"))
    z = DurbinWatson(suppressWarnings(reg))
    expect_equal(z$statistic, c(d = 1.49), tolerance = 1.0e-2)
    expect_equal(z$p, 0.004, tolerance = 1.0e-2)
    reg = Regression(y ~ x, data = dat)
    z = DurbinWatson(reg)
    expect_equal(z$statistic, c(d = 0.0444), tolerance = 1.0e-3)
    expect_equal(z$p, 0, tolerance = 1.0e-3)
    # Random (no autocorrelation)
    y = x + rnorm(100)
    dat = data.frame(x, y)
    reg = suppressWarnings(Regression(y ~ x, data = dat, type = "Binary Logit"))
    z = DurbinWatson(reg)
    expect_equal(z$statistic, c(d = 2.06), tolerance = 1.0e-2)
    expect_equal(z$p, 0.712, tolerance = 1.0e-3)
    reg = (Regression(y ~ x, data = dat))
    z = DurbinWatson(reg)
    expect_equal(z$statistic, c(d = 2.08), tolerance = 1.0e-3)
    expect_equal(z$p, 0.646, tolerance = 1.0e-3)
    # Comparing with car package
    z = car::durbinWatsonTest(lm(y ~ x, data = dat), simulate = TRUE, reps = 100000, alternative = "two.sided")
    reg = suppressWarnings(Regression(y ~ x, data = dat))
    z1 <- DurbinWatson(reg, n.permutations = 100000)
    expect_equal(as.numeric(z$dw), as.numeric(z1$statistic), tolerance = 1.0e-3)
    expect_equal(z$p, z1$p, tolerance = 1.0e-2)
    missing <- missing <- "Exclude cases with missing data"
    for (type in c( "Linear","Poisson", "Quasi-Poisson","Binary Logit", "Ordered Logit", "NBD"))
    {
        z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, detail = FALSE, data = bank, type = type))
        expect_error(DurbinWatson(z), NA)
    }
})

for (type in c("Linear", "Linear","Poisson", "Quasi-Poisson", "Binary Logit", "NBD"))
    test_that(paste("Cooks distance works:",type),
    {
        missing = "Exclude cases with missing data"
        z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, detail = FALSE, data = bank, type = type))
        expect_error((cooks.distance(z)), NA)
    })


for (type in c("Ordered Logit",  "Multinomial Logit"))
    test_that(paste("Cooks distance does not works:",type),
    {
        missing = "Exclude cases with missing data"
        z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, detail = FALSE, data = bank, type = type))
        expect_that(cooks.distance(z), throws_error())
    })

missing = "Multiple imputation"
type = "Ordered Logit"
for(missing in c("Multiple imputation", "Imputation (replace missing values with estimates)", "Exclude cases with missing data"))
    for (type in c( "Linear","Poisson", "Quasi-Poisson","Binary Logit", "Ordered Logit", "NBD", "Multinomial Logit"))
        test_that(paste("Accuracy and R-square:",missing, type),
        {
            z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, type = type))
            expect_true(Accuracy(z) > 0.2)
            expect_true(GoodnessOfFit(z)$value > 0.17)
            z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = sb,  type = type))
            expect_true(Accuracy(z) > 0.2)
            expect_true(GoodnessOfFit(z)$value > 0.17)
            z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, weights = wgt, missing = missing, data = bank, type = type))
            expect_true(Accuracy(z) > 0.2)
            expect_true(GoodnessOfFit(z)$value > 0.17)
            z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, weights = wgt, missing = missing, data = bank, subset = sb,  type = type))
            expect_true(Accuracy(z) > 0.2)
            expect_true(GoodnessOfFit(z)$value > 0.17)
        })


test_that("Tests of non-constant variance (Breush-Pagen test)",
{
    # Unfilitered
    z = ncvTest(suppressWarnings(Regression(zformula, data = bank)))
    z1 = car::ncvTest(lm(zformula, data = bank))
    expect_equal(z$p, z1$p, tolerance = 1.0e-8)

    # Correct error for ncvTest
    expect_error(ncvTest(suppressWarnings(Regression(zformula, data = bank, type = "Ordered Logit"))))


    # Filitered
    z = ncvTest(suppressWarnings(Regression(zformula, data = bank, subset = sb)))
    z1 = car::ncvTest(lm(zformula, data = bank, subset = sb))
    expect_equal(z$p, z1$p, tolerance = 1.0e-8)

    # Weighted.
    expect_that(ncvTest(suppressWarnings(Regression(zformula, data = bank,  weights = wgt))), throws_error())

    # Weighted and filtered
    expect_that(ncvTest(suppressWarnings(Regression(zformula, data = bank, subset = sb,  weights = wgt))), throws_error())

    # Unweighted
    zRegression <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank))
    zWLS <- lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank)
    expect_equal(ncvTest(zRegression)$p, ncvTest(zWLS)$p)
    # Filtered
    zRegression <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100))
    zWLS <- lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100)
    expect_equal(ncvTest(zRegression)$p, ncvTest(zWLS)$p)
    # Weighted.
    expect_that(ncvTest(suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, weights = bank$ID))), throws_error())

})

test_that("VIF", {
    library(car)
    # Unweighted - linear
    new.vif <- function(x) {
        out <- car::vif(x)
        out <- as.matrix(out)
        class(out) <- c(class(out), "visualization-selector")
        out
    }
    c.bank <- bank[complete.cases(bank), ]
    reg.form <- Overall ~ Fees + Interest + Phone + Branch + Online + ATM
    zRegression <- vif(Regression(reg.form, data = c.bank))
    zR <- new.vif(lm(reg.form, data = c.bank))
    expect_equal(zRegression, zR)
    # Filtered - linear
    subs <- c.bank[["ID"]] > 100
    zRegression <- vif(Regression(reg.form, data = c.bank, subset = subs))
    zR <- new.vif(lm(reg.form, data = c.bank, subset = subs))
    expect_equal(zRegression, zR)
    # Weighted.
    w <- runif(nrow(c.bank))
    expect_error(vif(Regression(reg.form, data = c.bank, subset = subs, weights = w)), NA)
    # Logit (used as a proxy for all the glms)
    type <- "Binary Logit"
    c.bank[["overall.gt.4"]] <- c.bank[["Overall"]] >= 4
    dichot.form <- overall.gt.4 ~ Fees + Interest + Phone + Branch + Online + ATM
    zRegression <- Regression(dichot.form, data = c.bank, type = type)
    zR <- glm(dichot.form, data = c.bank,  family = binomial(link = "logit"))
    expect_equal(vif(zRegression), new.vif(zR))
    # Logit - filtered
    zRegression <- Regression(dichot.form, data = c.bank, type = type, subset = subs)
    zR <- glm(dichot.form, data = c.bank,  family = binomial(link = "logit"), subset = subs)
    expect_equal(vif(zRegression), new.vif(zR))
    # Logit - filtered and weighted
    z <- wgt > 0 & complete.cases(bank[, c("Overall", "Fees", "Interest", "Phone", "Branch", "Online", "ATM")])
    zBank <- bank[z, ]
    zwgt <- wgt[z]
    zBank[["overall.gt.4"]] <- zBank[["Overall"]] >= 4
    zRegression <- Regression(dichot.form, data = zBank, weights = zwgt, type = type)
    zDesign <- flipData::WeightedSurveyDesign(zBank, zwgt)
    zR <- survey::svyglm(dichot.form, data = zBank, design = zDesign, family = quasibinomial())
    expect_equal(vif(zRegression), new.vif(zR))
    # Checking for no errors
    weights.list <- list(NULL, runif(nrow(c.bank)))
    for (type in c("Poisson", "Quasi-Poisson", "Ordered Logit"))
        for (w in weights.list)
           expect_error(vif(Regression(reg.form, data = c.bank, type = type, subset = subs, weights = w)), NA)
    for (w in weights.list)
        expect_warning(vif(Regression(reg.form, data = c.bank, type = "NBD", subset = subs, weights = w)),
                       "Model may not have converged")
    # Checking for errors in other types of models
   for (w in weights.list)
        expect_error(vif(Regression(reg.form, data = c.bank, type = "Multinomial Logit", subset = subs, weights = w)))
})
