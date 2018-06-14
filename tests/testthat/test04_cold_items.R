context("Cold items modelling")

datapath <- "../resources"
ms_usage <- read.csv(file.path(datapath, "demoUsage.csv"), stringsAsFactors=FALSE)
names(ms_usage) <- c("user", "item", "time")
ms_usage$time <- as.POSIXct(ms_usage$time, tz="UTC", format="%Y/%m/%dT%H:%M:%S")

ms_cat <- read.csv(file.path(datapath, "demoCatalog.csv"), stringsAsFactors=FALSE, header=FALSE)
names(ms_cat) <- c("item", "name", "category")

# make some more variables
ms_cat$ms <- grepl("microsoft", ms_cat$name, ignore.case=TRUE)
ms_cat$surf <- grepl("surface", ms_cat$name, ignore.case=TRUE)

f <- reformulate(names(ms_cat)[-(1:2)])


test_that("Cold item modelling works",
{
    mod0 <- sar(ms_usage, support_threshold=25)
    mod1 <- sar(ms_usage, support_threshold=25, catalog_data=ms_cat, catalog_formula=f, cold_item_model=NULL)
    mod2 <- sar(ms_usage, support_threshold=25, catalog_data=ms_cat, catalog_formula=f, cold_item_model=NULL,
                cold_to_cold=TRUE)

    mod3 <- sar(ms_usage, support_threshold=25, catalog_data=ms_cat, catalog_formula=f, cold_item_model="lm")
    mod4 <- sar(ms_usage, support_threshold=25, catalog_data=ms_cat, catalog_formula=f, cold_item_model="lm",
                cold_to_cold=TRUE)

    sim0 <- as.matrix(mod0$sim_mat)
    sim1 <- as.matrix(mod1$sim_mat)
    sim2 <- as.matrix(mod2$sim_mat)
    sim3 <- as.matrix(mod3$sim_mat)
    sim4 <- as.matrix(mod4$sim_mat)

    # identify elements that will be modified by cold item models
    warm <- diag(sim0) > 0
    cold <- !warm
    indices <- matrix(seq_along(sim0), nrow(sim0), ncol(sim0))
    cold_cold <- indices[cold, cold]
    indices_13 <- c(indices[warm, cold], indices[cold, warm])
    indices_24 <- c(setdiff(cold_cold, diag(cold_cold)), indices_13)

    sim1diff <- sim1[indices_13]
    sim2diff <- sim2[indices_24]
    sim3diff <- sim3[indices_13]
    sim4diff <- sim4[indices_24]

    # check that new matrices are still symmetrical
    expect_true(isSymmetric(sim1))
    expect_true(isSymmetric(sim2))
    expect_true(isSymmetric(sim3))
    expect_true(isSymmetric(sim4))

    # check that only cold entries are modified
    expect_true(all(sim0[indices_13] == 0))
    expect_true(all(sim0[indices_24] == 0))

    expect_identical(sim0[-indices_13], sim1[-indices_13])
    expect_identical(sim0[-indices_24], sim2[-indices_24])
    expect_identical(sim0[-indices_13], sim3[-indices_13])
    expect_identical(sim0[-indices_24], sim4[-indices_24])

    expect_true(all(sim1diff >= 0))
    expect_true(all(sim2diff >= 0))
    expect_true(all(sim3diff >= 0))
    expect_true(all(sim4diff >= 0))

    # check that new cold entries < warm entries (with fuzz)
    min_warm <- min(sim0[sim0 > 0])
    expect_true(all(sim1diff - min_warm < 1e-15))
    expect_true(all(sim2diff - min_warm < 1e-15))
    expect_true(all(sim3diff - min_warm < 1e-15))
    expect_true(all(sim4diff - min_warm < 1e-15))
})
