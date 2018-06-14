context("SAR basic")

datapath <- "../resources"
ms_usage <- read.csv(file.path(datapath, "demoUsage.csv"))
names(ms_usage) <- c("user", "item", "time")
ms_usage$time <- as.POSIXct(ms_usage$time, tz="UTC", format="%Y/%m/%dT%H:%M:%S")

sim_count1 <- as.matrix(read.csv(file.path(datapath, "sim_count1.csv"), row.names=1, check.names=FALSE))
sim_count3 <- as.matrix(read.csv(file.path(datapath, "sim_count3.csv"), row.names=1, check.names=FALSE))

sim_jac1 <- as.matrix(read.csv(file.path(datapath, "sim_jac1.csv"), row.names=1, check.names=FALSE))
sim_jac3 <- as.matrix(read.csv(file.path(datapath, "sim_jac3.csv"), row.names=1, check.names=FALSE))

sim_lift1 <- as.matrix(read.csv(file.path(datapath, "sim_lift1.csv"), row.names=1, check.names=FALSE))
sim_lift3 <- as.matrix(read.csv(file.path(datapath, "sim_lift3.csv"), row.names=1, check.names=FALSE))


test_that("SAR model fit works",
{
    count1 <- sar(ms_usage, support_threshold=1, similarity="count")
    expect_s3_class(count1, "sar")
    expect_equal(as.matrix(count1$sim_mat), sim_count1)

    count3 <- sar(ms_usage, support_threshold=3, similarity="count")
    expect_s3_class(count3, "sar")
    expect_equal(as.matrix(count3$sim_mat), sim_count3)

    jac1 <- sar(ms_usage, support_threshold=1, similarity="jaccard")
    expect_s3_class(jac1, "sar")
    expect_equal(as.matrix(jac1$sim_mat), sim_jac1)

    jac3 <- sar(ms_usage, support_threshold=3, similarity="jaccard")
    expect_s3_class(jac3, "sar")
    expect_equal(as.matrix(jac3$sim_mat), sim_jac3)

    lift1 <- sar(ms_usage, support_threshold=1, similarity="lift")
    expect_s3_class(lift1, "sar")
    expect_equal(as.matrix(lift1$sim_mat), sim_lift1)

    lift3 <- sar(ms_usage, support_threshold=3, similarity="lift")
    expect_s3_class(lift3, "sar")
    expect_equal(as.matrix(lift3$sim_mat), sim_lift3)
})


