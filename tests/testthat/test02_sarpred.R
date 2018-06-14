context("SAR prediction")

datapath <- "../resources"
ms_usage <- read.csv(file.path(datapath, "demoUsage.csv"), stringsAsFactors=FALSE)
names(ms_usage) <- c("user", "item", "time")
ms_usage$time <- as.POSIXct(ms_usage$time, tz="UTC", format="%Y/%m/%dT%H:%M:%S")

i <- readLines(file.path(datapath, "items.txt"))
u <- readLines(file.path(datapath, "user.txt"))
dfu <- subset(ms_usage, user == u)


test_that("Prediction for cooccurrence works",
{
    count3 <- sar(ms_usage, support_threshold=3, similarity="count")

    ipred3 <- read.csv(file.path(datapath, "itempred_count3.csv"), stringsAsFactors=FALSE,
                       colClasses=c(rep("character", 11), rep("numeric", 10)))
    expect_equal(item_predict(count3, i, k=10), ipred3)
    expect_equal(item_predict(count3, i), ipred3)

    upred3 <- read.csv(file.path(datapath, "userpred_count3_userid_only.csv"), stringsAsFactors=FALSE,
                       colClasses=c(rep("character", 11), rep("numeric", 10)))
    expect_equal(user_predict(count3, u, k=10), upred3)
    expect_equal(user_predict(count3, u), upred3)

    upred3 <- read.csv(file.path(datapath, "userpred_count3_userid_plus_events.csv"), stringsAsFactors=FALSE,
                       colClasses=c(rep("character", 11), rep("numeric", 10)))
    expect_equal(user_predict(count3, dfu, k=10), upred3)
    expect_equal(user_predict(count3, dfu), upred3)
})

test_that("Prediction for jaccard works",
{
    jac3 <- sar(ms_usage, support_threshold=3, similarity="jac")

    ipred3 <- read.csv(file.path(datapath, "itempred_jac3.csv"), stringsAsFactors=FALSE,
                       colClasses=c(rep("character", 11), rep("numeric", 10)))
    expect_equal(item_predict(jac3, i, k=10), ipred3)
    expect_equal(item_predict(jac3, i), ipred3)

    upred3 <- read.csv(file.path(datapath, "userpred_jac3_userid_only.csv"), stringsAsFactors=FALSE,
                       colClasses=c(rep("character", 11), rep("numeric", 10)))
    expect_equal(user_predict(jac3, u, k=10), upred3)
    expect_equal(user_predict(jac3, u), upred3)

    upred3 <- read.csv(file.path(datapath, "userpred_jac3_userid_plus_events.csv"), stringsAsFactors=FALSE,
                       colClasses=c(rep("character", 11), rep("numeric", 10)))
    expect_equal(user_predict(jac3, dfu, k=10), upred3)
    expect_equal(user_predict(jac3, dfu), upred3)
})

test_that("Prediction for lift works",
{
    lift3 <- sar(ms_usage, support_threshold=3, similarity="lift")

    ipred3 <- read.csv(file.path(datapath, "itempred_lift3.csv"), stringsAsFactors=FALSE,
                       colClasses=c(rep("character", 11), rep("numeric", 10)))
    expect_equal(item_predict(lift3, i, k=10), ipred3)
    expect_equal(item_predict(lift3, i), ipred3)

    upred3 <- read.csv(file.path(datapath, "userpred_lift3_userid_only.csv"), stringsAsFactors=FALSE,
                       colClasses=c(rep("character", 11), rep("numeric", 10)))
    expect_equal(user_predict(lift3, u, k=10), upred3)
    expect_equal(user_predict(lift3, u), upred3)

    upred3 <- read.csv(file.path(datapath, "userpred_lift3_userid_plus_events.csv"), stringsAsFactors=FALSE,
                       colClasses=c(rep("character", 11), rep("numeric", 10)))
    expect_equal(user_predict(lift3, dfu, k=10), upred3)
    expect_equal(user_predict(lift3, dfu), upred3)
})


test_that("Prediction with new/overlapping user IDs works",
{
    mod <- sar(ms_usage, support_threshold=3, similarity="count")

    ipred3 <- read.csv(file.path(datapath, "itempred_count3.csv"), stringsAsFactors=FALSE,
                       colClasses=c(rep("character", 11), rep("numeric", 10)))

    # new user ID: predicting with transactions should work, without transactions should fail
    dfu_new <- dfu
    dfu_new$user <- "xxxx"

    pred <- user_predict(mod, dfu_new)
    expect_s3_class(pred, "data.frame")
    expect_equal(nrow(pred), 1)

    expect_error(user_predict(mod, dfu_new[1]))

    expect_s3_class(user_predict(mod, dfu_new[-1]), "data.frame")

    # overlapping user IDs: predicting with transactions should work, without transactions should warn and drop
    dfu_ov <- rbind(dfu, dfu_new)

    pred <- user_predict(mod, dfu_ov)
    expect_s3_class(pred, "data.frame")
    expect_equal(nrow(pred), 2)

    expect_warning(pred <- user_predict(mod, dfu_ov[1]))
    expect_s3_class(pred, "data.frame")
    expect_equal(nrow(pred), 1)

    expect_s3_class(user_predict(mod, dfu_ov[-1]), "data.frame")
})


test_that("Prediction with new item IDs fails",
{
    mod <- sar(ms_usage)

    dfu_new <- dfu
    dfu_new$item[1] <- "xxxx"
    expect_error(user_predict(mod, dfu_new))
})


test_that("Backfilled and non-backfilled prediction work",
{
    u_bf <- readLines(file.path(datapath, "user_backfill.txt"))

    jac3 <- sar(ms_usage, support_threshold=3, similarity="jaccard")

    upred_nbf <- read.csv(file.path(datapath, "userpred_jac3_nobackfill.csv"), stringsAsFactors=FALSE,
                          colClasses=c(rep("character", 11), rep("numeric", 10)))
    expect_equal(user_predict(jac3, u_bf, backfill=FALSE), upred_nbf)

    upred_bf <- read.csv(file.path(datapath, "userpred_jac3_backfill.csv"), stringsAsFactors=FALSE,
                         colClasses=c(rep("character", 11), rep("numeric", 10)))
    expect_equal(user_predict(jac3, u_bf, backfill=TRUE), upred_bf)
})

