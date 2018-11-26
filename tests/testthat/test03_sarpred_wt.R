context("SAR weighted prediction")

datapath <- "../resources"
data(ms_usage, package="SAR", envir=environment())

i <- readLines(file.path(datapath, "items.txt"))
u <- readLines(file.path(datapath, "user.txt"))
newevents <- read.csv(file.path(datapath, "newevents.csv"), colClasses="character")
newevents_wt <- read.csv(file.path(datapath, "newevents_wt.csv"), colClasses="character")

newevents$time <- as.POSIXct(newevents$time, tz="UTC", format="%Y-%m-%d %H:%M:%S")
newevents_wt$time <- as.POSIXct(newevents_wt$time, tz="UTC", format="%Y-%m-%d %H:%M:%S")
newevents_wt$weight <- as.numeric(newevents_wt$weight)


test_that("Weighted prediction works",
{
    count_wt <- sar(ms_usage, support_threshold=3, similarity="count")

    pred1 <- read.csv(file.path(datapath, "wt_count3_userid_only.csv"), stringsAsFactors=FALSE,
                       colClasses=c(rep("character", 11), rep("numeric", 10)))
    expect_equal(user_predict(count_wt, u, k=10), pred1)
    expect_equal(user_predict(count_wt, u), pred1)

    pred2 <- read.csv(file.path(datapath, "wt_count3_userid_plus_unweighted_events.csv"), stringsAsFactors=FALSE,
                       colClasses=c(rep("character", 11), rep("numeric", 10)))
    expect_equal(user_predict(count_wt, newevents, k=10), pred2)
    expect_equal(user_predict(count_wt, newevents), pred2)

    pred3 <- read.csv(file.path(datapath, "wt_count3_unweighted_events.csv"), stringsAsFactors=FALSE,
                       colClasses=c(rep("character", 10), rep("numeric", 10)))
    expect_equal(user_predict(count_wt, newevents[-1], k=10), pred3)
    expect_equal(user_predict(count_wt, newevents[-1]), pred3)

    pred4 <- read.csv(file.path(datapath, "wt_count3_userid_plus_weighted_events.csv"), stringsAsFactors=FALSE,
                       colClasses=c(rep("character", 11), rep("numeric", 10)))
    expect_equal(user_predict(count_wt, newevents_wt, k=10), pred4)
    expect_equal(user_predict(count_wt, newevents_wt), pred4)

    pred5 <- read.csv(file.path(datapath, "wt_count3_weighted_events.csv"), stringsAsFactors=FALSE,
                       colClasses=c(rep("character", 10), rep("numeric", 10)))
    expect_equal(user_predict(count_wt, newevents_wt[-1], k=10), pred5)
    expect_equal(user_predict(count_wt, newevents_wt[-1]), pred5)
})

