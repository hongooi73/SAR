context("Azure API")

# check that credentials are available
svcname <- Sys.getenv("AZ_PROD_REC_SVCNAME")
admin_pri_key <- Sys.getenv("AZ_PROD_REC_ADMIN_PRI_KEY")
rec_pri_key <- Sys.getenv("AZ_PROD_REC_REC_PRI_KEY")
instrument_key <- Sys.getenv("AZ_PROD_REC_INSTRUMENT_KEY")

if(svcname == "" || admin_pri_key == "" || rec_pri_key == "" || instrument_key == "")
    skip("Credentials must be set prior to test")


datapath <- "../../../../msft-recommendation/src/test/resources"
ms_usage <- read.csv(file.path(datapath, "demoUsage.csv"), stringsAsFactors=FALSE)
names(ms_usage) <- c("user", "item", "time")
ms_usage$time <- as.POSIXct(ms_usage$time, tz="UTC", format="%Y/%m/%dT%H:%M:%S")

i <- readLines(file.path(datapath, "items.txt"))
u <- readLines(file.path(datapath, "user.txt"))
dfu <- subset(ms_usage, user == u)

check_preds <- function(df1, df2, threshold_ratio)
{
    s1 <- grep("score", names(df1))
    m1 <- as.matrix(df1[s1])

    s2 <- grep("score", names(df2))
    m2 <- as.matrix(df2[s2])

    na1 <- which(is.na(m1))
    na2 <- which(is.na(m2))

    r <- na.omit(as.numeric(abs(m1 / m2 - 1)))

    # use tolerance to handle Azure fiddling with reference date
    expect_true(all(r < threshold_ratio) && diff(range(r)) < 1e-5 && identical(na1, na2))
}


test_that("Azure API works",
{
    az <- az_sar$new(svcname, admin_pri_key, rec_pri_key, instrument_key)
    expect_s3_class(az, "az_sar")

    mods <- az$list_models()
    expect_s3_class(mods, "data.frame")
    if("test_count" %in% mods$description)
        az$delete_model("test_count")
    Sys.sleep(2)
    expect_false("test_count" %in% az$list_models()$description)

    obj <- az$fit_model("test_count", container="inputdata", usage="demoUsage.csv", support_threshold=3,
                        similarity="Cooccurrence", user_affinity=TRUE, user_to_items=TRUE, backfill=TRUE,
                        include_seed_items=FALSE)
    expect_s3_class(obj, "az_sar_model")

    # loop until training is complete (max 300 seconds)
    count <- 0
    while(obj$modelStatus != "Completed" && count < 300)
    {
        Sys.sleep(1)
        count <- count + 1
        obj <- az$show_model("test_count")
    }
    expect_identical(obj$modelStatus, "Completed")

    ipred0 <- read.csv(file.path(datapath, "itempred_count3.csv"), stringsAsFactors=FALSE,
                       colClasses=c(rep("character", 11), rep("numeric", 10)))
    ipred <- az$item_predict("test_count", item=i)
    expect_s3_class(ipred, "data.frame")
    check_preds(ipred0, ipred, 0.05)  # item prediction scores depend on time of prediction as well as time of fit (!)

    upred0 <- read.csv(file.path(datapath, "userpred_count3_userid_only.csv"), stringsAsFactors=FALSE,
                       colClasses=c(rep("character", 11), rep("numeric", 10)))
    upred <- az$user_predict("test_count", userdata=u)
    expect_s3_class(upred, "data.frame")
    check_preds(upred0, upred, 0.01)

    upred0 <- read.csv(file.path(datapath, "userpred_count3_userid_plus_events.csv"), stringsAsFactors=FALSE,
                       colClasses=c(rep("character", 11), rep("numeric", 10)))
    upred <- az$user_predict("test_count", userdata=dfu)
    expect_s3_class(upred, "data.frame")
    check_preds(upred0, upred, 0.01)

    # delete test model once we're done
    az$delete_model("test_count")
    Sys.sleep(2)
    mods <- az$list_models()
    expect_false("test_count" %in% mods$description)
})


test_that("Azure API backfill works",
{
    az <- az_sar$new(svcname, admin_pri_key, rec_pri_key, instrument_key)

    mods <- az$list_models()
    expect_s3_class(mods, "data.frame")

    if("test_nobf" %in% mods$description)
        az$delete_model("test_nobf")
    Sys.sleep(2)
    expect_false("test_nobf" %in% az$list_models()$description)

    if("test_bf" %in% mods$description)
        az$delete_model("test_bf")
    Sys.sleep(2)
    expect_false("test_bf" %in% az$list_models()$description)

    obj <- az$fit_model("test_bf", container="inputdata", usage="demoUsage.csv", support_threshold=3,
                        similarity="Jaccard", user_affinity=TRUE, user_to_items=TRUE, backfill=TRUE,
                        include_seed_items=FALSE)

    # loop until training is complete (max 300 seconds)
    count <- 0
    while(obj$modelStatus != "Completed" && count < 300)
    {
        Sys.sleep(1)
        count <- count + 1
        obj <- az$show_model("test_bf")
    }
    expect_identical(obj$modelStatus, "Completed")

    obj <- az$fit_model("test_nobf", container="inputdata", usage="demoUsage.csv", support_threshold=3,
                        similarity="Jaccard", user_affinity=TRUE, user_to_items=TRUE, backfill=FALSE,
                        include_seed_items=FALSE)

    # loop until training is complete (max 300 seconds)
    count <- 0
    while(obj$modelStatus != "Completed" && count < 300)
    {
        Sys.sleep(1)
        count <- count + 1
        obj <- az$show_model("test_nobf")
    }
    expect_identical(obj$modelStatus, "Completed")

    u_bf <- readLines(file.path(datapath, "user_backfill.txt"))

    upred0_bf <- read.csv(file.path(datapath, "userpred_jac3_backfill.csv"), stringsAsFactors=FALSE,
                          colClasses=c(rep("character", 11), rep("numeric", 10)))

    upred_bf <- az$user_predict("test_bf", userdata=u_bf)
    expect_s3_class(upred_bf, "data.frame")
    check_preds(upred0_bf, upred_bf, 0.01)

    upred0_nbf <- read.csv(file.path(datapath, "userpred_jac3_nobackfill.csv"), stringsAsFactors=FALSE,
                           colClasses=c(rep("character", 11), rep("numeric", 10)))

    upred_nbf <- az$user_predict("test_nobf", userdata=u_bf)
    expect_s3_class(upred_nbf, "data.frame")
    check_preds(upred0_nbf, upred_nbf, 0.01)

    # delete test models once we're done
    az$delete_model("test_bf")
    Sys.sleep(2)
    mods <- az$list_models()
    expect_false("test_bf" %in% mods$description)

    az$delete_model("test_nobf")
    Sys.sleep(2)
    mods <- az$list_models()
    expect_false("test_nbf" %in% mods$description)
})
