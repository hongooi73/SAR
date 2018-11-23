context("Azure recommender service backend")


# backend test ---

tenant <- Sys.getenv("AZ_TENANT_ID")
app <- Sys.getenv("AZ_APP_ID")
password <- Sys.getenv("AZ_PASSWORD")
subscription <- Sys.getenv("AZ_SUBSCRIPTION")

if(tenant == "" || app == "" || password == "" || subscription == "")
    skip("Resource Manager credentials must be set prior to test")

az <- AzureRMR::az_rm$new(tenant=tenant, app=app, password=password)
sub1 <- az$get_subscription(subscription)

test_that("Azure recommender service backend works",
{
    # generate random resource group name
    randgrp <- paste(sample(letters, 6, replace=TRUE), collapse="")

    expect_is(sub1$create_rec_service(randgrp, hosting_plan="S2", location="australiasoutheast"), "az_rec_service")

    rec_svc2 <- sub1$get_rec_service(randgrp)
    expect_is(rec_svc2, "az_rec_service")

    expect_true(!is_empty(rec_svc2$url) && !is_empty(rec_svc2$admin_key) && !is_empty(rec_svc2$rec_key))
    expect_true(rec_svc2$url != "" && rec_svc2$admin_key != "" && rec_svc2$rec_key != "")

    Sys.setenv(AZ_REC_RESGRP=randgrp,
               AZ_REC_SERVICE=rec_svc2$url,
               AZ_REC_ADMIN_KEY=rec_svc2$admin_key,
               AZ_REC_REC_KEY=rec_svc2$rec_key,
               AZ_REC_STORAGE_KEY=rec_svc2$storage_key)
})



# client test ---

# check that credentials are available
svcname <- Sys.getenv("AZ_REC_SERVICE")
admin_key <- Sys.getenv("AZ_REC_ADMIN_KEY")
rec_key <- Sys.getenv("AZ_REC_REC_KEY")
storage_key <- Sys.getenv("AZ_REC_STORAGE_KEY")

expect_true(svcname != "" && admin_key != "" && rec_key != "")

datapath <- "../resources"
ms_usage <- read.csv(file.path(datapath, "demoUsage.csv"), stringsAsFactors=FALSE)
names(ms_usage) <- c("user", "item", "time")
ms_usage$time <- as.POSIXct(ms_usage$time, tz="UTC", format="%Y/%m/%dT%H:%M:%S")

i <- readLines(file.path(datapath, "items.txt"))
u <- readLines(file.path(datapath, "user.txt"))
u2 <- readLines(file.path(datapath, "user2.txt"))
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


test_that("Azure recommender client works",
{
    endp <- rec_endpoint$new(svcname, admin_key, rec_key, storage_key=storage_key)
    expect_is(endp, "rec_endpoint")

    mods <- endp$models
    expect_s3_class(mods, "data.frame")
    if("test_count" %in% mods$description)
        endp$delete_model("test_count", confirm=FALSE)
    expect_false("test_count" %in% endp$models$description)

    endp$upload_csv(file.path(datapath, "demoUsage.csv"))

    test_count <- endp$train_model("test_count", usage="demoUsage.csv", support_threshold=3,
                            similarity="Cooccurrence", user_affinity=TRUE, user_to_items=TRUE, backfill=TRUE,
                            include_seed_items=FALSE)
    expect_is(test_count, "rec_model")

    expect_identical(test_count$status, "Completed")

    ipred0 <- read.csv(file.path(datapath, "itempred_count3.csv"), stringsAsFactors=FALSE,
                       colClasses=c(rep("character", 11), rep("numeric", 10)))
    ipred <- test_count$item_predict(item=i)
    expect_s3_class(ipred, "data.frame")
    check_preds(ipred0, ipred, 0.05) # item prediction scores depend on time of prediction as well as time of fit (!)

    upred0 <- read.csv(file.path(datapath, "userpred_count3_userid_only.csv"), stringsAsFactors=FALSE,
                       colClasses=c(rep("character", 11), rep("numeric", 10)))
    upred <- test_count$user_predict(userdata=u)
    expect_s3_class(upred, "data.frame")
    check_preds(upred0, upred, 0.01)

    upred0 <- read.csv(file.path(datapath, "userpred_count3_userid_plus_events.csv"), stringsAsFactors=FALSE,
                       colClasses=c(rep("character", 11), rep("numeric", 10)))
    upred <- test_count$user_predict(userdata=dfu)
    expect_s3_class(upred, "data.frame")
    check_preds(upred0, upred, 0.01)
})


test_that("Azure recommender client works with multiple user IDs",
{
    endp <- rec_endpoint$new(svcname, admin_key, rec_key, storage_key=storage_key)
    expect_is(endp, "rec_endpoint")

    test_count <- endp$get_model("test_count")
    expect_is(test_count, "rec_model")

    uu2 <- c(u, u2)

    expected <- read.csv(file.path(datapath, "userpred_2users_count3_userid_only.csv"),
                         stringsAsFactors=FALSE,
                         colClasses=c(rep("character", 11), rep("numeric", 10)))
    pred <- test_count$user_predict(uu2)
    check_preds(expected, pred, 0.01)

    pred2 <- test_count$user_predict(uu2[2:1])
    expected2 <- expected[2:1,]
    row.names(expected2) <- NULL
    check_preds(expected2, pred2, 0.01)

    pred3 <- test_count$user_predict(uu2[c(1, 2, 1, 2, 1, 2)])
    check_preds(expected, pred3, 0.01)

    expected <- read.csv(file.path(datapath, "userpred_2users_count3_userid_plus_events.csv"),
                         stringsAsFactors=FALSE,
                         colClasses=c(rep("character", 11), rep("numeric", 10)))
    dfuu2 <- rbind(subset(ms_usage, user == u), subset(ms_usage, user == u2))
    pred <- test_count$user_predict(dfuu2)
    check_preds(expected, pred, 0.01)

    dfu2u <- rbind(subset(ms_usage, user == u2), subset(ms_usage, user == u))
    pred2 <- test_count$user_predict(dfu2u)
    expected2 <- expected[2:1,]
    row.names(expected2) <- NULL
    check_preds(expected2, pred2, 0.01)

    # delete test model once we're done
    endp$delete_model("test_count", confirm=FALSE)
    expect_false("test_count" %in% endp$models$description)
})


test_that("Azure recommmender backfill works",
{
    endp <- rec_endpoint$new(svcname, admin_key, rec_key, storage_key=storage_key)
    expect_is(endp, "rec_endpoint")

    mods <- endp$models
    expect_s3_class(mods, "data.frame")

    if("test_nobf" %in% mods$description)
        endp$delete_model("test_nobf", confirm=FALSE)
    expect_false("test_nobf" %in% endp$models$description)

    if("test_bf" %in% mods$description)
        endp$delete_model("test_bf", confirm=FALSE)
    expect_false("test_bf" %in% endp$models$description)

    test_bf <- endp$train_model("test_bf", usage="demoUsage.csv", support_threshold=3,
                            similarity="Jaccard", user_affinity=TRUE, user_to_items=TRUE, backfill=TRUE,
                            include_seed_items=FALSE)
    expect_identical(test_bf$status, "Completed")

    test_nobf <- endp$train_model("test_nobf", usage="demoUsage.csv", support_threshold=3,
                            similarity="Jaccard", user_affinity=TRUE, user_to_items=TRUE, backfill=FALSE,
                            include_seed_items=FALSE)
    expect_identical(test_nobf$status, "Completed")

    u_bf <- readLines(file.path(datapath, "user_backfill.txt"))

    upred0_bf <- read.csv(file.path(datapath, "userpred_jac3_backfill.csv"), stringsAsFactors=FALSE,
                          colClasses=c(rep("character", 11), rep("numeric", 10)))

    upred_bf <- test_bf$user_predict(userdata=u_bf)
    expect_s3_class(upred_bf, "data.frame")
    check_preds(upred0_bf, upred_bf, 0.01)

    upred0_nbf <- read.csv(file.path(datapath, "userpred_jac3_nobackfill.csv"), stringsAsFactors=FALSE,
                           colClasses=c(rep("character", 11), rep("numeric", 10)))

    upred_nbf <- test_nobf$user_predict(userdata=u_bf)
    upred_nbf <- test_nobf$user_predict(userdata=u_bf)
    expect_s3_class(upred_nbf, "data.frame")
    check_preds(upred0_nbf, upred_nbf, 0.01)

    # delete test models once we're done
    endp$delete_model("test_bf", confirm=FALSE)
    expect_false("test_bf" %in% endp$models$description)

    endp$delete_model("test_nobf", confirm=FALSE)
    expect_false("test_nbf" %in% endp$models$description)
})



# backend and credential delete ---

sub1$delete_resource_group(Sys.getenv("AZ_REC_RESGRP"), confirm=FALSE)

Sys.unsetenv("AZ_REC_RESGRP")
Sys.unsetenv("AZ_REC_SERVICE")
Sys.unsetenv("AZ_REC_ADMIN_KEY")
Sys.unsetenv("AZ_REC_REC_KEY")
Sys.unsetenv("AZ_REC_STORAGE_KEY")

