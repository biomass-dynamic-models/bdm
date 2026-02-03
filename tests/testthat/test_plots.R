
test_that("diagnostic plots", {
    
    # load some data
    data(albio)
    dat <- bdmData(harvest = albio$catch, index = albio$cpue, time = albio$year)
    
    # initialise, compile and run
    mdl <- bdm()
    capture.output(mdl <- compiler(mdl))
    capture.output(mdl <- sampler(mdl, dat, chains = 1, iter = 100))
    
    # plots
    gg <- traceplot(mdl)
    expect_s3_class(gg, "ggplot")
    gg <- histplot(mdl)
    expect_s3_class(gg, "ggplot")
    gg <- cumsumplot(mdl)
    expect_s3_class(gg, "ggplot")
    gg <- dynplot(mdl)
    expect_s3_class(gg, "ggplot")
    gg <- idxplot(mdl)
    expect_s3_class(gg, "ggplot")
})


