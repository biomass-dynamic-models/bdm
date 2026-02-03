
test_that("project", {
    
    # get some data
    data(haknz)
    dat <- bdmData(harvest = haknz$landings, index = haknz$cpue, time = haknz$year)
     
    # initialize and fit default model
    mdl <- bdm()
    capture.output(mdl <- compiler(mdl))
    capture.output(mdl <- sampler(mdl, dat, chains = 1, run = 'example_run'))

    # constant harvest rate projection scenario
    mdl_project <- project(mdl, harvest = 0.10, time = 10, harvest_rate = TRUE)
    
    expect_type(mdl_project, "list")
    expect_equal(mdl@run, mdl_project$run)
    expect_equal(mdl_project$harvest_rate[1000, 48, 1], 0.1)
    
    # constant harvest projection scenario
    mdl_project <- project(mdl, harvest = 1000, time = 10, harvest_rate = FALSE)
    
    expect_type(mdl_project, "list")
    expect_equal(mdl@run, mdl_project$run)
    expect_equal(mdl_project$harvest[1000, 48, 1], 1000)
})


