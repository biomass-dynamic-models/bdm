
test_that("rstan sampler", {
    
    bdm_model <- "parameters {real y;} model {y ~ normal(0,1);}"
    mdl <- bdm(model_code = bdm_model)
    capture.output(mdl <- compiler(mdl))
    capture.output(mdl <- sampler(mdl, chains = 1, iter = 100))
    expect_true(length(mdl@trace) > 0)
    expect_equal(mdl@nsamples, 50)
})


