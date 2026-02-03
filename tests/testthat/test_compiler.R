
test_that("rstan compilation", {
    
    # initialize code directly
    expect_error(bdm(model_code = "parameters {real y;} model {y ~ normal(0,1);}"))
    
    # must provide a model name
    mdl <- bdm(model_code = "parameters {real y;} model {y ~ normal(0,1);}", model_name = "toy_model")
    
    capture.output(mdl <- compiler(mdl))
    expect_s4_class(mdl, "bdm")
    expect_equal(mdl@model_name, "toy_model")
    expect_s4_class(mdl@dso, "cxxdso")
    expect_true(mdl@dso@dso_saved)
    
    # initialize code indirectly
    # model name generated automatically
    toy_model <- "parameters {real y;} model {y ~ normal(0,1);}"
    mdl <- bdm(model_code = toy_model)
    
    capture.output(mdl <- compiler(mdl))
    expect_s4_class(mdl, "bdm")
    expect_equal(mdl@model_name, "toy_model")
    expect_s4_class(mdl@dso, "cxxdso")
    expect_true(mdl@dso@dso_saved)
    
    # default initialization
    mdl <- bdm()
    
    capture.output(mdl <- compiler(mdl))
    expect_s4_class(mdl, "bdm")
    expect_equal(mdl@model_name, "default")
    expect_s4_class(mdl@dso, "cxxdso")
    expect_true(mdl@dso@dso_saved)
})
