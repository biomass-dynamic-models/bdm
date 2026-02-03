
test_that('initialize bdmData', {
    dat <- bdmData(index = runif(10), harvest = 1:10)
    expect_s4_class(dat, "bdmData")
})

test_that('sigmao(dat) initialise', {
    
    # check dimensions
    dat <- bdmData(index = runif(10), harvest = 1:10)
    expect_equal(dim(sigmao(dat)), c(10, 1))
    dat <- bdmData(index = matrix(runif(20), 10, 2), harvest = 1:10)
    expect_equal(dim(sigmao(dat)), c(10, 2))
    
    # check values
    sigmao_in <- matrix(runif(20), 10, 2)
    dat <- bdmData(index = matrix(runif(20), 10, 2), harvest = 1:10, sigmao = sigmao_in)
    expect_equal(dim(sigmao(dat)), c(10, 2))
    expect_equal(sigmao(dat)[,1], sigmao_in[,1])
    expect_equal(sigmao(dat)[,2], sigmao_in[,2])
    sigmao_in <- runif(2)
    dat <- bdmData(index = matrix(runif(20), 10, 2), harvest = 1:10, sigmao = sigmao_in)
    expect_equal(dim(sigmao(dat)), c(10, 2))
    expect_equal(sigmao(dat)[,1], rep(sigmao_in[1],10))
    expect_equal(sigmao(dat)[,2], rep(sigmao_in[2],10))
    sigmao_in <- runif(1)
    dat <- bdmData(index = runif(10), harvest = 1:10, sigmao = sigmao_in)
    expect_equal(dim(sigmao(dat)), c(10, 1))
    expect_equal(sigmao(dat)[,1], rep(sigmao_in,10))
})

test_that('sigmao(dat) assignment', {
    
    # assign matrix for >1 index
    dat <- bdmData(index = matrix(runif(20), 10, 2), harvest = 1:10)
    sigmao_in <- matrix(runif(20), 10, 2)
    sigmao(dat) <- sigmao_in
    expect_equal(dim(sigmao(dat)), c(10, 2))
    expect_equal(sigmao(dat)[,1], sigmao_in[,1])
    expect_equal(sigmao(dat)[,2], sigmao_in[,2])
    
    # assign numeric for >1 index
    dat <- bdmData(index = matrix(runif(20), 10, 2), harvest = 1:10)
    sigmao_in <- runif(2)
    sigmao(dat) <- sigmao_in
    expect_equal(dim(sigmao(dat)), c(10, 2))
    expect_equal(sigmao(dat), matrix(sigmao_in, 10, 2, byrow = TRUE))
    sigmao_in <- runif(1)
    sigmao(dat) <- sigmao_in
    expect_equal(dim(sigmao(dat)), c(10, 2))
    expect_equal(sigmao(dat), matrix(sigmao_in, 10, 2))
    
    # assign numeric for 1 index
    dat <- bdmData(index = runif(10), harvest = 1:10)
    sigmao_in <- runif(1)
    sigmao(dat) <- sigmao_in
    expect_equal(dim(sigmao(dat)), c(10, 1))
    expect_equal(sigmao(dat), matrix(sigmao_in, 10, 1))
})

test_that('shape(dat) assignment', {
    
    dat <- bdmData(index = runif(10), harvest = 1:10)
    shape_in   <- runif(1, 0.1, 0.9)
    shape(dat) <- shape_in
    expect_lt(abs(shape(dat) - shape_in), .Machine$double.eps^0.25)
    n <- shape(dat, 'n')
    expect_lt(abs((1/n)^(1/(n-1)) - shape_in), .Machine$double.eps^0.25)
})

test_that('plot bdmData', {
    # load some data
    data(albio)
    dat <- bdmData(harvest = albio$catch, index = albio$cpue, time = albio$year)
    # plots
    gg <- plot(dat)
    expect_s3_class(gg, "ggplot")
})



