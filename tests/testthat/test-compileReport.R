# library(testthat); library(augere.core); source("test-compileReport.R")

test_that("compileReport works correctly", {
    original.wd <- getwd()
    tmp <- tempfile()
    dir.create(tmp)

    rmd.path <- file.path(tmp, "test.Rmd")
    write("```{r}
res <- data.frame(foo=1:5, bar=LETTERS[1:5])
write.csv(res, file='blah.csv', row.names=FALSE)
```", file=rmd.path)

    env <- new.env()
    compileReport(rmd.path, env=env)
    expect_true(is.data.frame(env$res))
    expect_true(file.exists(file.path(tmp, "blah.csv")))

    roundtrip <- read.csv(file.path(tmp, "blah.csv"))
    expect_identical(env$res, roundtrip)

    # Original working directory is restored.
    expect_identical(getwd(), original.wd)
})

test_that("compileReport works the same when the contents are supplied without the report", {
    original.wd <- getwd()
    tmp <- tempfile()
    dir.create(tmp)
    rmd.path <- file.path(tmp, "test.Rmd") # this doesn't exist.

    contents <- list(
        "```{r}",
        list(
            "res <- data.frame(foo=1:5, bar=LETTERS[1:5])",
            "write.csv(res, file='blah.csv', row.names=FALSE)"
        ),
        "```"
    )

    env <- new.env()
    compileReport(rmd.path, env=env, contents=contents)
    expect_true(is.data.frame(env$res))
    expect_true(file.exists(file.path(tmp, "blah.csv")))

    # Original working directory is restored.
    expect_identical(getwd(), original.wd)
})

test_that("compileReport skips chunks", {
    tmp <- tempfile()
    dir.create(tmp)

    rmd.path <- file.path(tmp, "stuff.Rmd")
    write("```{r}
res <- 1L
```

```{r skipme}
res <- 2L
```", file=rmd.path)

    # Without skipping.
    env <- new.env()
    compileReport(rmd.path, env)
    expect_identical(env$res, 2L)

    # Skip by name.
    env <- new.env()
    compileReport(rmd.path, env, skip.chunks="skipme")
    expect_identical(env$res, 1L)
})

test_that("compileReport uses cached inputs correctly", {
    tmp <- tempfile()
    dir.create(tmp)

    fun <- resetInputCache()
    on.exit(fun(), add=TRUE)

    rmd.path <- file.path(tmp, "whee.Rmd")
    write("```{r}", file=rmd.path)

    df <- data.frame(X=runif(26), Y=LETTERS)
    write(processInputCommands(df, "alpha"), file=rmd.path, append=TRUE)

    df2 <- data.frame(X=runif(10), Y=LETTERS[10:19])
    write(processInputCommands(wrapInput(df2, NULL), "bravo"), file=rmd.path, append=TRUE)

    df3 <- LETTERS
    write(processInputCommands(wrapInput(df3, "letters"), "charlie"), file=rmd.path, append=TRUE)

    write(processInputCommands(wrapInput(NULL, commands="data.frame(X=rnorm(5), Y=LETTERS[5:1])"), "delta"), file=rmd.path, append=TRUE)
    write("```", file=rmd.path, append=TRUE)

    env <- new.env()
    compileReport(rmd.path, env=env)
    expect_identical(env$alpha, df) # skips the error
    expect_identical(env$bravo, df2) # skips the error
    expect_identical(env$charlie, df3) # skips the incorrect code 
    expect_identical(nrow(env$delta), 5L) # runs the code to generate the object
})
