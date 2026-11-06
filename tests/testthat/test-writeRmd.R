# library(testthat); library(augere.core); source("test-writeRmd.R")

contents <- list(
    c("# TITLE", ""),
    list(
        c("```{r}", "a <- 1", "```"),
        c("", "", "super foo bar")
    ),
    c("", "## section")
)

test_that("writeRmd basically works", {
    tmp <- tempfile(fileext=".Rmd")
    writeRmd(contents, tmp, clean.empty=FALSE)
    expect_identical(readLines(tmp), unlist(contents))
})

test_that("writeRmd cleans empty lines", {
    tmp <- tempfile(fileext=".Rmd")
    writeRmd(contents, tmp, clean.empty=TRUE)
    expected <- unlist(contents)
    expected <- expected[-6]
    expect_identical(readLines(tmp), expected)
})

test_that("writeRmd appends content", {
    tmp <- tempfile(fileext=".Rmd")
    writeRmd("FOO", tmp)
    writeRmd("BAR", tmp, append=TRUE)
    expect_identical(readLines(tmp), c("FOO", "BAR"))
})

test_that("writeRmd adds an empty newline", {
    expected <- unlist(contents)
    tmp <- tempfile(fileext=".Rmd")

    writeRmd(contents, tmp, clean.empty=FALSE, trailing.empty=TRUE)
    expect_identical(readLines(tmp), c(expected, ""))

    # Empty line is already present in one form or another.
    writeRmd(c(contents, ""), tmp, clean.empty=FALSE, trailing.empty=TRUE)
    expect_identical(readLines(tmp), c(expected, ""))

    contents2 <- contents
    contents2[[length(contents2)]][2] <- paste0(contents2[[length(contents2)]][2], "\n")
    writeRmd(contents2, tmp, clean.empty=FALSE, trailing.empty=TRUE)
    expect_identical(readLines(tmp), c(expected, ""))

    # Avoids empty input.
    writeRmd(character(0), tmp, trailing.empty=TRUE)
    expect_identical(readLines(tmp), "")
})
