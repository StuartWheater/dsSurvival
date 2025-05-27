#-------------------------------------------------------------------------------
# Copyright (c) 2025 XXXX. All rights reserved.
# Copyright (c) 2024 Arjuna Technologies, Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#
# Set up
#

context("rmsTransDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("rmsTransDS::smk::transformation is rcs")
test_that("transformation is rcs", {
    x              <- "D"
    transformation <- "rcs"
    parms          <- NULL

    D <- c(1, 2, 3, 4, 5, 6, 7, 8)

    res <- rmsTransDS(x = x, transformation = transformation, parms = parms)
    expect_equal(class(res), "rms")
    expect_length(res, 32)
})


context("rmsTransDS::smk::transformation is asis")
test_that("transformation is asis", {
    x              <- "D"
    transformation <- "asis"
    parms          <- NULL

    D <- c(1, 2, 3, 4, 5, 6, 7, 8)

    res <- rmsTransDS(x = x, transformation = transformation, parms = parms)
    expect_equal(class(res), "rms")
    expect_length(res, 8)
})


context("rmsTransDS::smk::transformation is pol")
test_that("transformation is pol", {
    x              <- "D"
    transformation <- "pol"
    parms          <- NULL

    D <- c(1, 2, 3, 4, 5, 6, 7, 8)

    res <- rmsTransDS(x = x, transformation = transformation, parms = parms)
    expect_equal(class(res), "rms")
    expect_length(res, 16)
})


context("rmsTransDS::smk::transformation is lsp")
test_that("transformation is lsp", {
    x              <- "D"
    transformation <- "lsp"
    parms          <- 6      # Knots

    D <- c(1, 2, 3, 4, 5, 6, 7, 8)

    res <- rmsTransDS(x = x, transformation = transformation, parms = parms)
    expect_equal(class(res), "rms")
    expect_length(res, 16)
})


context("rmsTransDS::smk::transformation is catg")
test_that("transformation is catg", {
    x              <- "D"
    transformation <- "catg"
    parms          <- NULL

    D <- c(1, 2, 3, 4, 5, 6, 7, 8)

    res <- rmsTransDS(x = x, transformation = transformation, parms = parms)
    res_classes <- class(res)
    expect_length(res_classes, 2)
    expect_length(res, 8)
})


context("rmsTransDS::smk::transformation is scored")
test_that("transformation is scored", {
    x              <- "D"
    transformation <- "scored"
    parms          <- NULL

    D <- c(1, 2, 3, 4, 5, 6, 7, 8)

    res <- rmsTransDS(x = x, transformation = transformation, parms = parms)
    res_classes <- class(res)
    expect_length(res_classes, 3)
    expect_length(res, 8)
})


context("rmsTransDS::smk::transformation is strat")
test_that("transformation is strat", {
    x              <- "D"
    transformation <- "strat"
    parms          <- NULL

    D <- c(1, 2, 3, 4, 5, 6, 7, 8)

    res <- rmsTransDS(x = x, transformation = transformation, parms = parms)
    res_classes <- class(res)
    expect_length(res_classes, 2)
    expect_length(res, 8)
})


context("rmsTransDS::smk::transformation is gTrans")
test_that("transformation is gTrans", {
    x              <- "D"
    transformation <- "gTrans"
    parms          <- "exp"

    D <- c(1, 2, 3, 4, 5, 6, 7, 8)

    res <- rmsTransDS(x = x, transformation = transformation, parms = parms)
    expect_equal(class(res), "rms")
    expect_length(res, 8)
})

context("rmsTransDS::smk::transformation is lsp, knots parms")
test_that("transformation is lsp, knots parms", {
    x              <- "D"
    transformation <- "lsp"
    parms          <- "knots"
    knots          <- 6

    D <- c(1, 2, 3, 4, 5, 6, 7, 8)

    res <- rmsTransDS(x = x, transformation = transformation, parms = parms)
    expect_equal(class(res), "rms")
    expect_length(res, 16)
})

#
# Done
#

context("rmsTransDS::smk::shutdown")

context("rmsTransDS::smk::done")
