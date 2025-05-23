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

context("rmsTransDS::arg::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("rmsTransDS::arg::no parameters")
test_that("no parameters", {
    expect_error(rmsTransDS(), "Variable name (x) must be provided.", fixed = TRUE)
})

context("rmsTransDS::arg::null x parameter")
test_that("null x parameter", {
    x <- NULL

    expect_error(rmsTransDS(x = x), "Variable name (x) must be provided.", fixed = TRUE)
})

context("rmsTransDS::arg::valid x parameter")
test_that("valid x parameter", {
    x     <- "x_val"

    expect_error(rmsTransDS(x = x), "object 'x_val' not found", fixed = TRUE)
})

context("rmsTransDS::arg::valid x parameter class")
test_that("valid x parameter class", {
    x     <- "x_val"
    x_val <- "test"

    expect_error(rmsTransDS(x = x), "Variable to be transformed (x) must be numeric or integer.", fixed = TRUE)
})

context("rmsTransDS::smk::transformation is dummy")
test_that("transformation is dummy", {
    x              <- "D"
    transformation <- "dummy"
    parms          <- NULL

    D <- c(1, 2, 3, 4, 5, 6, 7, 8)

    expect_error(rmsTransDS(x = x, transformation = transformation, parms = parms), "Invalid transformation. Must be one of: rcs, asis, pol, lsp, catg, scored, strat, gTrans", fixed = TRUE)
})


#
# Done
#

context("rmsTransDS::arg::shutdown")

context("rmsTransDS::arg::done")
