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

context("finegrayDS::arg::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("finegrayDS::arg::no parameters")
test_that("no parameters", {
    expect_error(finegrayDS(), "Both 'formula' and 'data' parameters must be provided", fixed = TRUE)
})

context("finegrayDS::arg::null 'formula' and 'data' parameters")
test_that("null 'formula' and 'data' parameters", {
    formula <- NULL
    data    <- NULL

    expect_error(finegrayDS(formula = formula, data = data), "Both 'formula' and 'data' parameters must be provided", fixed = TRUE)
})

context("finegrayDS::arg::null 'formula' parameter")
test_that("null 'formula' parameter", {
    formula     <- NULL
    data        <- "data_val"
    data_val    <- "value"

    expect_error(finegrayDS(formula = formula, data = data), "Both 'formula' and 'data' parameters must be provided", fixed = TRUE)
})

context("finegrayDS::arg::null 'data' parameters")
test_that("null 'data' parameters", {
    formula     <- "formula_val"
    formula_val <- "value"
    data        <- NULL

    expect_error(finegrayDS(formula = formula, data = data), "Both 'formula' and 'data' parameters must be provided", fixed = TRUE)
})

context("finegrayDS::arg::valid times parameters")
test_that("valid times parameters finegrayDS", {
    formula     <- "formula_val"
    formula_val <- "x ~ a"
    data        <- "data_val"
    data_val    <- "value"

    expect_error(finegrayDS(formula = formula, data = data), "invalid formula \"\\\"formula_val\\\"\": not a call", fixed = TRUE)
})

#
# Done
#

context("finegrayDS::arg::shutdown")

context("finegrayDS::arg::done")
