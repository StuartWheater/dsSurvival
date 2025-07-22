#-------------------------------------------------------------------------------
# Copyright (c) 2025 ProPASS Consortium. All rights reserved.
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

context("coxphSLMADS::arg::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("coxphSLMADS::arg::no parameters")
test_that("no parameters coxphSLMADS", {
    expect_error(coxphSLMADS(), "", fixed = TRUE)
})

context("coxphSLMADS::arg::null parameters")
test_that("null parameters coxphSLMADS", {
    formula_arg  <- NULL
    dataName_arg <- NULL
    weights_arg  <- NULL

    expect_error(coxphSLMADS(formula = formula_arg, dataName = dataName_arg, weights = weights_arg), "The formula must be set for use in survival::coxph()", fixed = TRUE)
})

context("coxphSLMADS::arg::absent dataName parameters")
test_that("null parameters coxphSLMADS", {
    formula_arg <- NULL
    weights_arg <- NULL

    expect_error(coxphSLMADS(formula = formula_arg, weights = weights_arg), "The formula must be set for use in survival::coxph()", fixed = TRUE)
})

context("coxphSLMADS::arg::absent weights parameters")
test_that("null parameters coxphSLMADS", {
    formula_arg  <- NULL
    dataName_arg <- NULL

    expect_error(coxphSLMADS(formula = formula_arg, dataName = dataName_arg), "The formula must be set for use in survival::coxph()", fixed = TRUE)
})

context("coxphSLMADS::arg::absent dataName parameters with value")
test_that("null parameters coxphSLMADS", {
    formula_arg <- NULL
    weights_arg <- "weights"
    weights_arg <- c() 

    expect_error(coxphSLMADS(formula = formula_arg, weights = weights_arg), "The formula must be set for use in survival::coxph()", fixed = TRUE)
})

context("coxphSLMADS::arg::absent weights parameters with value")
test_that("null parameters coxphSLMADS", {
    formula_arg  <- NULL
    dataName_arg <- "dataName"
    dataName     <- c()

    expect_error(coxphSLMADS(formula = formula_arg, dataName = dataName_arg), "The formula must be set for use in survival::coxph()", fixed = TRUE)
})

context("coxphSLMADS::arg::absent dataName parameters without value")
test_that("null parameters coxphSLMADS", {
    formula_arg <- NULL
    weights_arg <- "weights_obj"

    expect_error(coxphSLMADS(formula = formula_arg, weights = weights_arg), "The formula must be set for use in survival::coxph()", fixed = TRUE)
})

context("coxphSLMADS::arg::absent weights parameters without value")
test_that("null parameters coxphSLMADS", {
    formula_arg  <- NULL
    dataName_arg <- "dataName"

    expect_error(coxphSLMADS(formula = formula_arg, dataName = dataName_arg), "object 'dataName' not found", fixed = TRUE)
})

#
# Done
#

context("coxphSLMADS::arg::shutdown")

context("coxphSLMADS::arg::done")
