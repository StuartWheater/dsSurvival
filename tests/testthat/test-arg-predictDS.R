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

context("predictDS::arg::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("predictDS::arg::no parameters")
test_that("no parameters predictDS", {
    expect_error(predictDS(), "invalid first argument", fixed = TRUE)
})

context("predictDS::arg::null parameters")
test_that("null parameters predictDS", {
    expect_error(predictDS(NULL), "invalid first argument", fixed = TRUE)
})

context("predictDS::arg::null parameter value")
test_that("null parameter value predictDS", {

    expect_error(predictDS("obj"), "The specified model object does not exist", fixed = TRUE)
})

context("predictDS::arg::null parameter value")
test_that("null parameter value predictDS", {
    object <- NULL

    expect_error(predictDS("object"), "The specified model is not an rms model", fixed = TRUE)
})

context("predictDS::arg::number parameter value")
test_that("number parameter value predictDS", {
    object <- 1.0

    expect_error(predictDS("object"), "The specified model is not an rms model", fixed = TRUE)
})

context("predictDS::arg::number parameter value")
test_that("number parameter value predictDS", {
    fit         <- "fit_obj"
    fit_obj_var <- 1.0

    expect_error(predictDS(fit = fit), "The specified model object does not exist", fixed = TRUE)
})

context("predictDS::arg::fun check")
test_that("fun check", {
    dataObj <- data.frame(y = c(1, 2, 3, 4, 5, 6), age = c(1, 2, 3, 4, 5, 6))
    dd      <- datadist(dataObj)
    options(datadist = dd)
    fit_var <- "fit_obj"
    fit_obj <- ols(y ~ age, data = dataObj)

    expect_error(predictDS(fit = fit_var, fun = "test"), "Disclosure risk: only standard transformations are allowed", fixed = TRUE)
})

#
# Done
#

context("predictDS::arg::shutdown")

context("predictDS::arg::done")
