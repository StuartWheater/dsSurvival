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

context("coxphSLMADS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("coxphSLMADS::smk::simple example")
test_that("simple example", {
    formula_arg  <- stats::as.formula("Surv(dataObj$time, dataObj$event) ~ dataObj$age + dataObj$edu")
    dataName_arg <- "dataObj"
    weights_arg  <- "weights_obj"

    dataObj     <- data.frame(time = c(1.0, 0.0, 0.5, 1.0, 0.9, 0.7, 0.4, 0.3), event = c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE), age = c(22, 23, 24, 25, 26, 27, 26, 28), edu = c(100, 105, 110, 90, 95, 100, 100, 105))
    weights_obj <- c(1.0, 0.1, 0.5, 1.0, 0.9, 0.7)

    res <- coxphSLMADS(formula = formula_arg, dataName = dataName_arg, weights = weights_arg)

    expect_length(res, 14)
})

context("coxphSLMADS::smk::simple example, with control")
test_that("simple example, with control", {
    formula_arg  <- stats::as.formula("Surv(dataObj$time, dataObj$event) ~ dataObj$age + dataObj$edu")
    dataName_arg <- "dataObj"
    weights_arg  <- "weights_obj"
    control_arg  <- survival::coxph.control()

    dataObj     <- data.frame(time = c(1.0, 0.0, 0.5, 1.0, 0.9, 0.7, 0.4, 0.3), event = c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE), age = c(22, 23, 24, 25, 26, 27, 26, 28), edu = c(100, 105, 110, 90, 95, 100, 100, 105))
    weights_obj <- c(1.0, 0.1, 0.5, 1.0, 0.9, 0.7)

    res <- coxphSLMADS(formula = formula_arg, dataName = dataName_arg, weights = weights_arg, control = control_arg)

    expect_length(res, 14)
})

#
# Done
#

context("coxphSLMADS::smk::shutdown")

context("coxphSLMADS::smk::done")
