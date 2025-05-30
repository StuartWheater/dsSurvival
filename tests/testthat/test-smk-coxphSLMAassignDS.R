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

context("coxphSLMAassignDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("coxphSLMAassignDS::smk::simple example")
test_that("simple example", {
    surv_obj <- Surv(time = c(1.0, 1.1, 1.2, 1.3, 1.4, 1.5), event = c(1, 1, 0, 1, 0, 0))

    formula_arg  <- "Surv(dataObj$time, dataObj$event) ~ dataObj$age + dataObj$edu"
    dataName_arg <- "dataObj"
    weights_arg  <- "weights_obj"

    dataObj     <- data.frame(time = c(1.0, 0.0, 0.5, 1.0, 0.9, 0.7), event = c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE), age = c(22, 23, 24, 25, 26, 27), edu = c(100, 105, 110, 90, 95, 100))
    weights_obj <- c(1.0, 0.0, 0.5, 1.0, 0.9, 0.7)

    # TODO: Fix check
    res <- expect_error(coxphSLMAassignDS(formula = formula_arg, dataName = dataName_arg, weights = weights_arg, ))
})

#
# Done
#

context("coxphSLMAassignDS::smk::shutdown")

context("coxphSLMAassignDS::smk::done")
