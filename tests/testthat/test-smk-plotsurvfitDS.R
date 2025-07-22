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

context("plotsurvfitDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("plotsurvfitDS::smk::simple example")
test_that("simple example", {
    dataframe    <- data.frame(time = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8), event = c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE), age = c(22, 23, 24, 25, 26, 27, 26, 28), y = c(100, 105, 110, 90, 95, 100, 100, 105))

    formula_arg  <- "formula_obj"
    formula_obj  <- "Surv(time, age) ~ event"
    dataName_arg <- "data_obj"
    data_obj     <- data.frame(time = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8), event = c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE), age = c(22, 23, 24, 25, 26, 27, 26, 28), y = c(100, 105, 110, 90, 95, 100, 100, 105))

    # TODO: Fix Error: need $strata in survfir model
    # expect_error(plotsurvfitDS(formula = formula_arg, dataName = dataName_arg), "$ operator is invalid for atomic vectors", fixed = TRUE)
    plotsurvfitDS(formula = formula_arg, dataName = dataName_arg)
})

#
# Done
#

context("plotsurvfitDS::smk::shutdown")

context("plotsurvfitDS::smk::done")
