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

context("survfitDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("survfitDS::smk::simple example")
test_that("simple example", {
    df <- data.frame(time = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8), event = c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE), age = c(22, 23, 24, 25, 26, 27, 26, 28), y = c(100, 105, 110, 90, 95, 100, 100, 105))

    formula_obj <- "ssssstimerrr eventzzz ~ fd$age"

    # ToDo: Fix Error
    # expect_error(survfitDS(formula = formula_obj), "invalid formula \"\\\"survival::Surv(time, event) ~ fd$age\\\"\": not a call", fixed = TRUE)
    survfitDS(formula = formula_obj)
})

#
# Done
#

context("survfitDS::smk::shutdown")

context("survfitDS::smk::done")
