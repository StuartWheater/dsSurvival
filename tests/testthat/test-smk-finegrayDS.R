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

context("finegrayDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("finegrayDS::smk::simple example")
test_that("simple example", {
#    formula_arg <- "formula_val"
#    formula_val <- parse(text = "Surv(time, event) ~ a")
    formula_arg <- parse(text = "Surv(time, event) ~ a")
    data_arg    <- "data_val"
    data_val    <- data.frame(time = c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), event = c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), y = c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), a = c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0))

    # ToDo: Fix error
    # expect_error(finegrayDS(formula = formula_arg, data = data_arg), "attempt to set an attribute on NULL")
    finegrayDS(formula = formula_arg, data = data_arg)
})

#
# Done
#

context("finegrayDS::smk::shutdown")

context("finegrayDS::smk::done")
