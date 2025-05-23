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

context("plotsurvfitDS::arg::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("plotsurvfitDS::arg::dataName valid")
test_that("dataName valid", {
    dataName = "data_obj"
    data_obj = NULL

    expect_error(plotsurvfitDS(formula = NULL, dataName = dataName), "The formula must be set for use in survival::coxph()", fixed = TRUE)
})

context("plotsurvfitDS::arg::formula is NULL")
test_that("formula is NULL", {
    expect_error(plotsurvfitDS(formula = NULL), "The formula must be set for use in survival::coxph()", fixed = TRUE)
})

#
# Done
#

context("plotsurvfitDS::arg::shutdown")

context("plotsurvfitDS::arg::done")
