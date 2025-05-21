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

context("survfitDS::arg::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("survfitDS::arg::formula is NULL")
test_that("formula is NULL", {
    expect_error(survfitDS(formula = NULL), "The input must have a non-empty formula to be used in survival::survfit()", fixed = TRUE)
})

#
# Done
#

context("survfitDS::arg::shutdown")

context("survfitDS::arg::done")
