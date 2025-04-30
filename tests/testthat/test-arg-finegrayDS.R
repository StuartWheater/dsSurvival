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
test_that("no parameters finegrayDS", {
    expect_error(finegrayDS(), "Both 'formula' and 'data' parameters must be provided", fixed = TRUE)
})

context("finegrayDS::arg::null parameters")
test_that("null parameters finegrayDS", {
    expect_error(finegrayDS(NULL), "Both 'formula' and 'data' parameters must be provided", fixed = TRUE)
})

context("finegrayDS::arg formula is null, data no value")
test_that("formula is null, data no value", {
    expect_error(finegrayDS(formula = "a ~ b"), "Both 'formula' and 'data' parameters must be provided", fixed = TRUE)
})

context("finegrayDS::arg formula no value, data is null")
test_that("formula no value, data is null", {
    expect_error(finegrayDS(data = NULL), "Both 'formula' and 'data' parameters must be provided", fixed = TRUE)
})

#
# Done
#

context("finegrayDS::arg::shutdown")

context("finegrayDS::arg::done")
