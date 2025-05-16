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

context("rmsTransDS::arg::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("rmsTransDS::arg::no parameters")
test_that("no parameters", {
    expect_error(rmsTransDS(), "Variable name (x) must be provided.", fixed = TRUE)
})

context("rmsTransDS::arg::null x parameter")
test_that("null x parameter", {
    x <- NULL

    expect_error(rmsTransDS(x = x), "Variable name (x) must be provided.", fixed = TRUE)
})

context("rmsTransDS::arg::valid x parameter")
test_that("valid x parameter", {
    x     <- "x_val"
    x_val <- "test"

#    expect_error(rmsTransDS(x = x), "Variable", fixed = TRUE)
})

#
# Done
#

context("rmsTransDS::arg::shutdown")

context("rmsTransDS::arg::done")
