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

context("rcsDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("rcsDS::smk::valid x and numeric knots")
test_that("valid x and numeric knots", {
    x     <- "D"
    knots <- 6

    D <- c(1, 2, 3, 4, 5, 6, 7, 8)

    res <- expect_warning(rcsDS(x = x, knots = knots), "6 knots requested with 8 unique values of x.  knots set to 6 interior values.", fixed = TRUE)
    expect_equal(class(res), "rms")
})

context("rcsDS::smk::valid x and ref to variable knots")
test_that("valid x and ref to variable knots", {
    x     <- "D"
    knots <- "knots_val"

    knots_val <- 6

    D <- c(1, 2, 3, 4, 5, 6, 7, 8)

    res <- expect_warning(rcsDS(x = x, knots = knots), "6 knots requested with 8 unique values of x.  knots set to 6 interior values.", fixed = TRUE)
    expect_equal(class(res), "rms")
})

#
# Done
#

context("rcsDS::smk::shutdown")

context("rcsDS::smk::done")
