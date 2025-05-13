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

context("SurvDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("SurvDS::smk::valid times and event parameters")
test_that("valid times and event parameters SurvDS", {
    time   <- 1234
    time2  <- 2345
    event  <- 1567
    type   <- NULL
    origin <- NULL

    expect_warning(SurvDS(time, time2, event, type, origin), "Invalid status value, converted to NA", fixed = TRUE)
})

context("SurvDS::smk::valid time and event parameters")
test_that("valid time and event parameters SurvDS", {
    time   <- 1234
    time2  <- NULL
    event  <- 1567
    type   <- NULL
    origin <- NULL

    expect_warning(SurvDS(time, time2, event, type, origin), "Invalid status value, converted to NA", fixed = TRUE)
})

context("SurvDS::smk::valid times and event parameters, type 'right'")
test_that("valid times and event parameters, type 'right' SurvDS", {
    time   <- 1234
    time2  <- 2345
    event  <- 1567
    type   <- "mstate"
    origin <- 0

    res <- SurvDS(time, time2, event, type, origin)
    expect_equal(class(res), "Surv")
})

context("SurvDS::smk::valid time and event parameters, type 'right'")
test_that("valid time and event parameters, type 'right' SurvDS", {
    time   <- 1234
    time2  <- NULL
    event  <- 1567
    type   <- "mstate"
    origin <- 0

    res <- SurvDS(time, time2, event, type, origin)
    expect_equal(class(res), "Surv")
})

#
# Done
#

context("SurvDS::smk::shutdown")

context("SurvDS::smk::done")
