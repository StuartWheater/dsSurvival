#-------------------------------------------------------------------------------
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

context("SurvDS::arg::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("SurvDS::arg::no parameters")
test_that("no parameters SurvDS", {
    expect_error(SurvDS(), "Start time parameter or follow-up time parameter (time) must be numeric or integer.", fixed = TRUE)
})

context("SurvDS::arg::null parameters")
test_that("null parameters SurvDS", {
    time   <- NULL
    time2  <- NULL
    event  <- NULL
    type   <- NULL
    origin <- NULL

    expect_error(SurvDS(time, time2, event, type, origin), "Start time parameter or follow-up time parameter (time) must be numeric or integer.", fixed = TRUE)
})

context("SurvDS::arg::valid time parameter")
test_that("valid time parameter SurvDS", {
    time   <- 1234
    time2  <- NULL
    event  <- NULL
    type   <- NULL
    origin <- NULL

    expect_error(SurvDS(time, time2, event, type, origin), "Event parameter (event) must be numeric or integer or factor.", fixed = TRUE)
})

context("SurvDS::arg::valid times parameters")
test_that("valid times parameters SurvDS", {
    time   <- 1234
    time2  <- 2345
    event  <- NULL
    type   <- NULL
    origin <- NULL

    expect_error(SurvDS(time, time2, event, type, origin), "Event parameter (event) must be numeric or integer or factor.", fixed = TRUE)
})

context("SurvDS::arg::valid times and event parameters")
test_that("valid times and event parameters SurvDS", {
    time   <- 1234
    time2  <- 2345
    event  <- 1567
    type   <- NULL
    origin <- NULL

    expect_warning(SurvDS(time, time2, event, type, origin), "Invalid status value, converted to NA", fixed = TRUE)
})

context("SurvDS::arg::valid time and event parameters")
test_that("valid time and event parameters SurvDS", {
    time   <- 1234
    time2  <- NULL
    event  <- 1567
    type   <- NULL
    origin <- NULL

    expect_warning(SurvDS(time, time2, event, type, origin), "Invalid status value, converted to NA", fixed = TRUE)
})

#
# Done
#

context("SurvDS::arg::shutdown")

context("SurvDS::arg::done")
