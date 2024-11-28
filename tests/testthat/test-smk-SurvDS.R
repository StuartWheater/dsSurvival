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

context("SurvDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("SurvDS::smk::no parameters")
test_that("no parameters SurvDS", {
    expect_error(SurvDS(), "Start time parameter or follow-up time parameter (time) must be numeric or integer.", fixed = TRUE)
})

context("SurvDS::smk::null parameters")
test_that("null parameters SurvDS", {
    time   <- NULL
    time2  <- NULL
    event  <- NULL
    type   <- NULL
    origin <- NULL

    expect_error(SurvDS(), "Start time parameter or follow-up time parameter (time) must be numeric or integer.", fixed = TRUE)
})

#
# Done
#

context("SurvDS::smk::shutdown")

context("SurvDS::smk::done")
