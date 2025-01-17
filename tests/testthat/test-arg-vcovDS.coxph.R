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

context("vcovDS.coxph::arg::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("vcovDS.coxph::arg::no parameters")
test_that("no parameters vcovDS.coxph", {
    expect_error(vcovDS.coxph(), "Please provide the name of a survival::coxph object", fixed = TRUE)
})

context("vcovDS.coxph::arg::null parameters")
test_that("null parameters vcovDS.coxph", {
    expect_error(vcovDS.coxph(NULL), "Please provide the name of a survival::coxph object", fixed = TRUE)
})

context("vcovDS.coxph::arg::null parameter value")
test_that("null parameter value vcovDS.coxph", {
    object <- NULL

    expect_error(vcovDS.coxph("object"), "Object is not of class survival::coxph, please check the name", fixed = TRUE)
})

context("vcovDS.coxph::arg::number parameter value")
test_that("number parameter value vcovDS.coxph", {
    object <- 1.0

    expect_error(vcovDS.coxph("object"), "Object is not of class survival::coxph, please check the name", fixed = TRUE)
})

#
# Done
#

context("vcovDS.coxph::arg::shutdown")

context("vcovDS.coxph::arg::done")
