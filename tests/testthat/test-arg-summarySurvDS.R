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

context("summarySurvDS::arg::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("summarySurvDS::arg::object is NULL")
test_that("object is NULL", {
    expect_error(summarySurvDS(object = NULL), "The input object must be a survival::Surv object", fixed = TRUE)
})

#
# Done
#

context("summarySurvDS::arg::shutdown")

context("summarySurvDS::arg::done")
