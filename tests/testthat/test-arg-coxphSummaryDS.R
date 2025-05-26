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

context("coxphSummaryDS::arg::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("coxphSummaryDS::arg::x is NULL")
test_that("x is NULL", {
    expect_error(coxphSummaryDS(x = NULL), "The name of the server-side fit Cox proportional hazards model must be set", fixed = TRUE)
})

#
# Done
#

context("coxphSummaryDS::arg::shutdown")

context("coxphSummaryDS::arg::done")
