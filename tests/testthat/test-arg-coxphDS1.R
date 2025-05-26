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

context("coxphDS1::arg::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("coxphDS1::arg::all paramters are NULL")
test_that("all paramters are NULL", {
    expect_error(coxphDS1(df = NULL, time_col = NULL, censor_col = NULL), "incorrect number of dimensions", fixed = TRUE)
})

#
# Done
#

context("coxphDS1::arg::shutdown")

context("coxphDS1::arg::done")
