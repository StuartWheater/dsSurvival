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

context("useDatadistDS::arg::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("useDatadistDS::arg::no parameter")
test_that("no parameter useDatadistDS", {
    expect_error(useDatadistDS(), "Please provide the name of a datadist object", fixed = TRUE)
})

context("useDatadistDS::arg::invalid parameter")
test_that("invalid parameter useDatadistDS", {
    obj <- c()

    expect_error(useDatadistDS("obj"), "The provided object is not a datadist object", fixed = TRUE)
})

#
# Done
#

context("useDatadistDS::arg::shutdown")

context("useDatadistDS::arg::done")
