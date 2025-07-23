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

context("rmsTransDS::disc::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("rmsTransDS::smk::transformation is dummy")
test_that("transformation is dummy", {
    x              <- "D"
    transformation <- "dummy"
    parms          <- NULL

    D <- c(1, 2)

    expect_error(rmsTransDS(x = x, transformation = transformation, parms = parms), "Insufficient data points for transformation. Minimum required: 3", fixed = TRUE)
})


#
# Done
#

context("rmsTransDS::disc::shutdown")

context("rmsTransDS::disc::done")
