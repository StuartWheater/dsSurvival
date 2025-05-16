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

context("datadistDS::arg::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("datadistDS::arg::data not data.frame")
test_that("data not data.frame", {
    dataName_arg <- "dataName"
    dataName     <- c()

    expect_error(datadistDS(data = dataName_arg), "The provided object is not a data frame", fixed = TRUE)
})

context("datadistDS::arg::data is data.frame")
test_that("data is data.frame", {
    data_arg <- "data"
    data     <- data.frame()

#    TODO: Check Test
#    expect_error(datadistDS(data = data_arg), "The provided object is not a data frame", fixed = TRUE)
})

#
# Done
#

context("datadistDS::arg::shutdown")

context("datadistDS::arg::done")
