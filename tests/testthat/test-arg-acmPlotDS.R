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

context("acmPlotDS::arg::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("acmPlotDS::arg::no parameters")
test_that("no parameters", {
    expect_error(acmPlotDS(), "invalid first argument", fixed = TRUE)
})

context("acmPlotDS::arg::pred_obj is NULL")
test_that("pred_obj is NULL", {
    expect_error(acmPlotDS(pred_obj = NULL), "invalid first argument", fixed = TRUE)
})

context("acmPlotDS::arg::missing pred_obj")
test_that("missing pred_obj", {
    pred_obj <- "pred"

    expect_error(acmPlotDS(pred_obj = pred_obj), "The specified pred_obj does not exist", fixed = TRUE)
})



#
# Done
#

context("acmPlotDS::arg::shutdown")

context("acmPlotDS::arg::done")
