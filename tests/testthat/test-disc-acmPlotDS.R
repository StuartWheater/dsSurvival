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

context("acmPlotDS::disc::setup")

#
# Tests
#

context("acmPlotDS::disc::disclosure check not permissive")
test_that("disclosure check not permissive", {
    set.non_permissive.disclosure.settings()

    prep_obj <- "prep"
    prep     <- NULL

    expect_error(acmPlotDS(prep_obj), "This function requires the server to be in permissive mode", fixed = TRUE)
})

context("acmPlotDS::disc::disclosure check permissive")
test_that("disclosure check permissive", {
    set.permissive.disclosure.settings()

    prep_obj <- "prep"
    prep     <- NULL

    expect_error(acmPlotDS(prep_obj), "The provided object is not of class 'Predict'", fixed = TRUE)
})

context("acmPlotDS::disc::disclosure check avocado")
test_that("disclosure check avocado", {
    set.banana.disclosure.settings()

    prep_obj <- "prep"
    prep     <- NULL

    expect_error(acmPlotDS(prep_obj), "This function requires the server to be in permissive mode", fixed = TRUE)
})

context("acmPlotDS::disc::disclosure check banana")
test_that("disclosure check banana", {
    set.banana.disclosure.settings()

    prep_obj <- "prep"
    prep     <- NULL

    expect_error(acmPlotDS(prep_obj), "This function requires the server to be in permissive mode", fixed = TRUE)
})

#
# Done
#

context("acmPlotDS::disc::shutdown")

context("acmPlotDS::disc::done")
