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

context("cox.zphSLMADS::arg::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("cox.zphSLMADS::arg::no parameters")
test_that("no parameters", {
    expect_error(cox.zphSLMADS(), "The name of the server-side fit Cox prportional hazards model must be set for use in survival::cox.zph()", fixed = TRUE)
})

context("cox.zphSLMADS::arg::fit is NULL")
test_that("fit is NULL", {
    expect_error(cox.zphSLMADS(fit = NULL), "The name of the server-side fit Cox prportional hazards model must be set for use in survival::cox.zph()", fixed = TRUE)
})

context("cox.zphSLMADS::arg::missing fit")
test_that("missing fir", {
    fit_obj <- "fit_obj"

    expect_error(cox.zphSLMADS(fit = fit_obj), "argument must be the result of Cox model fit", fixed = TRUE)
})



#
# Done
#

context("cox.zphSLMADS::arg::shutdown")

context("cox.zphSLMADS::arg::done")
