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

context("summarySurvDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("summarySurvDS::smk::simple example")
test_that("simple example", {
    object_obj <- Surv(time = c(1.0, 1.1, 1.2, 1.3, 1.4, 1.5), event = c(1, 1, 0, 1, 0, 0))

    res <- summarySurvDS(object = object_obj)
    
    expect_length(res, 2)
})

#
# Done
#

context("summarySurvDS::smk::shutdown")

context("summarySurvDS::smk::done")
