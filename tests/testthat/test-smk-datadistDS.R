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

context("datadistDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("datadistDS::smk::simple min example")
test_that("simple min example", {
    data_arg      <- "data_obj"
    adjust_to_obj <- list(a = 'min')
    data_obj      <- data.frame(a = c(0, 1, 2, 3, 4, 5), b = c(4, 5, 6, 7, 8, 9))

    res <- datadistDS(data = data_arg, adjust_to_obj)
    expect_equal(class(res), "datadist")
    expect_length(res, 2)
    expect_equal(class(res[1]), "list")
    expect_equal(class(res[2]), "list")
})


context("datadistDS::smk::simple max example")
test_that("simple max example", {
    data_arg      <- "data_obj"
    adjust_to_obj <- list(a = 'max')
    data_obj      <- data.frame(a = c(0, 1, 2, 3, 4, 5), b = c(4, 5, 6, 7, 8, 9))

    res <- datadistDS(data = data_arg, adjust_to_obj)
    expect_equal(class(res), "datadist")
    expect_length(res, 2)
    expect_equal(class(res[1]), "list")
    expect_equal(class(res[2]), "list")
})


context("datadistDS::smk::simple mean example")
test_that("simple mean example", {
    data_arg      <- "data_obj"
    adjust_to_obj <- list(a = 'mean')
    data_obj      <- data.frame(a = c(0, 1, 2, 3, 4, 5), b = c(4, 5, 6, 7, 8, 9))

    res <- datadistDS(data = data_arg, adjust_to_obj)
    expect_equal(class(res), "datadist")
    expect_length(res, 2)
    expect_equal(class(res[1]), "list")
    expect_equal(class(res[2]), "list")
})

#
# Done
#

context("datadistDS::smk::shutdown")

context("datadistDS::smk::done")
