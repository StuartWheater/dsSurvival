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

context("predictDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("predictDS::smk::simple example")
test_that("simple example", {
    dataObj <- data.frame(y = c(1, 2, 3, 4, 5, 6), age = c(1, 2, 3, 4, 5, 6))
    dd      <- datadist(dataObj)
    options(datadist = dd)
    fit_var <- "fit_obj"
    fit_obj <- ols(y ~ age, data = dataObj)

    res <- predictDS(fit = fit_var)
    
    expect_length(res, 5)
    res.classes <- class(res)
    expect_true("Predict" %in% res.classes)
    expect_true("data.frame" %in% res.classes)
})

#
# Done
#

context("predictDS::smk::shutdown")

context("predictDS::smk::done")
