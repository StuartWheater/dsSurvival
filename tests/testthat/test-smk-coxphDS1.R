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

context("coxphDS1::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("coxphDS1::smk::simple example")
test_that("simple example", {
    dataframe <- data.frame(time = c(0.1, 0.1, 0.1, 0.2, 0.2, 0.2, 0.3, 0.3, 0.3), cens = c(1, 1, 1, 1, 1, 1, 1, 1, 1), age = c(22, 23, 24, 25, 26, 27, 26, 28, 29),
y = c(100, 105, 110, 90, 95, 100, 100, 105, 101))

    df_arg         <- "dataframe"
    time_col_arg   <- "time"
    censor_col_arg <- "cens"

    res <- coxphDS1(df = df_arg, time_col = time_col_arg, censor_col = censor_col_arg)

    expect_length(res, 6)
})

#
# Done
#

context("coxphDS1::smk::shutdown")

context("coxphDS1::smk::done")
