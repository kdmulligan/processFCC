test_that("get_colname() selects the correct column names for 2020 June", {
  expect_equal(get_colname(year = 2020, month = "June"),
               c("LRN", "prov_id", "FRN", "prov_name", "DBA", "hoco_name",
                 "hoco_num", "hoco_final", "StateAbbr", "cb_fips", "tech_code",
                 "consumer", "max_down", "max_up", "business")
               )
})

test_that("get_colname() selects the correct column names for 2017 December", {
  expect_equal(get_colname(year = 2017, month = "Dec"),
               c("LRN", "prov_id", "FRN", "prov_name", "DBA", "hoco_name",
                 "hoco_num", "hoco_final", "StateAbbr", "cb_fips", "tech_code",
                 "consumer", "max_down", "max_up", "business",
                 "max_down_bus", "max_up_bus")
  )
})

test_that("get_colname() only accepts June or Dec as month", {
  expect_error(get_colname(year = 2019, month = "December"))
})
