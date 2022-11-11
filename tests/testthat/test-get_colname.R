test_that("get_colname() selects the correct column names for given year and month", {
  expect_equal(get_colname(year = 2020, month = "June"),
               c("LRN", "prov_id", "FRN", "prov_name", "DBA", "hoco_name",
                 "hoco_num", "hoco_final", "StateAbbr", "cb_fips", "tech_code",
                 "consumer", "max_down", "max_up", "business")
               )
})
