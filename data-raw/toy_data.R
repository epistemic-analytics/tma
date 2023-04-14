test_reddit = read.csv(system.file(package = "tma", 'extdata/redditextractbyr_Formatted.csv'), stringsAsFactors = FALSE)
usethis::use_data(test_reddit, overwrite = TRUE)

test_reddit2 = read.csv(system.file(package = "tma", 'extdata/reddit_output_2020_09_16.csv'), stringsAsFactors = FALSE)[-777,];
test_reddit2$Author2 <- make.names(test_reddit2$Author)
usethis::use_data(test_reddit2, overwrite = TRUE)

RS.data = read.csv(system.file(package = "tma", 'extdata/rs.data.csv'), stringsAsFactors = FALSE)
RS.data$Context <- "Public"
usethis::use_data(RS.data, overwrite = TRUE)

test_smalldata = read.csv(system.file(package = "tma", 'extdata/test_data_3.csv'), stringsAsFactors = FALSE)
usethis::use_data(test_smalldata, overwrite = TRUE)
