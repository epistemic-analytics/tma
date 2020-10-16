test_data = read.csv(system.file(package = "tma", 'extdata/test_data.csv'), stringsAsFactors = FALSE);
usethis::use_data(test_data, overwrite = TRUE)


test_data_empties = read.csv(system.file(package = "tma", 'extdata/test_data_with_empty_rows.csv'), stringsAsFactors = FALSE);
usethis::use_data(test_data_empties, overwrite = TRUE)


test_rsdata = read.csv(system.file(package = "tma", 'extdata/rs.data.csv'), stringsAsFactors = FALSE);
usethis::use_data(test_rsdata, overwrite = TRUE)