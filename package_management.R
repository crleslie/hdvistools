use_package("dplyr")
use_package("zoo")
use_package("lubridate")





# trafx_extract tests -----------------------------------------------------

test_data <- trafx_extract("R/data/ShuttleFile  190502-ALL_DB.TXT", out = "data")
test_head <- trafx_extract("R/data/ShuttleFile  190502-ALL_DB.TXT", out = "head")
test_flat <- trafx_extract("R/data/ShuttleFile  190502-ALL_DB.TXT", out = "flat")

test_data <- trafx_extract("R/data", out = "data")
test_head <- trafx_extract("R/data", out = "head")
test_flat <- trafx_extract("R/data", out = "flat")