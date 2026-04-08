
# NB workforce table
nb_workforce <- tibble::tribble(
  ~occupation_raw,             ~n_emp, ~n_ft, ~n_contract, ~salary,
  "Executive",                    12,    12,      0,        500000,
  "Vice President",               54,    54,      0,        250000,
  "Director",                    111,   111,      0,        150000,
  "HR",                          426,   426,      0,         80000,
  "IT",                          568,   426,    142,         85000,
  "Legal",                       142,    85,     57,        110000,
  "Finance & Accounting",        426,   398,     28,        100000,
  "Environmental Scientists",    711,   569,    142,        120000,
  "Safety Officer",             2132,  2132,      0,         80000,
  "Medical Personnel",          1421,  1137,    284,        130000,
  "Geologist",                   152,   152,      0,        120000,
  "Scientist",                   211,   169,     42,        120000,
  "Field technician",            842,   421,    421,         55000,
  "Drilling operators",         2526,  1684,    842,         60000,
  "Maintenance" ,              10316,  8253,   2063,         65000,
  "Maintenance A",              1137,  1174,      0,         65000,
  "Engineers",                  2684,  2684,      0,         95000,
  "Freight operators",          3553,  2842,    711,         60000,
  "Robotics technician",         711,   711,      0,         75000,
  "Navigation officers",        2842,  2842,      0,         85000,
  "Security personnel",         2132,  1421,    711,         55000,
  "Steward",                     568,   568,      0,         65000,
  "Galleyhand",                 2132,     0,   2132,         40000
)

# Occupation mapping to model occupations
occupation_map <- c(
  "Executive"                 = "Executive",
  "Vice President"            = "Manager",
  "Director"                  = "Manager",
  "HR"                        = "Administrator",
  "IT"                        = "Administrator",
  "Legal"                     = "Administrator",
  "Finance & Accounting"      = "Administrator",
  "Environmental Scientists"  = "Safety Officer",
  "Safety Officer"            = "Safety Officer",
  "Medical Personnel"         = "Safety Officer",
  "Geologist"                 = "Scientist",
  "Scientist"                 = "Scientist",
  "Field technician"          = "Planetary Operations",
  "Drilling operators"        = "Drill Operator",
  "Maintenance"               = "Maintenance Staff",
  "Maintenance A"             = "Maintenance Staff",
  "Engineers"                 = "Engineer",
  "Freight operators"         = "Planetary Operations",
  "Robotics technician"       = "Engineer",
  "Navigation officers"       = "Spacecraft Operator",
  "Security personnel"        = "Spacecraft Operator",
  "Steward"                   = "Spacecraft Operator",
  "Galleyhand"                = "Spacecraft Operator"
)

library(dplyr)
library(tidyr)
library(purrr)

worker_influx <- tibble::tribble(
  ~occupation,              ~yr1,  ~yr2, ~yr3, ~yr4, ~yr5, ~yr6, ~yr7, ~yr8, ~yr9, ~yr10,
  "Administrator",          1562,   38,   38,   38,   38,   38,   38,   38,   38,    38,
  "Drill Operator",         2526,   61,   61,   61,   61,   61,   61,   61,   61,    61,
  "Engineer",               3395,   82,   82,   82,   82,   82,   82,   82,   82,    82,
  "Executive",                12,    0,    1,    0,    0,    0,    1,    0,    0,     1,
  "Maintenance Staff",     11490,  277,  277,  277,  277,  277,  277,  277,  277,   277,
  "Manager",                 165,    4,    4,    4,    4,    4,    4,    4,    4,     4,
  "Planetary Operations",   4395,  106,  106,  106,  106,  106,  106,  106,  106,   106,
  "Safety Officer",         4264,  103,  103,  103,  103,  103,  103,  103,  103,   103,
  "Scientist",               363,    9,    9,    9,    9,    9,    9,    9,    9,     9,
  "Spacecraft Operator",    7674,  185,  185,  185,  185,  185,  185,  185,  185,   185
)