#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Changing Variable&Value Labels ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

DF_use <- 
  DF_use |>   
  dplyr::mutate(CAgeSvyW5y = case_when(CAgeSvyW %in% c(30:34) ~ "30-34",
                                       CAgeSvyW %in% c(35:39) ~ "35-39",
                                       CAgeSvyW %in% c(40:44) ~ "40-44",
                                       CAgeSvyW %in% c(45:49) ~ "45-49")) |>   
  dplyr::mutate(across(c("JobStatSvyFemale", "JobStatGrdFemale"), ~case_when(. == 1 ~ "Regular",
                                                                             . == 2 ~ "Part-time",
                                                                             . == 3 ~ "Dispatched/temporary",
                                                                             . == 4 ~ "Self-employed",
                                                                             . == 5 ~ "Unemployed",
                                                                             . == 6 ~ "Student"))) |>   
  dplyr::mutate(across(c("JobStatSvyFemale", "JobStatGrdFemale"), ~factor(., levels = c("Regular",
                                                                                        "Part-time",
                                                                                        "Dispatched/temporary",
                                                                                        "Self-employed",
                                                                                        "Unemployed",
                                                                                        "Student")))) |>   
  dplyr::mutate(across(c("JobTypeSvyFemale", "JobTypeGrdFemale"), ~case_when(. %in% c(1:2) ~ "Self-employed",
                                                                                    . == 3 ~ "Professional",
                                                                                    . == 4 ~ "Managerial",
                                                                                    . == 5 ~ "Clerical",
                                                                                    . == 6 ~ "Sales/service",
                                                                                    . == 7 ~ "Factory labor"))) |>   
  dplyr::mutate(across(c("JobTypeSvyFemale", "JobTypeGrdFemale"), ~factor(., levels = c("Self-employed",
                                                                                        "Professional",
                                                                                        "Managerial",
                                                                                        "Clerical",
                                                                                        "Sales/service",
                                                                                        "Factory labor")))) |>   
  dplyr::mutate(across(c("JobSizeSvyFemale", "JobSizeGrdFemale"), ~case_when(. == 1 ~ "1-29 employees",
                                                                             . == 2 ~ "30-99",
                                                                             . == 3 ~ "100-299",
                                                                             . == 4 ~ "300-999",
                                                                             . == 5 ~ "1000 or more",
                                                                             . == 6 ~ "Public"))) |>   
  dplyr::mutate(across(c("JobSizeSvyFemale", "JobSizeGrdFemale"), ~factor(., levels = c("1-29 employees",
                                                                                        "30-99",
                                                                                        "100-299",
                                                                                        "300-999",
                                                                                        "1000 or more",
                                                                                        "Public")))) |>   
  dplyr::mutate(SchoolGrd6GrpFemale = case_when(SchoolGrd6GrpFemale == 1 ~ "Junior high",
                                                SchoolGrd6GrpFemale == 2 ~ "High",
                                                SchoolGrd6GrpFemale == 3 ~ "Vocational",
                                                SchoolGrd6GrpFemale == 4 ~ "Junior college",
                                                SchoolGrd6GrpFemale == 5 ~ "University",
                                                SchoolGrd6GrpFemale == 6 ~ "Graduate")) |>   
  dplyr::mutate(SchoolGrd6GrpFemale  = factor(SchoolGrd6GrpFemale, levels = c("Junior high",
                                                                              "High",
                                                                              "Vocational",
                                                                              "Junior college",
                                                                              "University",
                                                                              "Graduate"))) |>   
  dplyr::mutate(JobYearsSvyFemale6Grp = case_when(JobYearsSvyFemale == 1 ~ "1 year or shorter",
                                           JobYearsSvyFemale %in% c(2:3) ~ "2-3",
                                           JobYearsSvyFemale %in% c(4:5) ~ "4-5",
                                          JobYearsSvyFemale %in% c(6:10) ~ "6-10",
                                         JobYearsSvyFemale %in% c(11:15) ~ "11-15",
                                         JobYearsSvyFemale %in% c(16:98) ~ "16+")) |>   
  dplyr::mutate(JobYearsSvyFemale6Grp  = factor(JobYearsSvyFemale6Grp, levels = c("1 year or shorter",
                                                                                  "2-3",
                                                                                  "4-5",
                                                                                  "6-10",
                                                                                  "11-15",
                                                                                  "16+")))
