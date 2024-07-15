#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Creating Variables ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

## 1 Wage and Career ----

tempDF <- 
  DF_org |>
  dplyr::mutate(tempIncLMonthW = if_else(IncLMonthW %in% c(0, 9999), NA, IncLMonthW),
         tempIncLMonthH = if_else(IncLMonthH %in% c(0, 9999), NA, IncLMonthH),
         tempIncLYear = if_else(IncCYear_imp == 0, NA, IncCYear_imp),
         tempIncLMonth = tempIncLYear * 1/12,
         tempIncLYearNumW = if_else(IncLYearNumW %in% c(0, 9999), NA, IncLYearNumW),
         tempIncLYearNumH = if_else(IncLYearNumH %in% c(0, 9999), NA, IncLYearNumH),
         tempIncLYearNum = if_else(IncLYearNum %in% c(0, 9999), NA, IncLYearNum),
         JobYearsSvyW = if_else(JobYearsSvyW == 99, NA, JobYearsSvyW),
         JobYearsSvyH = if_else(JobYearsSvyH == 99, NA, JobYearsSvyH),
         JobYearsSvy = if_else(JobYearsSvy == 99, NA, JobYearsSvy),
         tempWorkTimeWeekW = if_else(WorkTimeWeekW %in% c(0, 999), NA, WorkTimeWeekW),
         tempWorkTimeWeekH = if_else(WorkTimeWeekH %in% c(0, 999), NA, WorkTimeWeekH),
         tempWorkTimeWeek = if_else(WorkTimeWeek %in% c(0, 999), NA, WorkTimeWeek)) |> 
  dplyr::mutate(tempWorkTimeMonthW = tempWorkTimeWeekW * 31 / 7, #5月の1週間の労働時間を月変換
         tempWorkTimeMonthH = tempWorkTimeWeekH * 31 / 7,
         tempWorkTimeMonth = tempWorkTimeWeek * 31 / 7) |> 
  dplyr::mutate(WageW = case_when(Sex == 4 ~ tempIncLMonthW * 10000 / tempWorkTimeMonthW,
                           Sex == 2 ~ tempIncLMonth * 10000 / tempWorkTimeMonth),
         WageH = case_when(Sex == 3 ~ tempIncLMonthH * 10000 / tempWorkTimeMonthH,
                           Sex == 1 ~ tempIncLMonth * 10000 / tempWorkTimeMonth),
         altWageW = case_when(Sex == 4 ~ (tempIncLYearNumW * 1/12 * 10000) / tempWorkTimeMonthW,　 #altは年収から作成した時間あたり賃金の近似値
                              Sex == 2 ~ (tempIncLYearNum * 1/12 * 10000) / tempWorkTimeMonth),
         altWageH = case_when(Sex == 3 ~ (tempIncLYearNumH * 1/12 * 10000) / tempWorkTimeMonthH,
                              Sex == 1 ~ (tempIncLYearNum * 1/12 * 10000) / tempWorkTimeMonth)) |> 
  dplyr::mutate(WorkTimeWeekFemale = case_when(Sex == 4 ~ tempWorkTimeWeekW, #独身者と有配偶者をあわせた変数
                                       Sex == 2 ~ tempWorkTimeWeek),
         WorkTimeWeekMale = case_when(Sex == 3 ~ tempWorkTimeWeekH,
                                       Sex == 1 ~ tempWorkTimeWeek),
         IncLMonthFemale = case_when(Sex == 4 ~ tempIncLMonthW * 10000,
                                    Sex == 2 ~ tempIncLMonth * 10000),
         IncLMonthMale = case_when(Sex == 3 ~ tempIncLMonthH * 10000,
                                    Sex == 1 ~ tempIncLMonth * 10000)) |> 
  dplyr::mutate(LogWageW = log(WageW),
         LogWageH = log(WageH),
         altLogWageW = log(altWageW),
         altLogWageH = log(altWageH),
         LogIncomeW = case_when(Sex == 4 ~ log(tempIncLYearNumW  * 10000), #年収を円単位に変換
                                Sex == 2 ~ log(tempIncLYearNum  * 10000)),
         LogIncomeH = case_when(Sex == 3 ~ log(tempIncLYearNumH  * 10000),
                                Sex == 1 ~ log(tempIncLYearNum  * 10000)),
         JobYearsSvyFemale = case_when(Sex == 4 ~ JobYearsSvyW,
                                       Sex == 2 ~ JobYearsSvy),
         JobYearsSvyMale = case_when(Sex == 3 ~ JobYearsSvyH,
                                     Sex == 1 ~ JobYearsSvy))


## 2. Job ----

tempDF <- 
  tempDF |> 
  dplyr::mutate(across(starts_with("JobStat") & -ends_with("PriPub"), ~if_else(. %in% c(7, 9), NA, .))) |> 
  dplyr::mutate(across(starts_with("JobType"), ~if_else(. == 9, NA, .))) |>  
  dplyr::mutate(across(starts_with("JobSize") & -ends_with("PriPub"), ~if_else(. == 9, NA, .))) |>
  dplyr::mutate(JobStatSvyFemale = case_when(Sex == 4 ~ JobStatSvyW,
                                      Sex == 2 ~ JobStatSvy),
         JobStatSvyMale = case_when(Sex == 3 ~ JobStatSvyH,
                                    Sex == 1 ~ JobStatSvy),
         JobStatGrdFemale = case_when(Sex == 4 ~ JobStatGrdW,
                                      Sex == 2 ~ JobStatGrd),
         JobStatGrdMale = case_when(Sex == 3 ~ JobStatGrdH,
                                    Sex == 1 ~ JobStatGrd)) |>
  dplyr::mutate(JobTypeSvyFemale = case_when(Sex == 4 ~ JobTypeSvyW,
                                      Sex == 2 ~ JobTypeSvy),
         JobTypeSvyMale = case_when(Sex == 3 ~ JobTypeSvyH,
                                    Sex == 1 ~ JobTypeSvy),
         JobTypeGrdFemale = case_when(Sex == 4 ~ JobTypeGrdW,
                                      Sex == 2 ~ JobTypeGrd),
         JobTypeGrdMale = case_when(Sex == 3 ~ JobTypeGrdH,
                                    Sex == 1 ~ JobTypeGrd)) |>
  dplyr::mutate(tempJobSizeSvyW = JobSizeSvyW,
         tempJobSizeSvyH = JobSizeSvyH,
         tempJobSizeGrdW = JobSizeGrdW,
         tempJobSizeGrdH = JobSizeGrdH) |> 
  dplyr::mutate(across(c("tempJobSizeSvyW", "tempJobSizeSvyH", "tempJobSizeGrdW", "tempJobSizeGrdH"), ~case_when(. %in% c(1,2) ~ 1,
                                                                                                          . == 3 ~ 2,
                                                                                                          . == 4 ~ 3,
                                                                                                          . == 5 ~ 4,
                                                                                                          . == 6 ~ 5,
                                                                                                          . == 7 ~ 6))) |> 
  dplyr::mutate(JobSizeSvyFemale = case_when(Sex == 4 ~ tempJobSizeSvyW,
                                      Sex == 2 ~ JobSizeSvy),
         JobSizeSvyMale = case_when(Sex == 3 ~ tempJobSizeSvyH,
                                    Sex == 1 ~ JobSizeSvy),
         JobSizeGrdFemale = case_when(Sex == 4 ~ tempJobSizeGrdW,
                                      Sex == 2 ~ JobSizeGrd),
         JobSizeGrdMale = case_when(Sex == 3 ~ tempJobSizeGrdH,
                                    Sex == 1 ~ JobSizeGrd))


## 3. Births ----
### Only for couples

tempDF <- 
  tempDF |> 
  dplyr::mutate(across(c("NofChildEvb", "NofChildLiv", "NofChildEvbEverW"), ~if_else(. == 99, NA, .))) |> 
  dplyr::mutate(across(starts_with("CAgeNat") & ends_with("W"), ~if_else(. == 99, NA, .)))



## 4. Educational Attainment ----

tempDF <- 
  tempDF |> 
  dplyr::mutate(SchoolGrd6GrpFemale = case_when(Sex == 4 & GradW != 2 ~ SchoolGrd11GrpW, #在学中を除く
                                         Sex == 2 & Grad != 2  ~ SchoolGrd11Grp),
         SchoolGrd6GrpMale = case_when(Sex == 3 & GradH != 2  ~ SchoolGrd11GrpH,
                                       Sex == 1 & Grad != 2  ~ SchoolGrd11Grp)) |> 
  dplyr::mutate(across(c("SchoolGrd6GrpFemale", "SchoolGrd6GrpMale"), ~case_when(. == 1 ~ 1,
                                                                          . %in% c(2, 3, 10) ~ 2,
                                                                          . == 4 ~ 3,
                                                                          . == 5 ~ 4,
                                                                          . %in% c(6, 7, 11) ~ 5,
                                                                          . == 8 ~ 6,
                                                                          . %in% c(9, 99) ~ NA))) |> 
  dplyr::mutate(SchoolGrdYearFemale = case_when(Sex == 4 ~ SchoolGrdYearW, 
                                         Sex == 2 ~ SchoolGrdYear),
         SchoolGrdYearMale = case_when(Sex == 3 ~ SchoolGrdYearH, 
                                       Sex == 1 ~ SchoolGrdYear)) |> 
  dplyr::mutate(across(c("SchoolGrdYearFemale", "SchoolGrdYearMale"), ~if_else(. == 99, NA, .))) 


## 5. Other Variables ----

tempDF <-
  tempDF |> 
  dplyr::mutate(across(c("MarStat2W", "MarStat2H", "StrMarStat"), ~if_else(. == 9, NA, .)))   

