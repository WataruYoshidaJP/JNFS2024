#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Keeping Necessary Variables ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

DF <-
  tempDF |> 
  dplyr::select(JobYearsSvyW, JobYearsSvyH, JobYearsSvy, JobYearsSvyFemale, JobYearsSvyMale, 
         WageW, WageH, LogWageW, LogWageH, altWageW, altWageH, altLogWageW, altLogWageH, LogIncomeW, LogIncomeH,
         WorkTimeWeekFemale, WorkTimeWeekMale, IncLMonthFemale, IncLMonthMale,
         starts_with("JobStat") & -ends_with("PriPub"), starts_with("JobType"), starts_with("JobSize") & -ends_with("PriPub"),
         NofChildEvb, NofChildLiv, NofChildEvbEverW, starts_with("CAgeNat") & ends_with("W"),  CAgeSvyNatYg,
         CAgeCMarW, CAgeCMarH, CAgeFMarW, CAgeFMarDisW, CAgeFMarH, CAgeFMarDisH, CDurYearCMarSvy,
         SchoolGrd6GrpFemale, SchoolGrd6GrpMale, SchoolGrdYearFemale, SchoolGrdYearMale,
         SurveyRound, Sex, YearSvy, Prefecture, Chiiki7Grp, YearBthW, YearBthH, CAgeSvyW, CAgeSvyH, MarStat2W, MarStat2H, MarStatW, MarStatH, StrMarStat)


rm(tempDF)

haven::write_dta(DF,
          here::here("00_Data/JNFS_wage.dta"))
