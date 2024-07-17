#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Keeping Necessary Cases ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=


## 調査時年齢30-49歳の未婚女性と、夫婦の場合は妻初婚の女性（夫再婚は含む）

DF_use <- 
  DF  |>    
  dplyr::filter(CAgeSvyW %in% c(30:49) & (Sex == 2 & MarStatW == 5 | Sex == 4 & MarStat2W == 1)) |>  
  dplyr::filter_at(vars(LogWageW, JobStatSvyFemale, JobTypeSvyFemale, JobSizeSvyFemale, JobYearsSvyFemale, CAgeSvyW, SchoolGrd6GrpFemale), all_vars(!is.na(.))) #分析で使う変数についてリストワイズ処理
  
