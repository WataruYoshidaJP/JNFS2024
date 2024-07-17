#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Preparation for Decomposition ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

## 要因分解に用いる3グループ変数を作成

DF_use <- 
  DF_use |>   
  dplyr::mutate(Decomp3Grp = case_when(Sex == 2  & MarStatW == 5 ~ "Never married",
                                Sex == 4 & NofChildEvb == 0 ~ "Married w/o children",
                                Sex == 4 & NofChildEvb %in% c(1:8) ~ "Married w/ children")) |> 
  dplyr::mutate(Decomp2Grp_marry = case_when(Decomp3Grp == "Never married" ~ 1,
                                      Decomp3Grp %in% c("Married w/o children", "Married w/ children") ~ 0)) |> 
  dplyr::mutate(Decomp2Grp_child_in_marrige = case_when(Decomp3Grp == "Married w/o children" ~ 1,
                                                 Decomp3Grp == "Married w/ children" ~ 0)) |> 
  dplyr::mutate(Decomp2Grp_marry_without_child = case_when(Decomp3Grp == "Never married" ~ 1,
                                                    Decomp3Grp %in% "Married w/o children" ~ 0)) |> 
  dplyr::filter(!is.na(Decomp3Grp))


## 要因分解に用いるダミー変数を作成

DF_use <- 
  DF_use |> 
  dplyr::mutate(JobStatSvyFemale_Reg = if_else(JobStatSvyFemale == 1, 1, 0),
         JobStatSvyFemale_Part = if_else(JobStatSvyFemale == 2, 1, 0), 
         JobStatSvyFemale_Disp = if_else(JobStatSvyFemale == 3, 1, 0),
         JobStatSvyFemale_Self = if_else(JobStatSvyFemale == 4, 1, 0)) |> 
  dplyr::mutate(JobStatGrdFemale_Reg = if_else(JobStatGrdFemale == 1, 1, 0),
         JobStatGrdFemale_Part = if_else(JobStatGrdFemale == 2, 1, 0), 
         JobStatGrdFemale_Disp = if_else(JobStatGrdFemale == 3, 1, 0),
         JobStatGrdFemale_Self = if_else(JobStatGrdFemale == 4, 1, 0)) |> 
  dplyr::mutate(JobTypeSvyFemale_Self = if_else(JobTypeSvyFemale %in% c(1, 2), 1, 0),
         JobTypeSvyFemale_Pro = if_else(JobTypeSvyFemale == 3, 1, 0), 
         JobTypeSvyFemale_Mgr = if_else(JobTypeSvyFemale == 4, 1, 0),
         JobTypeSvyFemale_Cler = if_else(JobTypeSvyFemale == 5, 1, 0),
         JobTypeSvyFemale_Sales = if_else(JobTypeSvyFemale == 6, 1, 0),
         JobTypeSvyFemale_Fac = if_else(JobTypeSvyFemale == 7, 1, 0)) |> 
  dplyr::mutate(JobTypeGrdFemale_Self = if_else(JobTypeGrdFemale %in% c(1, 2), 1, 0),
         JobTypeGrdFemale_Pro = if_else(JobTypeGrdFemale == 3, 1, 0), 
         JobTypeGrdFemale_Mgr = if_else(JobTypeGrdFemale == 4, 1, 0),
         JobTypeGrdFemale_Cler = if_else(JobTypeGrdFemale == 5, 1, 0),
         JobTypeGrdFemale_Sales = if_else(JobTypeGrdFemale == 6, 1, 0),
         JobTypeGrdFemale_Fac = if_else(JobTypeGrdFemale == 7, 1, 0)) |> 
  dplyr::mutate(JobSizeSvyFemale_129 = if_else(JobSizeSvyFemale == 1, 1, 0),
         JobSizeSvyFemale_3099 = if_else(JobSizeSvyFemale == 2, 1, 0), 
         JobSizeSvyFemale_100299 = if_else(JobSizeSvyFemale == 3, 1, 0),
         JobSizeSvyFemale_300999 = if_else(JobSizeSvyFemale == 4, 1, 0),
         JobSizeSvyFemale_1000 = if_else(JobSizeSvyFemale == 5, 1, 0),
         JobSizeSvyFemale_Pub = if_else(JobSizeSvyFemale == 6, 1, 0)) |> 
  dplyr::mutate(JobSizeGrdFemale_129 = if_else(JobSizeGrdFemale == 1, 1, 0),
         JobSizeGrdFemale_3099 = if_else(JobSizeGrdFemale == 2, 1, 0), 
         JobSizeGrdFemale_100299 = if_else(JobSizeGrdFemale == 3, 1, 0),
         JobSizeGrdFemale_300999 = if_else(JobSizeGrdFemale == 4, 1, 0),
         JobSizeGrdFemale_1000 = if_else(JobSizeGrdFemale == 5, 1, 0),
         JobSizeGrdFemale_Pub = if_else(JobSizeGrdFemale == 6, 1, 0)) |> 
  dplyr::mutate(SchoolGrd6GrpFemale_JH = if_else(SchoolGrd6GrpFemale == 1, 1, 0),
         SchoolGrd6GrpFemale_High = if_else(SchoolGrd6GrpFemale == 2, 1, 0), 
         SchoolGrd6GrpFemale_Voc = if_else(SchoolGrd6GrpFemale == 3, 1, 0),
         SchoolGrd6GrpFemale_JC = if_else(SchoolGrd6GrpFemale == 4, 1, 0),
         SchoolGrd6GrpFemale_Univ = if_else(SchoolGrd6GrpFemale == 5, 1, 0),
         SchoolGrd6GrpFemale_Gra = if_else(SchoolGrd6GrpFemale == 6, 1, 0)) 
