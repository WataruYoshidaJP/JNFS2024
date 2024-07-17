#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Decomposition ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

## 6) Never-married vs. Married (Monthly Income) ----

DF_decomp <-
  DF_use |> 
  filter(Decomp3Grp %in% c("Never married", "Married w/o children", "Married w/ children")) |> 
  filter(!is.na(LogWageW))


decomp6 <- oaxaca(formula = log(IncLMonthFemale) ~ SchoolGrd6GrpFemale_JH + SchoolGrd6GrpFemale_Voc + SchoolGrd6GrpFemale_JC + SchoolGrd6GrpFemale_Univ + SchoolGrd6GrpFemale_Gra + CAgeSvyW + YearSvy
                    + JobStatSvyFemale_Reg + JobStatSvyFemale_Part + JobStatSvyFemale_Self + JobYearsSvyFemale
                    + JobTypeSvyFemale_Self + JobTypeSvyFemale_Pro + JobTypeSvyFemale_Mgr + JobTypeSvyFemale_Cler + JobTypeSvyFemale_Fac
                    + JobSizeSvyFemale_3099 + JobSizeSvyFemale_100299 + JobSizeSvyFemale_300999 + JobSizeSvyFemale_1000 + JobSizeSvyFemale_Pub
                    | Decomp2Grp_marry | SchoolGrd6GrpFemale_JH + SchoolGrd6GrpFemale_Voc + SchoolGrd6GrpFemale_JC + SchoolGrd6GrpFemale_Univ + SchoolGrd6GrpFemale_Gra, data = DF_decomp, R = 1000)

PLOT <- 
  decomp6 |> 
  plot(components = c("endowments", "coefficients"),
       variables = c("SchoolGrd6GrpFemale_JH", "SchoolGrd6GrpFemale_Voc", "SchoolGrd6GrpFemale_JC", "SchoolGrd6GrpFemale_Univ", "SchoolGrd6GrpFemale_Gra",
                     "JobStatSvyFemale_Reg", "JobStatSvyFemale_Part", "JobStatSvyFemale_Self", "JobYearsSvyFemale",
                     "JobTypeSvyFemale_Self", "JobTypeSvyFemale_Pro", "JobTypeSvyFemale_Mgr", "JobTypeSvyFemale_Cler", "JobTypeSvyFemale_Fac",
                     "JobSizeSvyFemale_3099", "JobSizeSvyFemale_100299", "JobSizeSvyFemale_300999", "JobSizeSvyFemale_1000", "JobSizeSvyFemale_Pub"),
       variable.labels = c("SchoolGrd6GrpFemale_JH" = "Educational Attainment: Junior high", 
                           "SchoolGrd6GrpFemale_Voc" = "Vocational",
                           "SchoolGrd6GrpFemale_JC" = "Junior college",
                           "SchoolGrd6GrpFemale_Univ" = "University",
                           "SchoolGrd6GrpFemale_Gra" = "Graduate",
                           "JobStatSvyFemale_Reg" = "Employment Status: Regular employee (Current)",
                           "JobStatSvyFemale_Part" = "Parttime (Current)", 
                           "JobStatSvyFemale_Self" = "Self-employed (Current)",
                           "JobYearsSvyFemale" = "Years in Current Job",
                           "JobTypeSvyFemale_Self" = "Occupation Type: Self-employed",
                           "JobTypeSvyFemale_Pro" = "Professional",
                           "JobTypeSvyFemale_Mgr" = "Managerial", 
                           "JobTypeSvyFemale_Cler" = "Clerical",
                           "JobTypeSvyFemale_Fac" = "Factory labor",
                           "JobSizeSvyFemale_3099" = "N of Employees: 30-99 employees",
                           "JobSizeSvyFemale_100299" = "100-299",
                           "JobSizeSvyFemale_300999" = "300-999",
                           "JobSizeSvyFemale_1000" = "1000+",
                           "JobSizeSvyFemale_Pub" = "Public"),
       title = " Never married vs. Married: Monthly income")
print(PLOT)

ggsave(plot     = PLOT,
       filename = paste0(OutDir, "Decompostion/MonthlyIncome_Decompose_marriage.png"),
       device   = "png",
       width    = 10,
       height   = 7,
       dpi      = 600)


rm(DF_decomp)


## 7) Married w/o children vs. Married w/ children (Monthly Income) ----

DF_decomp <-
  DF_use |> 
  filter(Decomp3Grp %in% c("Married w/o children", "Married w/ children")) |> 
  filter(!is.na(LogWageW))



decomp7 <- oaxaca(formula = log(IncLMonthFemale) ~ SchoolGrd6GrpFemale_JH + SchoolGrd6GrpFemale_Voc + SchoolGrd6GrpFemale_JC + SchoolGrd6GrpFemale_Univ + SchoolGrd6GrpFemale_Gra + CAgeSvyW + YearSvy
                    + JobStatSvyFemale_Reg + JobStatSvyFemale_Part + JobStatSvyFemale_Self + JobYearsSvyFemale
                    + JobTypeSvyFemale_Self + JobTypeSvyFemale_Pro + JobTypeSvyFemale_Mgr + JobTypeSvyFemale_Cler + JobTypeSvyFemale_Fac
                    + JobSizeSvyFemale_3099 + JobSizeSvyFemale_100299 + JobSizeSvyFemale_300999 + JobSizeSvyFemale_1000 + JobSizeSvyFemale_Pub
                    | Decomp2Grp_child_in_marrige | SchoolGrd6GrpFemale_JH + SchoolGrd6GrpFemale_Voc + SchoolGrd6GrpFemale_JC + SchoolGrd6GrpFemale_Univ + SchoolGrd6GrpFemale_Gra, data = DF_decomp, R = 1000)

PLOT <- 
  decomp7 |> 
  plot(components = c("endowments", "coefficients"),
       variables = c("SchoolGrd6GrpFemale_JH", "SchoolGrd6GrpFemale_Voc", "SchoolGrd6GrpFemale_JC", "SchoolGrd6GrpFemale_Univ", "SchoolGrd6GrpFemale_Gra",
                     "JobStatSvyFemale_Reg", "JobStatSvyFemale_Part", "JobStatSvyFemale_Self", "JobYearsSvyFemale",
                     "JobTypeSvyFemale_Self", "JobTypeSvyFemale_Pro", "JobTypeSvyFemale_Mgr", "JobTypeSvyFemale_Cler", "JobTypeSvyFemale_Fac",
                     "JobSizeSvyFemale_3099", "JobSizeSvyFemale_100299", "JobSizeSvyFemale_300999", "JobSizeSvyFemale_1000", "JobSizeSvyFemale_Pub"),
       variable.labels = c("SchoolGrd6GrpFemale_JH" = "Educational Attainment: Junior high", 
                           "SchoolGrd6GrpFemale_Voc" = "Vocational",
                           "SchoolGrd6GrpFemale_JC" = "Junior college",
                           "SchoolGrd6GrpFemale_Univ" = "University",
                           "SchoolGrd6GrpFemale_Gra" = "Graduate",
                           "JobStatSvyFemale_Reg" = "Employment Status: Regular employee (Current)",
                           "JobStatSvyFemale_Part" = "Parttime (Current)", 
                           "JobStatSvyFemale_Self" = "Self-employed (Current)",
                           "JobYearsSvyFemale" = "Years in Current Job",
                           "JobTypeSvyFemale_Self" = "Occupation Type: Self-employed",
                           "JobTypeSvyFemale_Pro" = "Professional",
                           "JobTypeSvyFemale_Mgr" = "Managerial", 
                           "JobTypeSvyFemale_Cler" = "Clerical",
                           "JobTypeSvyFemale_Fac" = "Factory labor",
                           "JobSizeSvyFemale_3099" = "N of Employees: 30-99 employees",
                           "JobSizeSvyFemale_100299" = "100-299",
                           "JobSizeSvyFemale_300999" = "300-999",
                           "JobSizeSvyFemale_1000" = "1000+",
                           "JobSizeSvyFemale_Pub" = "Public"),
       title = " Married w/o children vs. Married w/ children: Monthly income ")
print(PLOT)

ggsave(plot     = PLOT,
       filename = paste0(OutDir, "Decompostion/MonthlyIncome_Decompose_children_in_marriage.png"),
       device   = "png",
       width    = 10,
       height   = 7,
       dpi      = 600)


rm(DF_decomp)
