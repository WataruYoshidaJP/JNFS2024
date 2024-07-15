#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Coefplot for all cases----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

tidy_reg  <- 
  lm(LogWageW ~ relevel(as.factor(SchoolGrd6GrpFemale), ref = "High") + CAgeSvyW + YearSvy + 
       relevel(as.factor(JobStatSvyFemale), ref = "Dispatched/temporary") + JobYearsSvyFemale +
       relevel(as.factor(JobTypeSvyFemale), ref = "Sales/service") +
       relevel(as.factor(JobSizeSvyFemale), ref = "1-29 employees") + as.factor(Decomp3Grp), data = DF_use) |>  
  tidy(conf.int = TRUE) |>  
  mutate(term = case_when(term == '(Intercept)' ~ "Intercept",
                          term == 'relevel(as.factor(SchoolGrd6GrpFemale), ref = "High")Junior high' ~ "Educational Attainment: Junior high",
                          term == 'relevel(as.factor(SchoolGrd6GrpFemale), ref = "High")Vocational' ~ "Vocational",
                          term == 'relevel(as.factor(SchoolGrd6GrpFemale), ref = "High")Junior college' ~ "Junior college",
                          term == 'relevel(as.factor(SchoolGrd6GrpFemale), ref = "High")University' ~ "University",
                          term == 'relevel(as.factor(SchoolGrd6GrpFemale), ref = "High")Graduate' ~ "Graduate",
                          term == 'CAgeSvyW' ~ "Age at the survey",
                          term == 'YearSvy' ~ "Survey year",
                          term == 'relevel(as.factor(JobStatSvyFemale), ref = "Dispatched/temporary")Regular' ~ "Employment Status: Regular employee (Current)",
                          term == 'relevel(as.factor(JobStatSvyFemale), ref = "Dispatched/temporary")Part-time' ~ "Parttime (Current)",
                          term == 'relevel(as.factor(JobStatSvyFemale), ref = "Dispatched/temporary")Self-employed' ~ "Self-employed (Current)",
                          term == 'JobYearsSvyFemale' ~ "Years in Current Job",
                          term == 'relevel(as.factor(JobTypeSvyFemale), ref = "Sales/service")Self-employed' ~ "Occupation Type: Self-employed",
                          term == 'relevel(as.factor(JobTypeSvyFemale), ref = "Sales/service")Professional' ~ "Professional",
                          term == 'relevel(as.factor(JobTypeSvyFemale), ref = "Sales/service")Managerial' ~ "Managerial",
                          term == 'relevel(as.factor(JobTypeSvyFemale), ref = "Sales/service")Clerical' ~ "Clerical",
                          term == 'relevel(as.factor(JobTypeSvyFemale), ref = "Sales/service")Factory labor' ~ "Factory labor",
                          term == 'relevel(as.factor(JobSizeSvyFemale), ref = "1-29 employees")30-99' ~ "N of Employees: 30-99 employees",
                          term == 'relevel(as.factor(JobSizeSvyFemale), ref = "1-29 employees")100-299' ~ "100-299",
                          term == 'relevel(as.factor(JobSizeSvyFemale), ref = "1-29 employees")300-999' ~ "300-999",
                          term == 'relevel(as.factor(JobSizeSvyFemale), ref = "1-29 employees")1000 or more' ~ "1000+",
                          term == 'relevel(as.factor(JobSizeSvyFemale), ref = "1-29 employees")Public' ~ "Public employees",
                          term == 'as.factor(Decomp3Grp)Married w/o children' ~ "Married w/o children",
                          term == 'as.factor(Decomp3Grp)Never married' ~ "Never married")) |>  
  mutate(term = fct_relevel(term, c("Intercept",
                                    "Educational Attainment: Junior high",
                                    "Vocational",
                                    "Junior college",
                                    "University",
                                    "Graduate",
                                    "Age at the survey",
                                    "Survey year",
                                    "Employment Status: Regular employee (Current)",
                                    "Parttime (Current)",
                                    "Self-employed (Current)",
                                    "Years in Current Job",
                                    "Occupation Type: Self-employed",
                                    "Professional",
                                    "Managerial",
                                    "Clerical",
                                    "Factory labor",
                                    "N of Employees: 30-99 employees",
                                    "100-299",
                                    "300-999",
                                    "1000+",
                                    "Public employees",
                                    "Married w/o children",
                                    "Never married")))　|> 
  mutate(term = fct_rev(term))


PLOT <- tidy_reg |> 
  filter(term != "Intercept" & term != "Never married" & term != "Married w/o children"
         & term != "Age at the survey" & term != "Survey year") |> 
  ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, height = 0.1)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Estimate of effect of variable on logged hourly wage (in yen)",
    y = NULL
  )

ggsave(plot     = PLOT,
       filename = paste0(OutDir, "coefs/coefplot_for_all_cases.png"),
       device   = "png",
       width    = 10,
       height   = 7,
       dpi      = 600)
