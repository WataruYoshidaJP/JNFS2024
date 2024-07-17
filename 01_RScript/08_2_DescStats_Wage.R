#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Creating Descriptive Graphs ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

## 2) Wage Distribution ----

### 2-1) by group ----

PLOT <-
  DF_use |>   
  ggplot() +
  geom_violin(aes(x = fct_rev(Decomp3Grp),
             y = LogWageW,
             fill = Decomp3Grp)) +
  geom_boxplot(aes(x = fct_rev(Decomp3Grp),
                   y = LogWageW),
               width = 0.2) +
  xlab("") +
  ylab("Logged Hourly Wage (yen)") +
  theme_few() +
  geom_hline(yintercept = log(500), linewidth = 0.2, linetype = "dashed") +
  geom_hline(yintercept = log(1000), linewidth = 0.2) +
  geom_hline(yintercept = log(1500), linewidth = 0.2, linetype = "dashed") +
  geom_hline(yintercept = log(2000), linewidth = 0.2) +
  annotate("text", x = 0.6, y = log(500) + 0.1, label = "500 yen") +
  annotate("text", x = 0.6, y = log(1000) + 0.1, label = "1,000 yen") +
  annotate("text", x = 0.6, y = log(1500) + 0.1, label = "1,500 yen") +
  annotate("text", x = 0.6, y = log(2000) + 0.1, label = "2,000 yen") +
  theme(axis.ticks = element_line(colour = "black"),
        axis.text.x = element_text (size = rel(1.3), colour = "black"),
        axis.text.y = element_text (size = rel(1.0), colour = "black"),
        panel.spacing.x = unit(4, "mm"),
        legend.position = "bottom",
        axis.title.x = element_text(size = rel(1.0)),
        axis.title.y = element_text(size = rel(1.0)),
        strip.text   = element_text(size = rel(1.0)),
        text         = element_text(size = 16),
        legend.text  = element_text(size = rel(1.0)),
        plot.title   = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.caption.position = "plot") +
  # guides(fill = guide_legend(nrow = 1, reverse = T)) +
  guides(fill = FALSE) +
  labs(fill = NULL)
print(PLOT)


ggsave(plot     = PLOT,
       filename = paste0(OutDir, "Desc/Distribution_of_DVs/LogWage_group.png"),
       device   = "png",
       width    = 10,
       height   = 7,
       dpi      = 600)


###  2-2) by age  ----

PLOT <-
  DF_use |>   
  filter(!is.na(Decomp3Grp) & !is.na(CAgeSvyW5y)) |> 
  ggplot() +
  geom_violin(aes(x = fct_rev(Decomp3Grp),
             y = LogWageW,
             fill = Decomp3Grp)) +
  geom_boxplot(aes(x = fct_rev(Decomp3Grp),
                  y = LogWageW),
               width = 0.2) +
  facet_wrap( ~ CAgeSvyW5y) +
  xlab("") +
  ylab("Logged Hourly Wage (yen)") +
  theme_few() +
  geom_hline(yintercept = log(500), linewidth = 0.2, linetype = "dashed") +
  geom_hline(yintercept = log(1000), linewidth = 0.2) +
  geom_hline(yintercept = log(1500), linewidth = 0.2, linetype = "dashed") +
  geom_hline(yintercept = log(2000), linewidth = 0.2) +
  theme(axis.ticks = element_line(colour = "black"),
        # axis.text.x = element_blank(), # x軸ラベルは削除
        axis.text.x = element_text (size = rel(0.7), colour = "black"),
        axis.text.y = element_text (size = rel(1.0), colour = "black"),
        panel.spacing.x = unit(4, "mm"),
        legend.position = "bottom",
        axis.title.x = element_text(size = rel(1.0)),
        axis.title.y = element_text(size = rel(1.0)),
        strip.text   = element_text(size = rel(1.0)),
        text         = element_text(size = 16),
        legend.text  = element_text(size = rel(1.0)),
        plot.title   = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.caption.position = "plot") +
  # guides(fill = guide_legend(nrow = 1, reverse = T)) +
  guides(fill = FALSE) +
  labs(fill = NULL)
print(PLOT)


ggsave(plot     = PLOT,
       filename = paste0(OutDir, "Desc/Distribution_of_DVs/LogWage_age.png"),
       device   = "png",
       width    = 10,
       height   = 7,
       dpi      = 600)


###  2-3) by educational attainment ----

PLOT <-
  DF_use |>   
  filter(!is.na(Decomp3Grp) & !is.na(SchoolGrd6GrpFemale)) |>   
  ggplot() +
  geom_violin(aes(x = fct_rev(Decomp3Grp),
             y = LogWageW,
             fill = Decomp3Grp)) +
  geom_boxplot(aes(x = fct_rev(Decomp3Grp),
                  y = LogWageW),
               width = 0.2) +
  facet_wrap( ~ SchoolGrd6GrpFemale) +
  xlab("") +
  ylab("Logged Hourly Wage (yen)") +
  theme_few() +
  geom_hline(yintercept = log(500), linewidth= 0.2, linetype = "dashed") +
  geom_hline(yintercept = log(1000), linewidth = 0.2) +
  geom_hline(yintercept = log(1500), linewidth= 0.2, linetype = "dashed") +
  geom_hline(yintercept = log(2000), linewidth = 0.2) +
  theme(axis.ticks = element_line(colour = "black"),
        axis.text.x = element_text (size = rel(0.6), colour = "black"),
        axis.text.y = element_text (size = rel(1.0), colour = "black"),
        panel.spacing.x = unit(4, "mm"),
        legend.position = "bottom",
        axis.title.x = element_text(size = rel(1.0)),
        axis.title.y = element_text(size = rel(1.0)),
        strip.text   = element_text(size = rel(1.0)),
        text         = element_text(size = 16),
        legend.text  = element_text(size = rel(1.0)),
        plot.title   = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.caption.position = "plot") +
  # guides(fill = guide_legend(nrow = 1, reverse = T)) +
  guides(fill = FALSE) +
  labs(fill = NULL)
print(PLOT)


ggsave(plot     = PLOT,
       filename = paste0(OutDir, "Desc/Distribution_of_DVs/LogWage_education.png"),
       device   = "png",
       width    = 10,
       height   = 7,
       dpi      = 600)


###  2-4-1) by employment status ----

PLOT <-
  DF_use |>   
  filter(!is.na(Decomp3Grp) & JobStatSvyFemale %in% c("Regular",
                                                      "Part-time",
                                                      "Dispatched/temporary",
                                                      "Self-employed")) |>  
  ggplot() +
  geom_violin(aes(x = fct_rev(Decomp3Grp),
             y = LogWageW,
             fill = Decomp3Grp)) +
  geom_boxplot(aes(x = fct_rev(Decomp3Grp),
                  y = LogWageW),
               width = 0.2) +
  facet_wrap( ~ JobStatSvyFemale) +
  xlab("") +
  ylab("Logged Hourly Wage (yen)") +
  theme_few() +
  ggtitle("Employment Status") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 12)) +
  geom_hline(yintercept = log(500), linewidth = 0.2, linetype = "dashed") +
  geom_hline(yintercept = log(1000), linewidth = 0.2) +
  geom_hline(yintercept = log(1500), linewidth = 0.2, linetype = "dashed") +
  geom_hline(yintercept = log(2000), linewidth = 0.2) +
  theme(axis.ticks = element_line(colour = "black"),
        axis.text.x = element_text (size = rel(0.9), colour = "black"),
        axis.text.y = element_text (size = rel(1.0), colour = "black"),
        panel.spacing.x = unit(4, "mm"),
        legend.position = "bottom",
        axis.title.x = element_text(size = rel(1.0)),
        axis.title.y = element_text(size = rel(1.0)),
        strip.text   = element_text(size = rel(1.0)),
        text         = element_text(size = 16),
        legend.text  = element_text(size = rel(1.0)),
        plot.title   = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.caption.position = "plot") +
  # guides(fill = guide_legend(nrow = 1, reverse = T)) +
  guides(fill = "none") +
  labs(fill = NULL)
print(PLOT)


ggsave(plot     = PLOT,
       filename = paste0(OutDir, "Desc/Distribution_of_DVs/LogWage_employment_status.png"),
       device   = "png",
       width    = 10,
       height   = 5.5,
       dpi      = 600)


###  2-4-2) by occupation ----

PLOT <-
  DF_use |>   
  filter(!is.na(Decomp3Grp) & !is.na(JobTypeSvyFemale)) |>   
  ggplot() +
  geom_violin(aes(x = fct_rev(Decomp3Grp),
             y = LogWageW,
             fill = Decomp3Grp)) +
  geom_boxplot(aes(x = fct_rev(Decomp3Grp),
                  y = LogWageW),
               width = 0.2) +
  facet_wrap( ~ JobTypeSvyFemale) +
  xlab("") +
  ylab("Logged Hourly Wage (yen)") +
  theme_few() +
  ggtitle("Occupation Type") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 12)) +
  geom_hline(yintercept = log(500), linewidth = 0.2, linetype = "dashed") +
  geom_hline(yintercept = log(1000), linewidth = 0.2) +
  geom_hline(yintercept = log(1500), linewidth = 0.2, linetype = "dashed") +
  geom_hline(yintercept = log(2000), linewidth = 0.2) +
  theme(axis.ticks = element_line(colour = "black"),
        axis.text.x = element_text (size = rel(0.9), colour = "black"),
        axis.text.y = element_text (size = rel(1.0), colour = "black"),
        panel.spacing.x = unit(4, "mm"),
        legend.position = "bottom",
        axis.title.x = element_text(size = rel(1.0)),
        axis.title.y = element_text(size = rel(1.0)),
        strip.text   = element_text(size = rel(1.0)),
        text         = element_text(size = 16),
        legend.text  = element_text(size = rel(1.0)),
        plot.title   = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.caption.position = "plot") +
  # guides(fill = guide_legend(nrow = 1, reverse = T)) +
  guides(fill = "none") +
  labs(fill = NULL)
print(PLOT)


ggsave(plot     = PLOT,
       filename = paste0(OutDir, "Desc/Distribution_of_DVs/LogWage_occupation.png"),
       device   = "png",
       width    = 10,
       height   = 5.5,
       dpi      = 600)


###  2-4-3) by firm size ----

PLOT <-
  DF_use |>   
  filter(!is.na(Decomp3Grp) & !is.na(JobSizeSvyFemale)) |>   
  ggplot() +
  geom_violin(aes(x = fct_rev(Decomp3Grp),
             y = LogWageW,
             fill = Decomp3Grp)) +
  geom_boxplot(aes(x = fct_rev(Decomp3Grp),
                  y = LogWageW),
               width = 0.2) +
  facet_wrap( ~ JobSizeSvyFemale) +
  xlab("") +
  ylab("Logged Hourly Wage (yen)") +
  theme_few() +
  ggtitle("N of Employees") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 12)) +
  geom_hline(yintercept = log(500), linewidth = 0.2, linetype = "dashed") +
  geom_hline(yintercept = log(1000), linewidth = 0.2) +
  geom_hline(yintercept = log(1500), linewidth = 0.2, linetype = "dashed") +
  geom_hline(yintercept = log(2000), linewidth = 0.2) +
  theme(axis.ticks = element_line(colour = "black"),
        axis.text.x = element_text (size = rel(0.9), colour = "black"),
        axis.text.y = element_text (size = rel(1.0), colour = "black"),
        panel.spacing.x = unit(4, "mm"),
        legend.position = "bottom",
        axis.title.x = element_text(size = rel(1.0)),
        axis.title.y = element_text(size = rel(1.0)),
        strip.text   = element_text(size = rel(1.0)),
        text         = element_text(size = 16),
        legend.text  = element_text(size = rel(1.0)),
        plot.title   = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.caption.position = "plot") +
  # guides(fill = guide_legend(nrow = 1, reverse = T)) +
  guides(fill = "none") +
  labs(fill = NULL)
print(PLOT)


ggsave(plot     = PLOT,
       filename = paste0(OutDir, "Desc/Distribution_of_DVs/LogWage_firmsize.png"),
       device   = "png",
       width    = 10,
       height   = 5.5,
       dpi      = 600)


###  2-5) by tenure: Boxplot ----

PLOT <-
  DF_use |>   
  filter(!is.na(Decomp3Grp) & !is.na(JobYearsSvyFemale6Grp)) |>   
  ggplot() +
  geom_violin(aes(x = fct_rev(Decomp3Grp),
             y = LogWageW,
             fill = Decomp3Grp)) +
  geom_boxplot(aes(x = fct_rev(Decomp3Grp),
                  y = LogWageW),
               width = 0.2) +
  facet_wrap( ~ JobYearsSvyFemale6Grp) +
  xlab("") +
  ylab("Logged Hourly Wage (yen)") +
  theme_few() +
  ggtitle("Years in Current Job") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 12)) +
  geom_hline(yintercept = log(500), linewidth = 0.2, linetype = "dashed") +
  geom_hline(yintercept = log(1000), linewidth = 0.2) +
  geom_hline(yintercept = log(1500), linewidth = 0.2, linetype = "dashed") +
  geom_hline(yintercept = log(2000), linewidth = 0.2) +
  theme(axis.ticks = element_line(colour = "black"),
        axis.text.x = element_text (size = rel(0.9), colour = "black"),
        axis.text.y = element_text (size = rel(1.0), colour = "black"),
        panel.spacing.x = unit(4, "mm"),
        legend.position = "bottom",
        axis.title.x = element_text(size = rel(1.0)),
        axis.title.y = element_text(size = rel(1.0)),
        strip.text   = element_text(size = rel(1.0)),
        text         = element_text(size = 16),
        legend.text  = element_text(size = rel(1.0)),
        plot.title   = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.caption.position = "plot") +
  # guides(fill = guide_legend(nrow = 1, reverse = T)) +
  guides(fill = "none") +
  labs(fill = NULL)
print(PLOT)


ggsave(plot     = PLOT,
       filename = paste0(OutDir, "Desc/Distribution_of_DVs/LogWage_tenure.png"),
       device   = "png",
       width    = 10,
       height   = 5.5,
       dpi      = 600)

