#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Creating Descriptive Graphs ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

## 4) Monthly Income Distribution ----

### 4-1) by group: Boxplot ----

PLOT <-
  DF_use |>   
  filter(!is.na(Decomp3Grp)) |>   
  ggplot(aes(x = fct_rev(Decomp3Grp),
             y = log(IncLMonthFemale),
             fill = Decomp3Grp)) +
  geom_boxplot() +
  xlab("") +
  ylab("Logged Monthly Income (yen)") +
  theme_few() +
  theme(axis.ticks = element_line(colour = "black"),
        axis.text.x = element_text (size = rel(1.0), colour = "black"),
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
  guides(fill = guide_legend(nrow = 1, reverse = T)) +
  labs(fill = NULL)
print(PLOT)


ggsave(plot     = PLOT,
       filename = paste0(OutDir, "Desc/Distribution_of_OtherVs/MonthlyIncome/LogMonthIncome_group_box.png"),
       device   = "png",
       width    = 10,
       height   = 7,
       dpi      = 600)


#### Violin Plot ----

PLOT <-
  DF_use |>   
  filter(!is.na(Decomp3Grp)) |>   
  ggplot(aes(x = fct_rev(Decomp3Grp),
             y = log(IncLMonthFemale),
             fill = Decomp3Grp)) +
  geom_violin() +
  xlab("") +
  ylab("Logged Monthly Income (yen)") +
  theme_few() +
  geom_hline(yintercept = log(50000), linewidth = 0.2, linetype = "dashed") +
  geom_hline(yintercept = log(100000), linewidth = 0.2) +
  geom_hline(yintercept = log(150000), linewidth = 0.2, linetype = "dashed") +
  geom_hline(yintercept = log(200000), linewidth = 0.2) +
  geom_hline(yintercept = log(250000), linewidth = 0.2, linetype = "dashed") +
  geom_hline(yintercept = log(300000), linewidth = 0.2) +
  annotate("text", x = 0.6, y = log(50000) + 0.1, label = "50,000 yen") +
  annotate("text", x = 0.6, y = log(100000) + 0.1, label = "100,000 yen") +
  annotate("text", x = 0.6, y = log(150000) + 0.1, label = "150,000 yen") +
  annotate("text", x = 0.6, y = log(200000) + 0.1, label = "200,000 yen") +
  annotate("text", x = 0.6, y = log(250000) + 0.1, label = "250,000 yen") +
  annotate("text", x = 0.6, y = log(300000) + 0.1, label = "300,000 yen") +
  theme(axis.ticks = element_line(colour = "black"),
        axis.text.x = element_text (size = rel(1.0), colour = "black"),
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
  guides(fill = guide_legend(nrow = 1, reverse = T)) +
  labs(fill = NULL)
print(PLOT)


ggsave(plot     = PLOT,
       filename = paste0(OutDir, "Desc/Distribution_of_OtherVs/MonthlyIncome/LogMonthIncome_group_violin.png"),
       device   = "png",
       width    = 10,
       height   = 7,
       dpi      = 600)


###  4-2-1) by employment status: Boxplot ----

PLOT <-
  DF_use |>   
  filter(!is.na(Decomp3Grp) & JobStatSvyFemale %in% c("Regular",
                                                      "Part-time",
                                                      "Dispatched/temporary",
                                                      "Self-employed")) |>   
  ggplot(aes(x = fct_rev(Decomp3Grp),
             y = log(IncLMonthFemale),
             fill = Decomp3Grp)) +
  geom_boxplot() +
  facet_wrap( ~ JobStatSvyFemale) +
  xlab("") +
  ylab("Logged Monthly Income (yen)") +
  theme_few() +
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
  guides(fill = guide_legend(nrow = 1, reverse = T)) +
  labs(fill = NULL)
print(PLOT)


ggsave(plot     = PLOT,
       filename = paste0(OutDir, "Desc/Distribution_of_OtherVs/MonthlyIncome/LogMonthIncome_employment_status_box.png"),
       device   = "png",
       width    = 10,
       height   = 9,
       dpi      = 600)


#### Violin plot ----

PLOT <-
  DF_use |>   
  filter(!is.na(Decomp3Grp) & JobStatSvyFemale %in% c("Regular",
                                                      "Part-time",
                                                      "Dispatched/temporary",
                                                      "Self-employed")) |>   
  ggplot(aes(x = fct_rev(Decomp3Grp),
             y = log(IncLMonthFemale),
             fill = Decomp3Grp)) +
  geom_violin() +
  facet_wrap( ~ JobStatSvyFemale) +
  xlab("") +
  ylab("Logged Monthly Income (yen)") +
  theme_few() +
  geom_hline(yintercept = log(50000), linewidth = 0.2, linetype = "dashed") +
  geom_hline(yintercept = log(100000), linewidth = 0.2) +
  geom_hline(yintercept = log(150000), linewidth = 0.2, linetype = "dashed") +
  geom_hline(yintercept = log(200000), linewidth = 0.2) +
  geom_hline(yintercept = log(250000), linewidth = 0.2, linetype = "dashed") +
  geom_hline(yintercept = log(300000), linewidth = 0.2) +
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
  guides(fill = guide_legend(nrow = 1, reverse = T)) +
  labs(fill = NULL)
print(PLOT)


ggsave(plot     = PLOT,
       filename = paste0(OutDir, "Desc/Distribution_of_OtherVs/MonthlyIncome/LogMonthIncome_employment_status_violin.png"),
       device   = "png",
       width    = 10,
       height   = 9,
       dpi      = 600)


###  4-2-2) by occupation: Boxplot ----

PLOT <-
  DF_use |>   
  filter(!is.na(Decomp3Grp) & !is.na(JobTypeSvyFemale)) |>   
  ggplot(aes(x = fct_rev(Decomp3Grp),
             y = log(IncLMonthFemale),
             fill = Decomp3Grp)) +
  geom_boxplot() +
  facet_wrap( ~ JobTypeSvyFemale) +
  xlab("") +
  ylab("Logged Monthly Income (yen)") +
  theme_few() +
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
  guides(fill = guide_legend(nrow = 1, reverse = T)) +
  labs(fill = NULL)
print(PLOT)


ggsave(plot     = PLOT,
       filename = paste0(OutDir, "Desc/Distribution_of_OtherVs/MonthlyIncome/LogMonthIncome_occupation_box.png"),
       device   = "png",
       width    = 10,
       height   = 9,
       dpi      = 600)


#### Violin plot ----

PLOT <-
  DF_use |>   
  filter(!is.na(Decomp3Grp) & !is.na(JobTypeSvyFemale)) |>   
  ggplot(aes(x = fct_rev(Decomp3Grp),
             y = log(IncLMonthFemale),
             fill = Decomp3Grp)) +
  geom_violin() +
  facet_wrap( ~ JobTypeSvyFemale) +
  xlab("") +
  ylab("Logged Monthly Income (yen)") +
  theme_few() +
  geom_hline(yintercept = log(50000), linewidth = 0.2, linetype = "dashed") +
  geom_hline(yintercept = log(100000), linewidth = 0.2) +
  geom_hline(yintercept = log(150000), linewidth = 0.2, linetype = "dashed") +
  geom_hline(yintercept = log(200000), linewidth = 0.2) +
  geom_hline(yintercept = log(250000), linewidth = 0.2, linetype = "dashed") +
  geom_hline(yintercept = log(300000), linewidth = 0.2) +
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
  guides(fill = guide_legend(nrow = 1, reverse = T)) +
  labs(fill = NULL)
print(PLOT)


ggsave(plot     = PLOT,
       filename = paste0(OutDir, "Desc/Distribution_of_OtherVs/MonthlyIncome/LogMonthIncome_occupation_violin.png"),
       device   = "png",
       width    = 10,
       height   = 9,
       dpi      = 600)


###  4-2-3) by firm size: Boxplot ----

PLOT <-
  DF_use |>   
  filter(!is.na(Decomp3Grp) & !is.na(JobSizeSvyFemale)) |>   
  ggplot(aes(x = fct_rev(Decomp3Grp),
             y = log(IncLMonthFemale),
             fill = Decomp3Grp)) +
  geom_boxplot() +
  facet_wrap( ~ JobSizeSvyFemale) +
  xlab("") +
  ylab("Logged Monthly Income (yen)") +
  theme_few() +
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
  guides(fill = guide_legend(nrow = 1, reverse = T)) +
  labs(fill = NULL)
print(PLOT)


ggsave(plot     = PLOT,
       filename = paste0(OutDir, "Desc/Distribution_of_OtherVs/MonthlyIncome/LogMonthIncome_firmsize_box.png"),
       device   = "png",
       width    = 10,
       height   = 9,
       dpi      = 600)


#### Violin plot ----

PLOT <-
  DF_use |>   
  filter(!is.na(Decomp3Grp) & !is.na(JobSizeSvyFemale)) |>   
  ggplot(aes(x = fct_rev(Decomp3Grp),
             y = log(IncLMonthFemale),
             fill = Decomp3Grp)) +
  geom_violin() +
  facet_wrap( ~ JobSizeSvyFemale) +
  xlab("") +
  ylab("Logged Monthly Income (yen)") +
  theme_few() +
  geom_hline(yintercept = log(50000), linewidth = 0.2, linetype = "dashed") +
  geom_hline(yintercept = log(100000), linewidth = 0.2) +
  geom_hline(yintercept = log(150000), linewidth = 0.2, linetype = "dashed") +
  geom_hline(yintercept = log(200000), linewidth = 0.2) +
  geom_hline(yintercept = log(250000), linewidth = 0.2, linetype = "dashed") +
  geom_hline(yintercept = log(300000), linewidth = 0.2) +
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
  guides(fill = guide_legend(nrow = 1, reverse = T)) +
  labs(fill = NULL)
print(PLOT)


ggsave(plot     = PLOT,
       filename = paste0(OutDir, "Desc/Distribution_of_OtherVs/MonthlyIncome/LogMonthIncome_firmsize_violin.png"),
       device   = "png",
       width    = 10,
       height   = 9,
       dpi      = 600)
