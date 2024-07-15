#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Creating Descriptive Graphs ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

## 3) WorkTime Distribution ----

### 3-1) by group: Boxplot ----

PLOT <-
  DF_use |>   
  filter(!is.na(Decomp3Grp)) |>   
  ggplot(aes(x = fct_rev(Decomp3Grp),
             y = WorkTimeWeekFemale,
             fill = Decomp3Grp)) +
  geom_boxplot() +
  xlab("") +
  ylab("Working Time per Week (hours)") +
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
       filename = paste0(OutDir, "Desc/Distribution_of_OtherVs/WorkTime/WorkTime_group_box.png"),
       device   = "png",
       width    = 10,
       height   = 7,
       dpi      = 600)


#### Violin Plot ----

PLOT <-
  DF_use |>   
  filter(!is.na(Decomp3Grp)) |>   
  ggplot(aes(x = fct_rev(Decomp3Grp),
             y = WorkTimeWeekFemale,
             fill = Decomp3Grp)) +
  geom_violin() +
  xlab("") +
  ylab("Working Time per Week (hours)") +
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
       filename = paste0(OutDir, "Desc/Distribution_of_OtherVs/WorkTime/WorkTime_group_violin.png"),
       device   = "png",
       width    = 10,
       height   = 7,
       dpi      = 600)


###  3-2-1) by employment status: Boxplot ----

PLOT <-
  DF_use |>   
  filter(!is.na(Decomp3Grp) & JobStatSvyFemale %in% c("Regular",
                                                      "Part-time",
                                                      "Dispatched/temporary",
                                                      "Self-employed")) |>   
  ggplot(aes(x = fct_rev(Decomp3Grp),
             y = WorkTimeWeekFemale,
             fill = Decomp3Grp)) +
  geom_boxplot() +
  facet_wrap( ~ JobStatSvyFemale) +
  xlab("") +
  ylab("Working Time per Week (hours)") +
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
       filename = paste0(OutDir, "Desc/Distribution_of_OtherVs/WorkTime/WorkTime_employment_status_box.png"),
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
             y = WorkTimeWeekFemale,
             fill = Decomp3Grp)) +
  geom_violin() +
  facet_wrap( ~ JobStatSvyFemale) +
  xlab("") +
  ylab("Working Time per Week (hours)") +
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
       filename = paste0(OutDir, "Desc/Distribution_of_OtherVs/WorkTime/WorkTime_employment_status_violin.png"),
       device   = "png",
       width    = 10,
       height   = 9,
       dpi      = 600)


###  3-2-2) by occupation: Boxplot ----

PLOT <-
  DF_use |>   
  filter(!is.na(Decomp3Grp) & !is.na(JobTypeSvyFemale)) |>   
  ggplot(aes(x = fct_rev(Decomp3Grp),
             y = WorkTimeWeekFemale,
             fill = Decomp3Grp)) +
  geom_boxplot() +
  facet_wrap( ~ JobTypeSvyFemale) +
  xlab("") +
  ylab("Working Time per Week (hours)") +
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
       filename = paste0(OutDir, "Desc/Distribution_of_OtherVs/WorkTime/WorkTime_occupation_box.png"),
       device   = "png",
       width    = 10,
       height   = 9,
       dpi      = 600)


#### Violin plot ----

PLOT <-
  DF_use |>   
  filter(!is.na(Decomp3Grp) & !is.na(JobTypeSvyFemale)) |>   
  ggplot(aes(x = fct_rev(Decomp3Grp),
             y = WorkTimeWeekFemale,
             fill = Decomp3Grp)) +
  geom_violin() +
  facet_wrap( ~ JobTypeSvyFemale) +
  xlab("") +
  ylab("Working Time per Week (hours)") +
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
       filename = paste0(OutDir, "Desc/Distribution_of_OtherVs/WorkTime/WorkTime_occupation_violin.png"),
       device   = "png",
       width    = 10,
       height   = 9,
       dpi      = 600)


###  3-2-3) by firm size: Boxplot ----

PLOT <-
  DF_use |>   
  filter(!is.na(Decomp3Grp) & !is.na(JobSizeSvyFemale)) |>   
  ggplot(aes(x = fct_rev(Decomp3Grp),
             y = WorkTimeWeekFemale,
             fill = Decomp3Grp)) +
  geom_boxplot() +
  facet_wrap( ~ JobSizeSvyFemale) +
  xlab("") +
  ylab("Working Time per Week (hours)") +
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
       filename = paste0(OutDir, "Desc/Distribution_of_OtherVs/WorkTime/WorkTime_firmsize_box.png"),
       device   = "png",
       width    = 10,
       height   = 9,
       dpi      = 600)


#### Violin plot ----

PLOT <-
  DF_use |>   
  filter(!is.na(Decomp3Grp) & !is.na(JobSizeSvyFemale)) |>   
  ggplot(aes(x = fct_rev(Decomp3Grp),
             y = WorkTimeWeekFemale,
             fill = Decomp3Grp)) +
  geom_violin() +
  facet_wrap( ~ JobSizeSvyFemale) +
  xlab("") +
  ylab("Working Time per Week (hours)") +
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
       filename = paste0(OutDir, "Desc/Distribution_of_OtherVs/WorkTime/WorkTime_firmsize_violin.png"),
       device   = "png",
       width    = 10,
       height   = 9,
       dpi      = 600)
