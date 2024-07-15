#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Creating Descriptive Graphs ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

## 1) Distribution of Independent variables ----

### 1-1) by group: School Dist ----

DF_graph <- DF_use |>  
  group_by(Decomp3Grp, SchoolGrd6GrpFemale) |>  
  summarize(Freq = n()) |>  
  group_by(Decomp3Grp) |>  
  mutate(Prop = Freq / sum(Freq)) |>  
  ungroup()


PLOT <- 
  DF_graph |>  
  ggplot(aes(x     = Prop*100,
             y     = Decomp3Grp,
             group = fct_rev(SchoolGrd6GrpFemale))) + 
  geom_col(aes(fill  = fct_rev(SchoolGrd6GrpFemale),
               color = fct_rev(SchoolGrd6GrpFemale)),
           color = "black") +
  scale_fill_manual(values = c("gray20", "gray40", "gray65", "gray80", "gray90", "white")) +
  geom_text(aes(label = sprintf("%1.1f", Prop * 100), colour = SchoolGrd6GrpFemale),
            position = position_stack(vjust = .5),
            size = 5,
            show.legend = FALSE) +
  scale_colour_manual(values = c("black","black","black","white","white","white")) +
  xlab("（%）") +
  theme_few() +
  theme(axis.ticks = element_line(colour = "black"),
        axis.text.x = element_text (size = rel(2.0), colour = "black"),
        axis.text.y = element_text (size = rel(2.0), vjust = 0.5, colour = "black"),
        panel.spacing.x = unit(4, "mm"),
        legend.position = "bottom",
        axis.title.x = element_text(size = rel(1.2), hjust = 1.0),
        axis.title.y = element_text(size = rel(1.2)),
        strip.text   = element_text(size = rel(1.0)),
        text         = element_text(size = 10, family="mincho"),
        legend.text  = element_text(size = rel(1.2)),
        plot.title   = element_text(hjust = 0.5, family = "gothic", size = rel(1.6)),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.caption.position = "plot") +
  guides(fill = guide_legend(title = "Educational Attainment", nrow = 2, reverse = T)) +
  labs(y = NULL, fill = NULL)

ggsave(plot     = PLOT,
       filename = paste0(OutDir, "Desc/Distribution_of_IVs/SchoolGrd.png"),
       device   = "png",
       width    = 10,
       height   = 3,
       dpi      = 600)

rm(DF_graph)


### 1-2) by group: JobStatSvy ----

DF_graph <- DF_use |>  
  group_by(Decomp3Grp, JobStatSvyFemale) |>  
  summarize(Freq = n()) |>  
  group_by(Decomp3Grp) |>  
  mutate(Prop = Freq / sum(Freq)) |>  
  ungroup()


PLOT <- 
  DF_graph |>  
  ggplot(aes(x     = Prop*100,
             y     = Decomp3Grp,
             group = fct_rev(JobStatSvyFemale))) + 
  geom_col(aes(fill  = fct_rev(JobStatSvyFemale),
               color = fct_rev(JobStatSvyFemale)),
           color = "black") +
  scale_fill_manual(values = c("gray65", "gray80", "gray90", "white")) +
  geom_text(aes(label = sprintf("%1.1f", Prop * 100), colour = JobStatSvyFemale),
            position = position_stack(vjust = .5),
            size = 5,
            show.legend = FALSE) +
  scale_colour_manual(values = c("black","black","black","white")) +
  xlab("（%）") +
  theme_few() +
  theme(axis.ticks = element_line(colour = "black"),
        axis.text.x = element_text (size = rel(2.0), colour = "black"),
        axis.text.y = element_text (size = rel(2.0), vjust = 0.5, colour = "black"),
        panel.spacing.x = unit(4, "mm"),
        legend.position = "bottom",
        axis.title.x = element_text(size = rel(1.2), hjust = 1.0),
        axis.title.y = element_text(size = rel(1.2)),
        strip.text   = element_text(size = rel(1.0)),
        text         = element_text(size = 10, family="mincho"),
        legend.text  = element_text(size = rel(1.2)),
        plot.title   = element_text(hjust = 0.5, family = "gothic", size = rel(1.6)),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.caption.position = "plot") +
  guides(fill = guide_legend(title = "Employment Status", nrow = 2, reverse = T)) +
  labs(y = NULL, fill = NULL)

ggsave(plot     = PLOT,
       filename = paste0(OutDir, "Desc/Distribution_of_IVs/JobStatSvy.png"),
       device   = "png",
       width    = 10,
       height   = 3,
       dpi      = 600)

rm(DF_graph)


### 1-3) by group: JobTypeSvy ----

DF_graph <- DF_use |>  
  group_by(Decomp3Grp, JobTypeSvyFemale) |>  
  summarize(Freq = n()) |>  
  group_by(Decomp3Grp) |>  
  mutate(Prop = Freq / sum(Freq)) |>  
  ungroup()


PLOT <- 
  DF_graph |>  
  ggplot(aes(x     = Prop*100,
             y     = Decomp3Grp,
             group = fct_rev(JobTypeSvyFemale))) + 
  geom_col(aes(fill  = fct_rev(JobTypeSvyFemale),
               color = fct_rev(JobTypeSvyFemale)),
           color = "black") +
  scale_fill_manual(values = c("gray20", "gray40", "gray65", "gray80", "gray90", "white")) +
  geom_text(aes(label = sprintf("%1.1f", Prop * 100), colour = JobTypeSvyFemale),
            position = position_stack(vjust = .5),
            size = 5,
            show.legend = FALSE) +
  scale_colour_manual(values = c("black","black","black","white","white","white")) +
  xlab("（%）") +
  theme_few() +
  theme(axis.ticks = element_line(colour = "black"),
        axis.text.x = element_text (size = rel(2.0), colour = "black"),
        axis.text.y = element_text (size = rel(2.0), vjust = 0.5, colour = "black"),
        panel.spacing.x = unit(4, "mm"),
        legend.position = "bottom",
        axis.title.x = element_text(size = rel(1.2), hjust = 1.0),
        axis.title.y = element_text(size = rel(1.2)),
        strip.text   = element_text(size = rel(1.0)),
        text         = element_text(size = 10, family="mincho"),
        legend.text  = element_text(size = rel(1.2)),
        plot.title   = element_text(hjust = 0.5, family = "gothic", size = rel(1.6)),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.caption.position = "plot") +
  guides(fill = guide_legend(title = "Occupation Type", nrow = 2, reverse = T)) +
  labs(y = NULL, fill = NULL)

ggsave(plot     = PLOT,
       filename = paste0(OutDir, "Desc/Distribution_of_IVs/JobTypeSvy.png"),
       device   = "png",
       width    = 10,
       height   = 3,
       dpi      = 600)

rm(DF_graph)


### 1-4) by group: JobSizeSvy ----

DF_graph <- DF_use |>  
  group_by(Decomp3Grp, JobSizeSvyFemale) |>  
  summarize(Freq = n()) |>  
  group_by(Decomp3Grp) |>  
  mutate(Prop = Freq / sum(Freq)) |>  
  ungroup()


PLOT <- 
  DF_graph |>  
  ggplot(aes(x     = Prop*100,
             y     = Decomp3Grp,
             group = fct_rev(JobSizeSvyFemale))) + 
  geom_col(aes(fill  = fct_rev(JobSizeSvyFemale),
               color = fct_rev(JobSizeSvyFemale)),
           color = "black") +
  scale_fill_manual(values = c("gray20", "gray40", "gray65", "gray80", "gray90", "white")) +
  geom_text(aes(label = sprintf("%1.1f", Prop * 100), colour = JobSizeSvyFemale),
            position = position_stack(vjust = .5),
            size = 5,
            show.legend = FALSE) +
  scale_colour_manual(values = c("black","black","black","white","white","white")) +
  xlab("（%）") +
  theme_few() +
  theme(axis.ticks = element_line(colour = "black"),
        axis.text.x = element_text (size = rel(2.0), colour = "black"),
        axis.text.y = element_text (size = rel(2.0), vjust = 0.5, colour = "black"),
        panel.spacing.x = unit(4, "mm"),
        legend.position = "bottom",
        axis.title.x = element_text(size = rel(1.2), hjust = 1.0),
        axis.title.y = element_text(size = rel(1.2)),
        strip.text   = element_text(size = rel(1.0)),
        text         = element_text(size = 10, family="mincho"),
        legend.text  = element_text(size = rel(1.2)),
        plot.title   = element_text(hjust = 0.5, family = "gothic", size = rel(1.6)),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.caption.position = "plot") +
  guides(fill = guide_legend(title = "N of Employees", nrow = 2, reverse = T)) +
  labs(y = NULL, fill = NULL)

ggsave(plot     = PLOT,
       filename = paste0(OutDir, "Desc/Distribution_of_IVs/JobSizeSvy.png"),
       device   = "png",
       width    = 10,
       height   = 3,
       dpi      = 600)

rm(DF_graph)


### 1-5) by group: JobYearsSvy ----

DF_graph <- DF_use |>  
  group_by(Decomp3Grp, JobYearsSvyFemale6Grp) |>  
  summarize(Freq = n()) |>  
  group_by(Decomp3Grp) |>  
  mutate(Prop = Freq / sum(Freq)) |>  
  ungroup()


PLOT <- 
  DF_graph |>  
  ggplot(aes(x     = Prop*100,
             y     = Decomp3Grp,
             group = fct_rev(JobYearsSvyFemale6Grp))) + 
  geom_col(aes(fill  = fct_rev(JobYearsSvyFemale6Grp),
               color = fct_rev(JobYearsSvyFemale6Grp)),
           color = "black") +
  scale_fill_manual(values = c("gray20", "gray40", "gray65", "gray80", "gray90", "white")) +
  geom_text(aes(label = sprintf("%1.1f", Prop * 100), colour = JobYearsSvyFemale6Grp),
            position = position_stack(vjust = .5),
            size = 5,
            show.legend = FALSE) +
  scale_colour_manual(values = c("black","black","black","white","white","white")) +
  xlab("（%）") +
  theme_few() +
  theme(axis.ticks = element_line(colour = "black"),
        axis.text.x = element_text (size = rel(2.0), colour = "black"),
        axis.text.y = element_text (size = rel(2.0), vjust = 0.5, colour = "black"),
        panel.spacing.x = unit(4, "mm"),
        legend.position = "bottom",
        axis.title.x = element_text(size = rel(1.2), hjust = 1.0),
        axis.title.y = element_text(size = rel(1.2)),
        strip.text   = element_text(size = rel(1.0)),
        text         = element_text(size = 10, family="mincho"),
        legend.text  = element_text(size = rel(1.2)),
        plot.title   = element_text(hjust = 0.5, family = "gothic", size = rel(1.6)),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.caption.position = "plot") +
  guides(fill = guide_legend(title = "Years in Current Job", nrow = 2, reverse = T)) +
  labs(y = NULL, fill = NULL)

ggsave(plot     = PLOT,
       filename = paste0(OutDir, "Desc/Distribution_of_IVs/JobYearsSvy.png"),
       device   = "png",
       width    = 10,
       height   = 3,
       dpi      = 600)

rm(DF_graph)
