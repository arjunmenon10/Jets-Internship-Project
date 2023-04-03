# defense rotate graph
pbp_offense |> 
  filter(pff_RUNPASS %in%  c('P', 'R'), pff_NOPLAY == 0) |> 
  mutate(rotate = ifelse(pff_MOFOCPLAYED == pff_MOFOCSHOWN, 'N', 'Y')) |> 
  group_by(pff_DEFTEAM, rotate) |> 
  summarise(
    EPAp = mean(EPA, na.rm = T),
    successr = mean(success, na.rm = T),
    YPP = mean(pff_GAINLOSS, na.rm = T),
    plays = n()
  ) |> 
  left_join(teams_colors_logos, by = c('pff_DEFTEAM' = 'team_abbr')) |> 
  filter(!is.na(rotate)) |> 
  mutate(rotate = ifelse(rotate == 'Y', 'Yes', 'No')) |> 
  ggplot(aes(x = rotate, y = EPAp, group = pff_DEFTEAM))+
  geom_bar(aes(fill = team_color), position = 'dodge', stat = 'identity', width = 0.75)+
  geom_text(aes(y = EPAp/2, label = paste(plays, "snaps")), color = 'white',
            position = position_dodge(0.75), size = 8)+
  geom_image(aes(y = ifelse(EPAp > 0, -0.07, 0.07), image = team_logo_espn), 
             asp = 16/9, position = position_dodge(0.75), size = 0.08)+
  scale_fill_identity()+
  theme_fivethirtyeight()+
  labs(title = "Defensive efficiency when rotating and not rotating safeties post-snap",
       caption = "",
       subtitle = glue::glue("The Cowboys defense was very successful when they rotated their safeties after the ball was snapped"))+
  theme(axis.title = element_text(size = 18)) + ylab('EPA/play allowed') + xlab("Post-snap safety rotation?")+
  theme(panel.grid.minor=element_blank())+
  theme(axis.text = element_text(size = 17))+
  theme(plot.title = element_markdown(size = 21, hjust = 0.5, face = "bold"),
        plot.subtitle = element_markdown(size = 16, hjust = 0.5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n=8))
ggsave('PHIDALrotate.png', width = 14, height = 10, dpi = "retina")


# rush direction graph
pbp_offense |> 
  filter(pff_RUNPASS %in% c('R'), pff_NOPLAY == 0) |> 
  mutate(
    rundir = case_when(
      pff_POAINTENDED %in% c('LE', 'LT', 'LG') ~ 'Left',
      pff_POAINTENDED %in% c('RE', 'RT', 'RG') ~ 'Right',
      pff_POAINTENDED %in% c('MR', 'ML') ~ 'Middle',
      TRUE ~ 'Other'
    )
  ) |> 
  group_by(pff_OFFTEAM, rundir) |> 
  summarise(
    EPAp = mean(EPA, na.rm = T),
    successr = mean(success, na.rm = T),
    YPP = mean(pff_GAINLOSS, na.rm = T),
    plays = n()
  ) |> 
  filter(rundir != 'Other') |> 
  left_join(teams_colors_logos, by = c('pff_OFFTEAM' = 'team_abbr')) |>
  ggplot(aes(x = rundir, y = successr, group = pff_OFFTEAM))+
  geom_bar(aes(fill = team_color), position = 'dodge', stat = 'identity', width = 0.75)+
  geom_text(aes(y = successr/2, label = paste(plays, "attempts")), color = 'white',
            position = position_dodge(0.75), size = 6.5)+
  geom_image(aes(y = successr, image = team_logo_espn), 
             asp = 16/9, position = position_dodge(0.75), size = 0.08)+
  scale_fill_identity()+
  theme_fivethirtyeight()+
  labs(title = "Rushing success rates for each offense broken down by run direction",
       caption = "",
       subtitle = glue::glue("Both offenses had the most success on average running to the right side of their offensive line"))+
  theme(axis.title = element_text(size = 18)) + ylab('Success Rate') + xlab("Run Direction")+
  theme(panel.grid.minor=element_blank())+
  theme(axis.text = element_text(size = 17))+
  theme(plot.title = element_markdown(size = 21, hjust = 0.5, face = "bold"),
        plot.subtitle = element_markdown(size = 16, hjust = 0.5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n=8), labels = scales::percent_format())
ggsave('PHIDALrushdir.png', width = 14, height = 10, dpi = "retina")



  