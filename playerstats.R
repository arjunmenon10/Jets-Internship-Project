# RB stats
offense |> 
  mutate(snap = 1) |> 
  filter(PosGroup == 'RB') |> 
  group_by(PlayerName) |> 
  summarise(
    rushyards = sum(pff_RUSHINGYARDS, na.rm = T),
    rushYPC = mean(pff_RUSHINGYARDS, na.rm = T),
    MTF = sum(pff_TACKLESAVOIDED, na.rm = T),
    recyards = sum(pff_RECEIVINGYARDS, na.rm = T),
    receptions = sum(snap[pff_ROLE == 'Pass Route' & pff_BALLCARRIER == 'Y'], na.rm = T),
  ) 

# WR & TE stats
offense |> 
  mutate(snap = 1,
         target = ifelse(is.na(pff_TARGETEDRECEIVER), 0, 1)) |> 
  filter(PosGroup %in% c('WR', 'TE'), pff_ROLE == 'Pass Route') |> 
  group_by(PlayerName, pff_TEAM) |> 
  summarise(
    snaps = sum(snap),
    recyards = sum(pff_RECEIVINGYARDS, na.rm = T),
    receptions = sum(snap[pff_ROLE == 'Pass Route' & pff_BALLCARRIER == 'Y'], na.rm = T),
    YAC = sum(pff_YARDSAFTERCATCH, na.rm = T),
    target = sum(target, na.rm = T)
  ) |> group_by(pff_TEAM) |> mutate(tottargets = sum(target)) |> 
  group_by(PlayerName) |> mutate(targetshare = target/tottargets) |> 
  filter(target > 0) |> arrange(pff_TEAM, -target)


defense |> 
  mutate(pressure = ifelse(is.na(pff_PRESSURE), 0 , 1),
         sack = ifelse(is.na(pff_SACK), 0, 1),
         snap = 1) |> 
  filter(pff_ROLE == 'Pass Rush') |> 
  group_by(PlayerName, pff_TEAM) |> 
  summarise(
    snaps = sum(snap, na.rm = T),
    pressures = sum(pressure, na.rm = T),
    sacks = sum(sack, na.rm = T),
  ) |> filter(pressures > 0) |> 
  arrange(pff_TEAM, -pressures)

defense |> 
  group_by(PlayerName) |> 
  summarise(
    EPARm = sum(EPAR, na.rm = T),
    PFFVARm = sum(PFFVAR, na.rm = T)
  ) |> 
  arrange(-abs(EPARm))


DALtbl <- offense |> 
  group_by(PlayerName, pff_TEAM) |> 
  summarise(
    EPARm = sum(EPAR, na.rm = T),
    PFFVARm = sum(PFFVAR, na.rm = T)
  ) |> 
  arrange(-abs(EPARm)) |> 
  filter(pff_TEAM == 'DAL') |> 
  head(5) |> 
  ungroup() |> 
  left_join(teams_colors_logos, by = c('pff_TEAM' = 'team_abbr')) |> 
  select(PlayerName, team_logo_espn, EPARm, PFFVARm) |> 
  gt() |> 
  gt_img_rows(team_logo_espn) |> 
  fmt_number(c(EPARm, PFFVARm), decimals = 2) |> 
  cols_label(
    PlayerName = 'Player',
    team_logo_espn = '',
    EPARm = "Total EPA over Replacement",
    PFFVARm = "Total PFF Grade over Replacement"
    ) |> 
  opt_row_striping() %>%
  data_color(
    columns = c(EPARm),
    colors = scales::col_numeric(
      palette = c("#ff9d9a", "lightgreen", "darkgreen"),
      domain = NULL
    )
  ) %>%
  gt_theme_538() |> 
  tab_header(title = md("Most and least valuable players on the Cowboys offense based on EPA over replacement"),
             subtitle = "Dak Prescott was the most valuable player on the Cowboys offense") 
gtsave(DALtbl, "DALotbl.png")

  
PHItbl <- offense |> 
  group_by(PlayerName, pff_TEAM) |> 
  summarise(
    EPARm = sum(EPAR, na.rm = T),
    PFFVARm = sum(PFFVAR, na.rm = T)
  ) |> 
  arrange(-abs(EPARm)) |> 
  filter(pff_TEAM == 'PHI') |> 
  head(5) |> 
  ungroup() |> 
  left_join(teams_colors_logos, by = c('pff_TEAM' = 'team_abbr')) |> 
  select(PlayerName, team_logo_espn, EPARm, PFFVARm) |> 
  gt() |> 
  gt_img_rows(team_logo_espn) |> 
  fmt_number(c(EPARm, PFFVARm), decimals = 2) |> 
  cols_label(
    PlayerName = 'Player',
    team_logo_espn = '',
    EPARm = "Total EPA over Replacement",
    PFFVARm = "Total PFF Grade over Replacement"
  ) |> 
  opt_row_striping() %>%
  data_color(
    columns = c(EPARm),
    colors = scales::col_numeric(
      palette = c("#ff9d9a", "lightgreen", "darkgreen"),
      domain = NULL
    )
  ) %>%
  gt_theme_538() |> 
  tab_header(title = md("Most and least valuable players on the Eagles offense based on EPA over replacement"),
             subtitle = "Devonta Smith was the most valuable player on the Eagles offense") 
gtsave(PHItbl, "PHIotbl.png")

img1 <- magick::image_read("DALotbl.png")
img2 <- magick::image_read("PHIotbl.png")

img3 <- magick::image_append(c(img1, img2), stack = TRUE)

image_write(img3, path = "FinalOtbl", format = "png")


DALdtbl <- defense |> 
  group_by(PlayerName, pff_TEAM) |> 
  summarise(
    EPARm = sum(EPAR, na.rm = T),
    PFFVARm = sum(PFFVAR, na.rm = T)
  ) |> 
  arrange(-abs(EPARm)) |> 
  filter(pff_TEAM == 'DAL') |> 
  head(5) |> 
  ungroup() |> 
  left_join(teams_colors_logos, by = c('pff_TEAM' = 'team_abbr')) |> 
  select(PlayerName, team_logo_espn, EPARm, PFFVARm) |> 
  gt() |> 
  gt_img_rows(team_logo_espn) |> 
  fmt_number(c(EPARm, PFFVARm), decimals = 2) |> 
  cols_label(
    PlayerName = 'Player',
    team_logo_espn = '',
    EPARm = "Total EPA over Replacement",
    PFFVARm = "Total PFF Grade over Replacement"
  ) |> 
  opt_row_striping() %>%
  data_color(
    columns = c(EPARm),
    colors = scales::col_numeric(
      palette = c("#ff9d9a", "lightgreen", "darkgreen"),
      domain = NULL
    )
  ) %>%
  gt_theme_538() |> 
  tab_header(title = md("Most and least valuable players on the Cowboys defense based on EPA over replacement"),
             subtitle = "Carlos Watkins was the most valuable player on the Cowboys defense while Trevon Diggs was the least") 
gtsave(DALdtbl, "DALdtbl.png")


PHIdtbl <- defense |> 
  group_by(PlayerName, pff_TEAM) |> 
  summarise(
    EPARm = sum(EPAR, na.rm = T),
    PFFVARm = sum(PFFVAR, na.rm = T)
  ) |> 
  arrange(-abs(EPARm)) |> 
  filter(pff_TEAM == 'PHI') |> 
  head(5) |> 
  ungroup() |> 
  left_join(teams_colors_logos, by = c('pff_TEAM' = 'team_abbr')) |> 
  select(PlayerName, team_logo_espn, EPARm, PFFVARm) |> 
  gt() |> 
  gt_img_rows(team_logo_espn) |> 
  fmt_number(c(EPARm, PFFVARm), decimals = 2) |> 
  cols_label(
    PlayerName = 'Player',
    team_logo_espn = '',
    EPARm = "Total EPA over Replacement",
    PFFVARm = "Total PFF Grade over Replacement"
  ) |> 
  opt_row_striping() %>%
  data_color(
    columns = c(EPARm),
    colors = scales::col_numeric(
      palette = c("#ff9d9a", "lightgreen", "darkgreen"),
      domain = NULL
    )
  ) %>%
  gt_theme_538() |> 
  tab_header(title = md("Most and least valuable players on the Eagles defense based on EPA over replacement"),
             subtitle = "Josh Sweat was the most valuable player on the Eagles defense while Marcus Epps was the least") 
gtsave(PHIdtbl, "PHIdtbl.png")

img1 <- magick::image_read("DALdtbl.png")
img2 <- magick::image_read("PHIdtbl.png")

img3 <- magick::image_append(c(img1, img2), stack = TRUE)

image_write(img3, path = "FinalDtbl", format = "png")
