library(nflreadr)
library(tidyverse)
library(gt)
library(gtExtras)
library(nflfastR)

pbp <- read_csv("data.plays.csv")

# create EPA and WPA
pbp <- pbp |> 
  mutate(
    pff_DRIVE = case_when(is.na(pff_DRIVE) ~ lag(pff_DRIVE), T ~ pff_DRIVE)
  ) |> 
  mutate(
    EPn1 = lead(EP),
    EPA = EPn1 - EP,
    WPn1 = lead(WP),
    WPA = WPn1 - WP
  ) |> 
  select(EP, EPn1, EPA, PlayDescription, everything())

pbp <- pbp |> 
  mutate(
    nextoff = lead(pff_OFFTEAM),
    diff = ifelse(pff_OFFTEAM == nextoff, 0, 1),
    change = ifelse(diff == 1 & pff_DRIVEENDEVENT %in% c('INTERCEPTION-TD', 'INTERCEPTION',
                                                         'FUMBLE', 'DOWNS'), 1, 0)
  )

pbp <- pbp |> 
  mutate(TDteam = substr(pff_TOUCHDOWN, 1, 3),
         points = case_when(
           TDteam == pff_OFFTEAM ~ 7,
           TDteam != pff_OFFTEAM ~ -7,
           pff_SPECIALTEAMSTYPE == 'FIELD GOAL' ~ 3,
           TRUE ~ 0
         ))

pbp <- pbp |> 
  mutate(EPA = case_when(
    points != 0 ~  points - EP,
    points == 0 & change == 1 ~ -EPn1 - EP,
    TRUE ~ EPA))

# create success rate based on if EPA > 0
pbp <- pbp |> 
  mutate(
    success = ifelse(EPA > 0, 1, 0)
  )

offense <- read_csv("data.off.csv")

offense <- offense |> 
  mutate(rushername = ifelse(pff_BALLCARRIER == 'Y' & pff_ROLE == 'Run', PlayerName, NA),
         receivername = ifelse(pff_TARGETEDRECEIVER == 'Y' & pff_ROLE == 'Pass Route', PlayerName, NA),
         passername = ifelse(pff_PASSER == 'Y' & pff_ROLE == 'Pass', PlayerName, NA))

rushers <- offense |> 
  filter(pff_ROLE == 'Run') |> 
  select(pff_PLAYID, rushername, EPAR, PFFVAR) |> 
  rename(rusherEPAR = 'EPAR',
         rusherPFFVAR = 'PFFVAR')

passers <- offense |> 
  filter(pff_ROLE %in% c('Pass')) |>  
  select(pff_PLAYID, passername, EPAR, PFFVAR) |> 
  rename(passerEPAR = 'EPAR',
         passerPFFVAR = 'PFFVAR')

receivers <- offense |> 
  filter(pff_ROLE %in% c('Pass Route') & pff_TARGETEDRECEIVER == 'Y') |>  
  select(pff_PLAYID, receivername, EPAR, PFFVAR, pff_PASSROUTENAME) |> 
  rename(receiverEPAR = 'EPAR',
         receiverPFFVAR = 'PFFVAR')

# join passers, rushers, receivers for each play
pbp_offense <- pbp |> 
  filter(pff_RUNPASS %in% c('R', 'P')) |> 
  left_join(rushers, by = 'pff_PLAYID') |> 
  left_join(passers, by = 'pff_PLAYID') |> 
  left_join(receivers, by = 'pff_PLAYID')

# create scramble and sack columns
pbp_offense <- pbp_offense |> 
  mutate(qb_scramble = ifelse(grepl("scramble", PlayDescription), 1, 0),
         sacked = ifelse(grepl("sack", PlayDescription), 1, 0)) |> 
  select(qb_scramble, sacked, everything())

pbp_offense |>
  filter(pff_RUNPASS %in% c('P', 'R')) |> 
  group_by(passername) |> 
  summarise(
    rawEPA = mean(EPA, na.rm = T),
    EPARm = mean(passerEPAR, na.rm = T),
    PFFVARm = mean(passerPFFVAR, na.rm = T),
    successrate = mean(success, na.rm = T),
    yards = sum(pff_GAINLOSS, na.rm = T),
    passes = n()
  )

pbp |> 
  filter(pff_RUNPASS %in% c('R')) |> 
  group_by(pff_OFFTEAM) |> 
  summarise(
    EPAp = mean(EPA, na.rm = T),
    successr = mean(success, na.rm = T),
    yards = sum(pff_GAINLOSS, na.rm = T),
    plays = n()
  )

defense <- read_csv("data.def.csv")

coverage <- defense |> 
  filter(pff_ROLE %in% c('Coverage'), pff_PRIMARYCOVERAGE == 'Y') |>  
  select(pff_PLAYID, PlayerName, pff_GAMEPOSITION) |> 
  rename(CovPlayer = 'PlayerName')

pbp_offense <- pbp_offense |> 
  filter(pff_RUNPASS %in% c('R', 'P')) |> 
  left_join(coverage, by = 'pff_PLAYID')


motion <- offense |> 
  filter(!is.na(pff_MOTION)) |> 
  group_by(pff_PLAYID) |> slice(1) |> 
  select(pff_PLAYID, pff_MOTION)

pbp_offense <- left_join(pbp_offense, motion, by = 'pff_PLAYID') |> 
  mutate(pff_MOTION = ifelse(is.na(pff_MOTION), 0, 1))


WPAtbl <- pbp |> 
  arrange(-abs(WPA)) |> 
  select(WPA, pff_QUARTER, Down, YardsToGo, PlayDescription) |> 
  head(6) |> 
  gt() |> 
  fmt_percent(WPA, decimals = 2) |> 
  cols_label(
    WPA = 'Win Probability Added',
    pff_QUARTER = 'Quarter',
    YardsToGo = 'Distance',
    PlayDescription = 'Play'
  ) |> 
  opt_row_striping() %>%
  data_color(
    columns = c(WPA),
    colors = scales::col_numeric(
      palette = c("#e15759", "#ff9d9a", "lightgreen"),
      domain = NULL
    )
  ) %>%
  gt_theme_538() |> 
  tab_header(title = md("Biggest Win Probability swings during Eagles-Cowboys"),
             subtitle = "Win Probability Added from Cowboys perspective") 
gtsave(WPAtbl, "WPAtbl.png")

Turnoverstbl <- pbp_offense |> 
  mutate(TO = ifelse(grepl("INTERCEPTED", PlayDescription) | 
                       grepl("FUMBLES", PlayDescription), 1, 0)) |> 
  filter(TO == 1) |> slice(c(-4, -5)) |> 
  group_by(pff_OFFTEAM) |> 
  summarise(
    turnovers = sum(TO),
    EPATO = sum(EPA),
    WPATO = sum(WPA)
  ) |> mutate(WPATO = ifelse(pff_OFFTEAM == 'PHI', WPATO*-1, WPATO)) |> 
  left_join(teams_colors_logos, by = c('pff_OFFTEAM' = 'team_abbr')) |> 
  select(team_wordmark, turnovers, WPATO, EPATO) |> 
  gt() |> 
  gt_img_rows(team_wordmark) |> 
  fmt_percent(WPATO, decimals = 2) |> 
  fmt_number(EPATO, decimals = 2) |> 
  cols_label(
    team_wordmark = 'Offense',
    EPATO = "EPA from TO's",
    WPATO = "WPA from TO's"  ) |> 
  opt_row_striping() %>%
  data_color(
    columns = c(WPATO),
    colors = scales::col_numeric(
      palette = c("#e15759", "#ff9d9a", "lightgreen"),
      domain = NULL
    )
  ) %>%
  gt_theme_538() |> 
  tab_header(title = md("Win Probability Added (WPA) and Expected Points Added (EPA) from offensive turnovers"),
             subtitle = "The Eagles lost around 50 more Win Probabiilty points than the Cowboys off turnovers alone") 
gtsave(Turnoverstbl, "TOstbl.png")
         