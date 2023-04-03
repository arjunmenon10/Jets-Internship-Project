pbp_offense$pff_FIRSTDOWNGAINED[is.na(pbp_offense$pff_FIRSTDOWNGAINED)] <- 0

# # team pass summary table
pbp_offense |> 
  filter(pff_RUNPASS %in% c('P'), pff_NOPLAY == 0) |> 
  group_by(pff_OFFTEAM, pff_RUNPASS) |> 
  summarise(
    EPAp = mean(EPA[qb_scramble == 0], na.rm = T),
    successr = mean(success[qb_scramble == 0], na.rm = T),
    yards = sum(pff_GAINLOSS[qb_scramble == 0 & sacked == 0], na.rm = T),
    plays = n(),
    yardsperplay = yards/plays
  )

# completion percentage
pbp_offense |> 
  filter(pff_RUNPASS == 'P' & pff_PASSRESULT %in% c('COMPLETE', 'INTERCEPTION',
                                                    'INCOMPLETE', 'THROWN AWAY', 'BATTED PASS',
                                                    'SPIKE'),
         pff_NOPLAY == 0) |>
  mutate(complete = ifelse(pff_PASSRESULT == 'COMPLETE', 1, 0)) |> 
  group_by(pff_OFFTEAM) |> 
  summarise(
    compperc = mean(complete)
  )

# # team run summary table
pbp_offense |> 
  filter(pff_RUNPASS %in% c('R') | qb_scramble == 1, pff_NOPLAY == 0) |> 
  group_by(pff_OFFTEAM) |> 
  summarise(
    EPAp = mean(EPA, na.rm = T),
    successr = mean(success, na.rm = T),
    yards = sum(pff_GAINLOSS, na.rm = T),
    plays = n(),
    yardsperplay = yards/plays,
    firstdownperc = mean(pff_FIRSTDOWNGAINED, na.rm = T)
  )

# team defense stats
defense |> 
  filter(pff_PRESSURE == 'Y') |> 
  group_by(pff_TEAM) |> 
  count() 

pbp_offense |> 
  filter(sacked == '1') |> 
  group_by(pff_DEFTEAM) |> 
  count()

# defensive blitz rate
pbp_offense |> 
  mutate(blitz = ifelse(Rushers == '5+', 1, 0)) |> 
  filter(pff_RUNPASS == 'P') |> 
  group_by(pff_DEFTEAM) |> 
  summarise(blitzr = mean(blitz, na.rm = T))


# # offensive run types
pbp_offense |> 
  mutate(rungroup = case_when(
    PFFRunType %in% c('INSIDE ZONE READ', 'OUTSIDE ZONE READ') ~ 'Zone Reads',
    PFFRunType %in% c('INSIDE ZONE', 'OUTSIDE ZONE') ~ 'Zone Runs',
    PFFRunType %in% c('POWER', 'COUNTER', 'COUNTER READ', 'CROSS LEAD',
                      'PIN PULL') ~ 'Gap Pulling Runs',
    PFFRunType %in% c('DUO', 'ISO', 'DRAW') ~ 'Gap runs',
    TRUE ~ 'Other'
  )) |> 
  filter(pff_RUNPASS == 'R', pff_NOPLAY == 0) |> 
  group_by(pff_OFFTEAM, rungroup) |> 
  summarise(
    EPAp = mean(EPA, na.rm = T),
    successr = mean(success, na.rm = T),
    YPC = mean(pff_GAINLOSS, na.rm = T),
    plays = n()
  )

# # RPO Table
pbp_offense |> 
  filter(pff_NOPLAY == 0) |> 
  group_by(pff_OFFTEAM, pff_RUNPASSOPTION) |> 
  summarise(
    EPAp = mean(EPA, na.rm = T),
    successr = mean(success, na.rm = T),
    YPP = mean(pff_GAINLOSS, na.rm = T),
    plays = n()
  )

# # pre-snap 2 high looks for defense
pbp_offense |> 
  filter(pff_RUNPASS %in%  c('P', 'R'), pff_NOPLAY == 0) |> 
  group_by(pff_DEFTEAM, pff_MOFOCSHOWN) |> 
  summarise(
    EPAp = mean(EPA, na.rm = T),
    successr = mean(success, na.rm = T),
    YPP = mean(pff_GAINLOSS, na.rm = T),
    plays = n()
  )

# # safety shell rotation for defense
pbp_offense |> 
  filter(pff_RUNPASS %in%  c('P', 'R'), pff_NOPLAY == 0) |> 
  mutate(rotate = ifelse(pff_MOFOCPLAYED == pff_MOFOCSHOWN, 'N', 'Y')) |> 
  group_by(pff_DEFTEAM, rotate) |> 
  summarise(
    EPAp = mean(EPA, na.rm = T),
    successr = mean(success, na.rm = T),
    YPP = mean(pff_GAINLOSS, na.rm = T),
    plays = n()
  )

# defense safety rotations broken down
pbp_offense |> 
  filter(pff_RUNPASS %in%  c('P', 'R'), pff_NOPLAY == 0) |> 
  group_by(pff_DEFTEAM, pff_MOFOCSHOWN, pff_MOFOCPLAYED) |> 
  summarise(
    EPAp = mean(EPA, na.rm = T),
    successr = mean(success, na.rm = T),
    YPP = mean(pff_GAINLOSS, na.rm = T),
    plays = n()
  )

# # offensive formation stats
pbp_offense |> 
  filter(!is.na(QBPos), pff_NOPLAY == 0) |> 
  group_by(pff_OFFTEAM, QBPos) |> 
  summarise(
    EPAp = mean(EPA, na.rm = T),
    successr = mean(success, na.rm = T),
    YPP = mean(pff_GAINLOSS, na.rm = T),
    plays = n()
  )

# # defensive man zone stats
pbp_offense |> 
  filter(pff_RUNPASS == 'P', pff_NOPLAY == 0) |> 
  group_by(pff_DEFTEAM, ManZone) |> 
  summarise(
    EPAp = mean(EPA, na.rm = T),
    successr = mean(success, na.rm = T),
    YPP = mean(pff_GAINLOSS, na.rm = T),
    plays = n()
  )

# # defensive coverage type stats
pbp_offense |> 
  filter(pff_RUNPASS == 'P', pff_NOPLAY == 0) |> 
  group_by(pff_DEFTEAM, CovNumber) |> 
  summarise(
    EPAp = mean(EPA, na.rm = T),
    successr = mean(success, na.rm = T),
    YPP = mean(pff_GAINLOSS, na.rm = T),
    plays = n()
  )

# # defensive personnel stats
pbp_offense |> 
  filter(pff_RUNPASS %in% c('P', 'R'), pff_NOPLAY == 0) |> 
  group_by(pff_DEFTEAM, DefPers) |> 
  summarise(
    EPAp = sum(EPA, na.rm = T),
    successr = mean(success, na.rm = T),
    YPP = mean(pff_GAINLOSS, na.rm = T),
    plays = n()
  )

# blitzing stats
pbp_offense |> 
  mutate(blitz = ifelse(Rushers == '5+', 1, 0)) |> 
  filter(pff_RUNPASS %in% c('P'), pff_NOPLAY == 0) |> 
  group_by(pff_DEFTEAM, blitz) |> 
  summarise(
    EPAp = mean(EPA, na.rm = T),
    successr = mean(success, na.rm = T),
    YPP = mean(pff_GAINLOSS, na.rm = T),
    plays = n()
  )

# offensive target route stats
pbp_offense |> 
  filter(pff_RUNPASS %in% c('P'), pff_NOPLAY == 0) |> 
  group_by(pff_OFFTEAM, pff_PASSROUTENAME) |> 
  summarise(
    EPAp = mean(EPA, na.rm = T),
    successr = mean(success, na.rm = T),
    YPP = mean(pff_GAINLOSS, na.rm = T),
    plays = n()
  ) |> view()

# simulated pressure looks
pbp_offense |> 
  filter(Rushers == '4 Sim') |> 
  group_by(pff_DEFTEAM) |> 
  summarise(
    EPAp = mean(EPA, na.rm = T),
    successr = mean(success, na.rm = T),
    YPP = mean(pff_GAINLOSS, na.rm = T),
    plays = n()
  )
  
# motion stats
pbp_offense |> 
  filter(pff_RUNPASS %in% c('P', 'R'), pff_NOPLAY == 0) |> 
  group_by(pff_OFFTEAM, pff_MOTION) |> 
  summarise(
    EPAp = mean(EPA, na.rm = T),
    successr = mean(success, na.rm = T),
    YPP = mean(pff_GAINLOSS, na.rm = T),
    plays = n()
  )

# red zone motion stats
pbp_offense |> 
  filter(pff_RUNPASS %in% c('P', 'R'), pff_NOPLAY == 0, AbsoluteYardLine <= 20) |> 
  group_by(pff_OFFTEAM, pff_MOTION) |> 
  summarise(
    EPAp = mean(EPA, na.rm = T),
    successr = mean(success, na.rm = T),
    YPP = mean(pff_GAINLOSS, na.rm = T),
    plays = n()
  )
  
# offensive formation stats
pbp_offense |> 
  filter(pff_RUNPASS %in% c('P', 'R'), pff_NOPLAY == 0) |> 
  group_by(pff_OFFTEAM, AsFormation) |> 
  summarise(
    EPAp = mean(EPA, na.rm = T),
    successr = mean(success, na.rm = T),
    YPP = mean(pff_GAINLOSS, na.rm = T),
    plays = n()
  )
