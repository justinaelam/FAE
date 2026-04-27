# Computing HEI score function
library(tidyverse)

score_adequacy <- function(intake, kcal, standard, max_pts) {
  density <- intake / (kcal / 1000)
  if (density == 0) return(0)
  min(max_pts * (density / standard), max_pts)
}

score_moderation_density <- function(intake, kcal, min_std, max_std, max_pts = 10) {
  density <- intake / (kcal / 1000)
  if (density <= min_std) return(max_pts)
  if (density >= max_std) return(0)
  max_pts * (1 - (density - min_std) / (max_std - min_std))
}

score_sodium <- function(sodium_mg, kcal, max_pts = 10) {
  ratio <- sodium_mg / kcal
  if (ratio <= 1.1) return(max_pts)
  if (ratio >= 2.0) return(0)
  max_pts * (1 - (ratio - 1.1) / (2.0 - 1.1))
}

score_addsug <- function(add_sugars_tsp, kcal, max_pts = 10) {
  pct <- 100 * (add_sugars_tsp * 16) / kcal
  if (pct <= 6.5) return(max_pts)
  if (pct >= 26) return(0)
  max_pts * (1 - (pct - 6.5) / (26 - 6.5))
}

score_satfat <- function(satfat_g, kcal, max_pts = 10) {
  pct <- 100 * (satfat_g * 9) / kcal
  if (pct <= 8) return(max_pts)
  if (pct >= 16) return(0)
  max_pts * (1 - (pct - 8) / (16 - 8))
}

score_fattyacid <- function(monopoly_g, satfat_g, max_pts = 10) {
  if (satfat_g == 0 && monopoly_g == 0) return(0)
  if (satfat_g == 0 && monopoly_g > 0) return(max_pts)
  ratio <- monopoly_g / satfat_g
  if (ratio >= 2.5) return(max_pts)
  if (ratio <= 1.2) return(0)
  max_pts * (ratio - 1.2) / (2.5 - 1.2)
}
compute_hei <- function(food_list, fped, fndds) {

  diet <- food_list %>%
    left_join(fped, by = "FOODCODE") %>%
    left_join(fndds, by = "FOODCODE") %>%
    mutate(scale = grams / 100) %>%
    mutate(across(
      c(F_TOTAL, F_CITMLB, F_OTHER,
        V_TOTAL, V_DRKGR, V_LEGUMES,
        G_WHOLE, G_REFINED, D_TOTAL,
        PF_MPS_TOTAL, PF_EGGS, PF_NUTSDS, PF_SOY, PF_LEGUMES,
        PF_SEAFD_HI, PF_SEAFD_LOW,
        ADD_SUGARS,
        KCAL, SODIUM, SATFAT, MUFA, PUFA),
      ~ . * scale
    ))

  totals <- diet %>%
    summarise(
      KCAL = sum(KCAL, na.rm = TRUE),
      F_TOTAL = sum(F_TOTAL, na.rm = TRUE),
      FWHOLEFRT = sum(F_CITMLB + F_OTHER, na.rm = TRUE),
      VTOTALLEG = sum(V_TOTAL + V_LEGUMES, na.rm = TRUE),
      VDRKGRLEG = sum(V_DRKGR + V_LEGUMES, na.rm = TRUE),
      G_WHOLE = sum(G_WHOLE, na.rm = TRUE),
      G_REFINED = sum(G_REFINED, na.rm = TRUE),
      D_TOTAL = sum(D_TOTAL, na.rm = TRUE),
      PFALLPROTLEG = sum(PF_MPS_TOTAL + PF_EGGS + PF_NUTSDS + PF_SOY + PF_LEGUMES * 4, na.rm = TRUE),
      PFSEAPLANTLEG = sum(PF_SEAFD_HI + PF_SEAFD_LOW + PF_NUTSDS + PF_SOY + PF_LEGUMES * 4, na.rm = TRUE),
      ADD_SUGARS = sum(ADD_SUGARS, na.rm = TRUE),
      SODIUM = sum(SODIUM, na.rm = TRUE),
      SATFAT = sum(SATFAT, na.rm = TRUE),
      MONOPOLY = sum(MUFA + PUFA, na.rm = TRUE)
    )

  kcal <- totals$KCAL
  if (kcal <= 0) stop("Invalid kcal")

  scores <- c(
    score_adequacy(totals$F_TOTAL,        kcal, 0.8,  5),
    score_adequacy(totals$FWHOLEFRT,      kcal, 0.4,  5),
    score_adequacy(totals$VTOTALLEG,      kcal, 1.1,  5),
    score_adequacy(totals$VDRKGRLEG,      kcal, 0.2,  5),
    score_adequacy(totals$G_WHOLE,        kcal, 1.5, 10),
    score_adequacy(totals$D_TOTAL,        kcal, 1.3, 10),
    score_adequacy(totals$PFALLPROTLEG,   kcal, 2.5,  5),
    score_adequacy(totals$PFSEAPLANTLEG,  kcal, 0.8,  5),
    score_fattyacid(totals$MONOPOLY,      totals$SATFAT),
    score_moderation_density(totals$G_REFINED, kcal, 1.8, 4.3),
    score_sodium(totals$SODIUM, kcal),
    score_addsug(totals$ADD_SUGARS, kcal),
    score_satfat(totals$SATFAT, kcal)
  )

  return(sum(scores))
}

# read in datasets
library(readxl)
FNDDS_nutrients <- read_excel("dataraw/FNDDS_nutrients.xlsx",
                              skip = 1)


FPED_1718 <- read_excel("dataraw/FPED_1718.xls")
library(tibble)

# example list of foods
food_list <- tibble(
  FOODCODE = c(
    63101000, 61100600, 75113080, 75117010,
    26137110, 23101000, 11100000, 57123000,
    41102020, 82104000, 91101010
  ),
  grams = c(
    182, 120, 85, 60,
    150, 100, 244, 45,
    130, 14, 10
  )
)

hei_score_test <- compute_hei(food_list, fped_raw, fndds)
hei_score_test
