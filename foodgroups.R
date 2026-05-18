# initalization of fndds food code and fdc ic branded food
library(readr)
library(tidyverse)
library(readxl)
# food_attribute <- read_csv("dataraw/food_attribute.csv")
branded_food <- read_csv("dataraw/branded_food.csv")
survey_fndds_food <- read_csv("dataraw/survey_fndds_food.csv")


FNDDS_nutrients <- read_excel("dataraw/FNDDS_nutrients2123.xlsx",
                              skip = 1)
FPED_1720 <- read_excel("dataraw/FPED_1720.xls")

FNDDS_nutrients <- FNDDS_nutrients %>%
  rename(FOODCODE = `Food code`,
         DESCRIPTION = `Main food description`,
         KCAL   = `Energy (kcal)`,
         SODIUM = `Sodium (mg)`,
         SATFAT = `Fatty acids, total saturated (g)`,
         MUFA   = `Fatty acids, total monounsaturated (g)`,
         PUFA   = `Fatty acids, total polyunsaturated (g)`)

FPED_1720 <- FPED_1720 %>%
  rename_with(~ gsub(" \\(.*\\)", "", .x))


nutrient <- FNDDS_nutrients %>%
  left_join(FPED_1720 %>% select(-DESCRIPTION), by = "FOODCODE") # avoid description duplicates

# branded_food <- branded_food %>%
#   select(fdc_id,
#          brand_owner,
#          gtin_upc,
#          ingredients,
#          serving_size,
#          serving_size_unit,
#          household_serving_fulltext,
#          branded_food_category
#          )

survey_fndds_food <- survey_fndds_food %>%
  select(fdc_id, food_code)

intersect(nutrient$FOODCODE, survey_fndds_food$food_code) %>% length()
intersect(survey_fndds_food$fdc_id, branded_food$fdc_id) %>% length()

newdf <- nutrient %>%
  left_join(survey_fndds_food, by = c("FOODCODE" = "food_code"))


###### organize groups based on myplate

veggie <- newdf %>%
  filter(V_TOTAL > 0)

fruit <- newdf %>%
  filter(F_TOTAL > 0)


# protein -- fish, poultry, meats, beans, legumes, nuts
# remove dairy
protein <- newdf %>%
  filter(PF_TOTAL > 0)

protein_legume <- newdf %>%
  filter(PF_LEGUMES > 0)

grains <- newdf %>%
  filter(G_TOTAL > 0)


dairy <- newdf %>%
  filter(D_TOTAL > 0)



# --------- 5/17/26 ----------------------------

# organize newdf by its food groups

newdf <- newdf %>%
  mutate(
    is_veg = as.integer(V_TOTAL > 0),
    is_fruit        = as.integer(F_TOTAL > 0),
    is_whole_grain  = as.integer(G_WHOLE > 0),
    is_refined_grain = as.integer(G_REFINED > 0),
    is_protein      = as.integer(PF_TOTAL > 0),
    is_dairy        = as.integer(D_TOTAL > 0),
    is_oil          = as.integer(OILS > 0),
    is_added_sugar  = as.integer(ADD_SUGARS > 0),
    is_solid_fat    = as.integer(SOLID_FATS > 0)
  )

# also merge newdf with branded_foods to get the gtin_upc
newdf <- newdf %>%
  left_join(
    branded_food %>% select(fdc_id, gtin_upc),
    by = "fdc_id"
  )

