# =============================================================================
# HEI-2020 Scoring Pipeline for Branded Food Products
# Last updated: 6/29/2026
#
# Goal: Compute HEI-2020 scores for branded food baskets using USDA FoodData
#       Central and FPED data. Simulates baskets both globally and per store.
#
# Inputs:
#   dataraw/nutrient.csv            - USDA nutrient definitions
#   dataraw/branded_food.csv        - branded food metadata (ingredients, etc.)
#   dataraw/food_nutrient.csv       - per-product nutrient amounts
#   dataraw/FPED_1720.xls           - FPED 2017-2020 food composition data
#   baskets_by_store/*.csv          - Nielsen scanner data, one file per store
#
# Outputs:
#   basket_sims         - tibble of N_BASKETS_GLOBAL simulated baskets (global pool)
#   all_store_sims      - tibble of N_BASKETS_PER_STORE baskets per store
#   store_hei_summary   - per-store mean/median/sd HEI summary
# =============================================================================


# ── Libraries ─────────────────────────────────────────────────────────────────

library(readr)
library(readxl)
library(tidyverse)
library(dplyr)
library(purrr)
library(ggplot2)
library(patchwork)


# ── Constants — edit as needed ──────────────────────────────────────────

STORE_FOLDER        <- "/Users/juslam/Library/CloudStorage/GoogleDrive-justinalam@umass.edu/Shared drives/Nielsen Scanner/processed_data/baskets_by_store"
TARGET_KCAL         <- 2000   # daily calorie threshold for kcal normalization
N_BASKETS_GLOBAL    <- 1000   # simulated baskets from the global product pool
N_BASKETS_PER_STORE <- 50     # baskets per store (20-50 recommended; 5 is too noisy)
GRAMS_PER_SLOT      <- 100    # grams assigned to each basket slot


# =============================================================================
# 1. LOAD DATA
# =============================================================================

nutrient      <- read_csv("dataraw/nutrient.csv")
branded_food  <- read_csv("dataraw/branded_food.csv")
food_nutrient <- read_csv("dataraw/food_nutrient.csv")
fped          <- read_xls("dataraw/FPED_1720.xls")

# Load store files first — food_id is derived from data_list
files     <- list.files(STORE_FOLDER, pattern = "\\.csv$", full.names = TRUE)
data_list <- map(files, read_csv)

cat("Loaded", length(data_list), "store files\n")


# looks like df 73  94  97 102 113 147 165 200 210 260 are missing fdc_id
bad <- which(!map_lgl(data_list, ~ "fdc_id" %in% names(.x)))
bad

good_data <- keep(data_list, ~ "fdc_id" %in% names(.x))


# Sanity check: confirm channel composition, only has "Food"
channel_counts <- good_data %>%
  map_dfr(~ count(.x, channel_name), .id = "store_id") %>%
  group_by(channel_name) %>%
  summarise(total = sum(n), .groups = "drop") %>%
  arrange(desc(total))
print(channel_counts)


# =============================================================================
# 2. DERIVE PRODUCT LIST FROM STORE DATA
# =============================================================================

# Pull all unique fdc_ids seen across all stores — replaces top50_by_category
food_id <- map(good_data, ~ .x %>% filter(!is.na(fdc_id)) %>% pull(fdc_id)) %>%
  unlist() %>%
  unique()

cat("Unique fdc_ids across all stores:", length(food_id), "\n")


# =============================================================================
# 3. MERGE NUTRIENT DATA, copied from Qian's code
# =============================================================================

food_nutrient_joined <- food_nutrient |>
  filter(fdc_id %in% food_id) |>
  select(fdc_id, nutrient_id, amount) |>
  left_join(nutrient, by = join_by(nutrient_id == id)) |>
  select(-unit_name, -nutrient_nbr, -rank, -nutrient_id) |>
  group_by(name, fdc_id) |>
  summarize(amount = mean(amount), .groups = "drop") |>
  pivot_wider(names_from = name, values_from = amount)


# =============================================================================
# 4. BUILD MAIN PRODUCT DATA FRAME
# =============================================================================

# Derived from branded_food filtered to store fdc_ids — no top50_by_category needed
df <- branded_food %>%
  filter(fdc_id %in% food_id) %>%
  select(fdc_id, gtin_upc, serving_size, serving_size_unit,
         branded_food_category, ingredients) %>%
  left_join(food_nutrient_joined, by = "fdc_id")

cat("Products in df:", nrow(df), "\n")


# =============================================================================
# 5. NORMALIZE NUTRIENTS TO PER-100g (from Youcef's code)
# =============================================================================

df <- df %>%
  mutate(
    serving_size_g = case_when(
      serving_size_unit == "ml" ~ serving_size,   # assume ml ≈ g (water density)
      serving_size > 0          ~ serving_size,
      TRUE                      ~ NA_real_
    ),
    scale = 100 / serving_size_g,

    calories_100g      = Energy                           * scale,
    protein_100g       = Protein                          * scale,
    total_fat_100g     = `Total lipid (fat)`              * scale,
    saturated_fat_100g = `Fatty acids, total saturated`   * scale,
    sodium_100g        = `Sodium, Na`                     * scale,
    fiber_100g         = `Fiber, total dietary`           * scale,
    sugar_100g         = `Sugars, Total`                  * scale,
    added_sugar_100g   = `Sugars, added`                  * scale
  )


# =============================================================================
# 6. KEYWORD-BASED FOOD GROUP CLASSIFICATION (from ingredients column)
# =============================================================================

contains_any <- function(text, keywords) {
  pattern <- paste(keywords, collapse = "|")
  str_detect(str_to_lower(text), str_to_lower(pattern))
}

df <- df %>%
  mutate(
    is_dairy = contains_any(ingredients, c(
      "milk", "cream", "cheese", "yogurt", "yoghurt",
      "whey", "lactose", "butter", "buttermilk", "casein",
      "nonfat milk", "skim milk", "whole milk"
    )),
    is_fruit = contains_any(ingredients, c(
      "apple", "orange", "grape", "lemon", "lime", "mango",
      "peach", "berry", "blueberry", "strawberry", "raspberry",
      "cherry", "pineapple", "banana", "fruit juice", "fruit puree",
      "juice concentrate", "cranberry", "apricot", "plum", "pear"
    )),
    is_vegetable = contains_any(ingredients, c(
      "tomato", "spinach", "carrot", "broccoli", "pepper", "onion",
      "garlic", "celery", "corn", "pea", "beet", "zucchini",
      "cucumber", "lettuce", "kale", "cabbage", "potato",
      "sweet potato", "tomatillo", "avocado", "cactus"
    )),
    is_nutseed = contains_any(ingredients, c(
      "almond", "walnut", "pecan", "cashew", "pistachio",
      "hazelnut", "macadamia", "brazil nut", "pine nut",
      "flax", "chia", "hemp seed", "sunflower seed", "pumpkin seed",
      "sesame seed"
    )),
    is_wholegrain = contains_any(ingredients, c(
      "whole wheat", "whole grain", "oats", "oat bran", "oat flour",
      "brown rice", "quinoa", "millet", "buckwheat", "amaranth",
      "barley", "rye flour", "whole rye", "bran", "spelt"
    )),
    is_refinedgrain = contains_any(ingredients, c(
      "enriched flour", "enriched wheat flour", "wheat flour",
      "bleached flour", "unbleached flour", "white flour",
      "corn starch", "rice flour", "semolina", "pasta",
      "macaroni", "noodle", "corn meal", "corn grits",
      "modified food starch", "modified corn starch"
    )),
    is_legume = contains_any(ingredients, c(
      "soy", "soybean", "tofu", "tempeh", "edamame",
      "lentil", "chickpea", "black bean", "kidney bean",
      "navy bean", "pinto bean", "peanut", "pea protein",
      "soy protein", "soy flour", "soy lecithin"
    )),
    is_seafood = contains_any(ingredients, c(
      "salmon", "tuna", "cod", "grouper", "tilapia", "halibut",
      "sardine", "anchovy", "shrimp", "crab", "lobster",
      "scallop", "clam", "oyster", "fish", "seafood",
      "seaweed", "nori", "crabmeat"
    )),
    is_added_sugar = contains_any(ingredients, c(
      "sugar", "corn syrup", "high fructose corn syrup",
      "honey", "dextrose", "fructose", "maltose",
      "cane sugar", "invert sugar", "brown sugar",
      "maple syrup", "molasses", "tapioca syrup",
      "rice syrup", "agave", "glucose syrup"
    )),
    is_oil = contains_any(ingredients, c(
      "olive oil", "vegetable oil", "canola oil", "sunflower oil",
      "corn oil", "soybean oil", "safflower oil", "grapeseed oil",
      "avocado oil", "sesame oil", "peanut oil", "coconut oil"
    )),
    is_meat = contains_any(ingredients, c(
      "beef", "pork", "chicken", "turkey", "lamb",
      "veal", "venison", "bison", "duck", "goose",
      "meat", "poultry", "prosciutto", "bacon",
      "sausage", "salami", "pepperoni", "ham"
    ))
  ) %>%
  mutate(across(starts_with("is_"), as.integer))


# =============================================================================
# 7. FOOD GROUP SUMMARY
# =============================================================================

food_group_cols <- c(
  "is_dairy", "is_fruit", "is_vegetable", "is_nutseed", "is_wholegrain",
  "is_refinedgrain", "is_legume", "is_seafood",
  "is_added_sugar", "is_oil", "is_meat"
)

food_group_summary <- df %>%
  summarise(across(all_of(food_group_cols), ~ sum(., na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "group", values_to = "n") %>%
  mutate(pct = round(n / nrow(df) * 100, 1)) %>%
  arrange(desc(n))

print(food_group_summary)


# =============================================================================
# 8. INGREDIENT KEYWORD → FPED FOODCODE LOOKUP TABLE
# =============================================================================

ingredient_fped_map <- tribble(
  ~food_group,      ~ingredient_keyword,       ~fped_desc_pattern,

  # --- DAIRY ---
  "is_dairy",       "whole milk",              "milk, whole$",
  "is_dairy",       "reduced fat milk",        "milk, reduced fat",
  "is_dairy",       "low fat milk",            "milk, low fat",
  "is_dairy",       "skim milk",               "milk, fat free|milk, skim",
  "is_dairy",       "nonfat milk",             "milk, fat free",
  "is_dairy",       "buttermilk",              "^buttermilk",
  "is_dairy",       "evaporated milk",         "milk, evaporated",
  "is_dairy",       "cream cheese",            "cream cheese$",
  "is_dairy",       "cheddar cheese",          "cheddar cheese$",
  "is_dairy",       "mozzarella",              "mozzarella cheese$",
  "is_dairy",       "parmesan",                "parmesan cheese$",
  "is_dairy",       "yogurt",                  "yogurt, NFS|yogurt, plain",
  "is_dairy",       "sour cream",              "sour cream$",
  "is_dairy",       "heavy cream",             "cream, heavy",
  "is_dairy",       "whey",                    "^whey$",
  "is_dairy",       "light cream",             "^cream, light|half and half",
  "is_dairy",       "pasteurized milk",        "^milk, NFS|^milk, whole",

  # --- FRUIT ---
  "is_fruit",       "apple juice",             "apple juice, 100%$",
  "is_fruit",       "apple",                   "^apples$|^apple, raw",
  "is_fruit",       "orange juice",            "orange juice, 100%$",
  "is_fruit",       "mango",                   "^mango, raw|^mango$",
  "is_fruit",       "mango puree",             "mango, NS|mango, raw",
  "is_fruit",       "peach",                   "^peach, raw|^peach$",
  "is_fruit",       "strawberry",              "^strawberries|strawberry, raw",
  "is_fruit",       "blueberry",               "^blueberries|blueberry, raw",
  "is_fruit",       "raspberry",               "^raspberries|raspberry, raw",
  "is_fruit",       "grape",                   "^grapes$|grape juice",
  "is_fruit",       "lemon juice",             "lemon juice$",
  "is_fruit",       "lime juice",              "lime juice$",
  "is_fruit",       "cranberry",               "^cranberries|cranberry juice",
  "is_fruit",       "pineapple",               "^pineapple, raw|pineapple juice",
  "is_fruit",       "banana",                  "^banana, raw|^banana$",

  # --- VEGETABLE ---
  "is_vegetable",   "tomato",                  "^tomato, raw|^tomatoes, raw",
  "is_vegetable",   "tomato paste",            "tomato paste$",
  "is_vegetable",   "tomato puree",            "tomato puree$",
  "is_vegetable",   "crushed tomatoes",        "tomatoes, crushed|tomato sauce",
  "is_vegetable",   "spinach",                 "^spinach, raw|^spinach, cooked",
  "is_vegetable",   "carrot",                  "^carrots, raw|^carrot$",
  "is_vegetable",   "broccoli",                "^broccoli, raw|^broccoli, cooked",
  "is_vegetable",   "green pepper",            "^peppers, green, raw",
  "is_vegetable",   "red pepper",              "^peppers, red, raw",
  "is_vegetable",   "jalapeno",                "^peppers, jalapeno",
  "is_vegetable",   "onion",                   "^onions, raw|^onion, raw",
  "is_vegetable",   "garlic",                  "^garlic, raw|^garlic$",
  "is_vegetable",   "celery",                  "^celery, raw|^celery$",
  "is_vegetable",   "corn",                    "^corn, yellow, cooked|^corn, canned",
  "is_vegetable",   "potato",                  "^potatoes, boiled|^potato, NS",
  "is_vegetable",   "avocado",                 "^avocado, raw|^avocado$",
  "is_vegetable",   "cucumber",                "^cucumber, raw|^cucumber$",
  "is_vegetable",   "beet",                    "^beets, cooked|^beets, raw",
  "is_vegetable",   "olives",                  "^olives|olive, ripe",
  "is_vegetable",   "green beans",             "^green beans, cooked|^snap beans",

  # --- NUT/SEED ---
  "is_nutseed",     "almond",                  "^almonds|almond, dry roasted",
  "is_nutseed",     "flax",                    "^flax seeds$",
  "is_nutseed",     "chia",                    "^chia seeds$",
  "is_nutseed",     "hemp",                    "^hemp seeds$",

  # --- WHOLE GRAIN ---
  "is_wholegrain",  "oats",                    "^oatmeal, NS|^oat cereal",
  "is_wholegrain",  "oat bran",                "oat bran$",
  "is_wholegrain",  "whole wheat flour",       "whole wheat bread|whole wheat flour",
  "is_wholegrain",  "whole grain",             "whole grain cereal|whole grain bread",
  "is_wholegrain",  "brown rice",              "^brown rice, cooked|^brown rice$",
  "is_wholegrain",  "quinoa",                  "^quinoa, cooked|^quinoa$",
  "is_wholegrain",  "millet",                  "^millet, cooked|^millet$",
  "is_wholegrain",  "buckwheat",               "^buckwheat|buckwheat groats",
  "is_wholegrain",  "amaranth",                "^amaranth",
  "is_wholegrain",  "rye flour",               "^rye bread|rye flour",

  # --- REFINED GRAIN ---
  "is_refinedgrain","enriched wheat flour",    "white bread|enriched wheat flour",
  "is_refinedgrain","wheat flour",             "wheat flour fritter|white bread",
  "is_refinedgrain","white bread",             "^white bread$",
  "is_refinedgrain","pasta",                   "^pasta, cooked|^pasta$",
  "is_refinedgrain","macaroni",                "^macaroni, cooked|^macaroni$",
  "is_refinedgrain","corn starch",             "^corn starch|cornstarch",
  "is_refinedgrain","rice flour",              "^rice flour|rice, white, cooked",
  "is_refinedgrain","corn grits",              "^corn grits|grits, cooked",
  "is_refinedgrain","jasmine rice",            "^rice, white, cooked|^rice, NS",
  "is_refinedgrain","semolina",                "^pasta, cooked|macaroni, cooked",

  # --- LEGUME ---
  "is_legume",      "soybean",                 "^soybeans, cooked|^soybean$",
  "is_legume",      "soy milk",                "^soy milk$",
  "is_legume",      "soy protein",             "soy protein|soybean protein",
  "is_legume",      "soy lecithin",            "soy milk|^soy milk",
  "is_legume",      "pea protein",             "^peas, cooked|pea protein",
  "is_legume",      "black bean",              "^black beans, NFS|black beans, cooked",
  "is_legume",      "kidney bean",             "^kidney beans|kidney beans, cooked",
  "is_legume",      "chickpea",                "^chickpeas|garbanzo beans",
  "is_legume",      "lentil",                  "^lentils, cooked|^lentils$",
  "is_legume",      "peanut",                  "^peanuts, dry roasted|^peanuts, raw",

  # --- SEAFOOD ---
  "is_seafood",     "salmon",                  "^salmon, cooked|salmon, NS as to cooking",
  "is_seafood",     "tuna",                    "^tuna, NS as to cooking|^tuna, canned",
  "is_seafood",     "cod",                     "^cod, cooked|cod, NS as to cooking",
  "is_seafood",     "grouper",                 "^grouper|grouper, cooked",
  "is_seafood",     "tilapia",                 "^tilapia|tilapia, cooked",
  "is_seafood",     "shrimp",                  "^shrimp, cooked|shrimp, NS",
  "is_seafood",     "crab",                    "^crab, cooked|^crabmeat",
  "is_seafood",     "scallop",                 "^scallops|scallop, cooked",
  "is_seafood",     "halibut",                 "^halibut|halibut, cooked",
  "is_seafood",     "sardine",                 "^sardines",

  # --- ADDED SUGAR ---
  "is_added_sugar", "sugar",                   "^sugar$|^sugar, white$|^table sugar",
  "is_added_sugar", "brown sugar",             "^sugar, brown$",
  "is_added_sugar", "high fructose corn syrup","^corn syrup|high fructose corn syrup",
  "is_added_sugar", "corn syrup",              "^corn syrup$",
  "is_added_sugar", "honey",                   "^honey$",
  "is_added_sugar", "maple syrup",             "^maple syrup$",
  "is_added_sugar", "cane sugar",              "^sugar, white, granulated or lump$",
  "is_added_sugar", "invert sugar",            "^sugar$",
  "is_added_sugar", "glucose syrup",           "^glucose$|corn syrup",
  "is_added_sugar", "tapioca syrup",           "^tapioca|corn syrup",

  # --- OIL ---
  "is_oil",         "olive oil",               "^oil, olive|olive oil",
  "is_oil",         "vegetable oil",           "^oil, vegetable, NFS|vegetable oil",
  "is_oil",         "canola oil",              "^oil, canola|canola oil",

  # --- MEAT ---
  "is_meat",        "beef",                    "^beef, NS as to cut, cooked",
  "is_meat",        "pork",                    "^pork, NS as to cut, cooked",
  "is_meat",        "chicken",                 "^chicken breast, NS as to cooking",
  "is_meat",        "chicken breast",          "^chicken breast, NS as to cooking",
  "is_meat",        "turkey",                  "^turkey, nfs$",
  "is_meat",        "lamb",                    "^lamb, NS as to cut, cooked",
  "is_meat",        "bacon",                   "^bacon, NS|^bacon, cooked",
  "is_meat",        "ham",                     "^ham, NS|^ham, sliced",
  "is_meat",        "sausage",                 "^sausage, NS|^pork sausage",
  "is_meat",        "salami",                  "^salami",
  "is_meat",        "pepperoni",               "^pepperoni"
)


# =============================================================================
# 9. MATCH EACH INGREDIENT KEYWORD TO BEST-FIT FPED FOODCODE
# =============================================================================

fped_lookup <- ingredient_fped_map %>%
  rowwise() %>%
  mutate(
    matched = list(
      fped %>%
        filter(str_detect(str_to_lower(DESCRIPTION), str_to_lower(fped_desc_pattern))) %>%
        slice(1) %>%
        select(FOODCODE, DESCRIPTION)
    )
  ) %>%
  ungroup() %>%
  unnest(matched, names_sep = "_") %>%
  rename(FOODCODE = matched_FOODCODE, fped_description = matched_DESCRIPTION) %>%
  filter(!is.na(FOODCODE)) %>%
  left_join(fped, by = "FOODCODE")

cat("\nIngredient → FOODCODE lookup (sample):\n")
fped_lookup %>%
  select(food_group, ingredient_keyword, FOODCODE, fped_description) %>%
  print(n = 10)


# =============================================================================
# 10. MATCH BRANDED PRODUCTS TO FPED FOODCODEs VIA INGREDIENTS
# =============================================================================

match_ingredients <- function(fdc_id_val, ingredients_val, lookup) {
  lookup %>%
    filter(str_detect(str_to_lower(ingredients_val),
                      str_to_lower(ingredient_keyword))) %>%
    mutate(fdc_id = fdc_id_val)
}

df_matched <- df %>%
  select(fdc_id, gtin_upc, ingredients) %>%
  pmap_dfr(function(fdc_id, gtin_upc, ingredients) {
    match_ingredients(fdc_id, ingredients, fped_lookup) %>%
      mutate(gtin_upc = gtin_upc)
  })

nutrient_cols <- setdiff(names(fped), c("FOODCODE", "DESCRIPTION"))

df_with_fped <- df_matched %>%
  select(fdc_id, gtin_upc, food_group, ingredient_keyword,
         FOODCODE, fped_description, all_of(nutrient_cols))

cat("\nUnique products matched:", df_with_fped %>% distinct(fdc_id) %>% nrow(),
    "of", nrow(df), "\n")

df_with_fped %>%
  count(fdc_id, name = "n_matches") %>%
  arrange(desc(n_matches)) %>%
  print(n = 20)

unmatched <- df %>%
  filter(!fdc_id %in% df_with_fped$fdc_id) %>%
  select(fdc_id, gtin_upc, ingredients)

cat("\nUnmatched products (", nrow(unmatched), "):\n")
print(unmatched) # looks like 149 are not matched


# =============================================================================
# 11. COLLAPSE TO ONE ROW PER PRODUCT
# =============================================================================

df_wide <- df_with_fped %>%
  group_by(fdc_id) %>%
  summarise(
    gtin_upc            = first(gtin_upc),
    food_groups_matched = paste(unique(food_group), collapse = ", "),
    foodcodes_matched   = paste(unique(FOODCODE),   collapse = ", "),
    n_matches           = n(),
    across(all_of(nutrient_cols), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

cat("\ndf_wide rows:", nrow(df_wide), "(should equal matched products above)\n")


# =============================================================================
# 12. BUILD HEI INPUT TABLE
# =============================================================================

hei_input <- df_wide %>%
  rename_with(~ str_replace_all(., " \\(cup eq\\)| \\(oz eq\\)| \\(tsp eq\\)| \\(g\\)", "")) %>%
  left_join(
    df %>% select(fdc_id, calories_100g, total_fat_100g, saturated_fat_100g, sodium_100g),
    by = "fdc_id"
  ) %>%
  mutate(
    FWHOLEFRT     = F_CITMLB + F_OTHER,
    VTOTALLEG     = V_TOTAL + V_LEGUMES,
    VDRKGRLEG     = V_DRKGR + V_LEGUMES,
    PFALLPROTLEG  = PF_MPS_TOTAL + PF_EGGS + PF_NUTSDS + PF_SOY + PF_LEGUMES * 4,
    PFSEAPLANTLEG = PF_SEAFD_HI + PF_SEAFD_LOW + PF_NUTSDS + PF_SOY + PF_LEGUMES * 4,
    monopoly_g    = pmax(total_fat_100g - saturated_fat_100g, 0)
  )

n_before <- nrow(hei_input)

hei_input_valid <- hei_input %>%
  filter(
    !is.na(calories_100g), calories_100g > 0,
    !is.na(saturated_fat_100g),
    !is.na(monopoly_g)
  )

n_dropped <- n_before - nrow(hei_input_valid)
cat("\nDropped", n_dropped, "of", n_before,
    "matched products due to missing/invalid serving-size or fat data.\n")

dropped_rows <- hei_input %>%
  filter(
    is.na(calories_100g) | calories_100g <= 0 |
      is.na(saturated_fat_100g) |
      is.na(monopoly_g)
  )
dropped_rows


# =============================================================================
# 13. HEI SCORING HELPER FUNCTIONS
# =============================================================================

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
  if (satfat_g == 0 && monopoly_g > 0)  return(max_pts)
  ratio <- monopoly_g / satfat_g
  if (ratio >= 2.5) return(max_pts)
  if (ratio <= 1.2) return(0)
  max_pts * (ratio - 1.2) / (2.5 - 1.2)
}


# =============================================================================
# 14. SLOT ALLOCATION (proportional from dataset composition)
# =============================================================================

plate_groups <- c("is_protein", "is_vegetable", "is_dairy",
                  "is_refinedgrain", "is_fruit", "is_wholegrain")

slot_allocation <- tibble(
  group2 = c(
    "is_vegetable",
    "is_fruit",
    "is_wholegrain",
    "is_refinedgrain",
    "is_protein",
    "is_dairy"
  ),
  slots = c(3, 2, 2, 1, 2, 2) # sums up to 12
)

# food_group_summary2 <- food_group_summary %>%
#   mutate(group2 = case_when(
#     group %in% c("is_meat", "is_seafood", "is_legume", "is_nutseed") ~ "is_protein",
#     TRUE ~ group
#   )) %>%
#   group_by(group2) %>%
#   summarise(n = sum(n), pct = sum(pct)) %>%
#   arrange(desc(n))
#
# slot_allocation <- food_group_summary2 %>%
#   filter(group2 %in% plate_groups) %>%
#   mutate(
#     prop  = pct / sum(pct),
#     slots = round(prop * 12)
#   ) %>%
#   mutate(slots = {
#     s    <- slots
#     diff <- 12L - sum(s)
#     s[which.max(prop)] <- s[which.max(prop)] + diff
#     s
#   })
#
# print(slot_allocation)


# =============================================================================
# 15. CLASSIFY PRODUCTS
# =============================================================================

# NOTE: wholegrain product pool may be small — document in methods if so.

hei_input_classified <- hei_input %>%
  filter(!is.na(calories_100g), calories_100g > 0, !is.na(saturated_fat_100g)) %>%
  distinct(fdc_id, .keep_all = TRUE) %>%
  mutate(
    primary_group = case_when(
      str_detect(food_groups_matched, "is_meat|is_seafood|is_legume|is_nutseed") ~ "protein",
      str_detect(food_groups_matched, "is_wholegrain|is_refinedgrain")           ~ "grain",
      str_detect(food_groups_matched, "is_vegetable")                            ~ "veg",
      str_detect(food_groups_matched, "is_fruit")                                ~ "fruit",
      str_detect(food_groups_matched, "is_dairy")                                ~ "dairy",
      TRUE                                                                        ~ "other"
    ),
    grain_type = case_when(
      primary_group == "grain" & str_detect(food_groups_matched, "is_wholegrain")   ~ "wholegrain",
      primary_group == "grain" & str_detect(food_groups_matched, "is_refinedgrain") ~ "refined",
      TRUE                                                                            ~ NA_character_
    )
  )

cat("\nProduct pool by group:\n")
hei_input_classified %>%
  count(primary_group, grain_type) %>%
  arrange(primary_group) %>%
  print()


# =============================================================================
# 16. COMPUTE HEI FOR A BASKET
# =============================================================================

compute_hei_products <- function(fdc_ids, grams, hei_input) {

  selected <- tibble(fdc_id = fdc_ids, grams = grams) %>%
    left_join(hei_input, by = "fdc_id") %>%
    mutate(scale = grams / 100) %>%
    mutate(across(
      c(F_TOTAL, FWHOLEFRT, VTOTALLEG, VDRKGRLEG,
        G_WHOLE, G_REFINED, D_TOTAL,
        PFALLPROTLEG, PFSEAPLANTLEG,
        ADD_SUGARS, calories_100g, sodium_100g,
        saturated_fat_100g, monopoly_g),
      ~ . * scale
    ))

  totals <- selected %>%
    summarise(
      kcal          = sum(calories_100g,      na.rm = TRUE),
      F_TOTAL       = sum(F_TOTAL,            na.rm = TRUE),
      FWHOLEFRT     = sum(FWHOLEFRT,          na.rm = TRUE),
      VTOTALLEG     = sum(VTOTALLEG,          na.rm = TRUE),
      VDRKGRLEG     = sum(VDRKGRLEG,          na.rm = TRUE),
      G_WHOLE       = sum(G_WHOLE,            na.rm = TRUE),
      G_REFINED     = sum(G_REFINED,          na.rm = TRUE),
      D_TOTAL       = sum(D_TOTAL,            na.rm = TRUE),
      PFALLPROTLEG  = sum(PFALLPROTLEG,       na.rm = TRUE),
      PFSEAPLANTLEG = sum(PFSEAPLANTLEG,      na.rm = TRUE),
      ADD_SUGARS    = sum(ADD_SUGARS,         na.rm = TRUE),
      SODIUM        = sum(sodium_100g,        na.rm = TRUE),
      SATFAT        = sum(saturated_fat_100g, na.rm = TRUE),
      MONOPOLY      = sum(monopoly_g,         na.rm = TRUE)
    )

  kcal <- totals$kcal
  if (kcal <= 0) stop("Invalid total kcal for selected products")

  scores <- c(
    fruit      = score_adequacy(totals$F_TOTAL,       kcal, 0.8,  5),
    wholefruit = score_adequacy(totals$FWHOLEFRT,     kcal, 0.4,  5),
    veg        = score_adequacy(totals$VTOTALLEG,     kcal, 1.1,  5),
    greensbean = score_adequacy(totals$VDRKGRLEG,     kcal, 0.2,  5),
    wholegrain = score_adequacy(totals$G_WHOLE,       kcal, 1.5, 10),
    dairy      = score_adequacy(totals$D_TOTAL,       kcal, 1.3, 10),
    protein    = score_adequacy(totals$PFALLPROTLEG,  kcal, 2.5,  5),
    seaplant   = score_adequacy(totals$PFSEAPLANTLEG, kcal, 0.8,  5),
    fattyacid  = score_fattyacid(totals$MONOPOLY, totals$SATFAT),
    refinedgr  = score_moderation_density(totals$G_REFINED, kcal, 1.8, 4.3),
    sodium     = score_sodium(totals$SODIUM, kcal),
    addsug     = score_addsug(totals$ADD_SUGARS, kcal),
    satfat     = score_satfat(totals$SATFAT, kcal)
  )

  list(total_score = sum(scores), component_scores = scores, totals = totals)
}


# =============================================================================
# 17. BASKET BUILDER
# =============================================================================

build_proportional_basket <- function(classified_input,
                                      slot_allocation,
                                      grams_per_slot = GRAMS_PER_SLOT) {

  pool_for <- function(grp, data) {
    switch(grp,
           is_protein      = filter(data, primary_group == "protein"),
           is_vegetable    = filter(data, primary_group == "veg"),
           is_dairy        = filter(data, primary_group == "dairy"),
           is_refinedgrain = filter(data, grain_type    == "refined"),
           is_fruit        = filter(data, primary_group == "fruit"),
           is_wholegrain   = filter(data, grain_type    == "wholegrain"),
           stop(paste("Unknown group:", grp))
    )
  }

  safe_sample <- function(pool, n, label) {
    if (nrow(pool) == 0) stop(paste("Empty pool:", label))
    if (nrow(pool) < n) {
      warning(paste(label, "pool size", nrow(pool), "< slots", n,
                    "— sampling with replacement"))
      sample_n(pool, n, replace = TRUE)
    } else {
      sample_n(pool, n)
    }
  }

  basket <- map_dfr(seq_len(nrow(slot_allocation)), function(i) {
    grp  <- slot_allocation$group2[i]
    n    <- slot_allocation$slots[i]
    pool <- pool_for(grp, classified_input)
    safe_sample(pool, n, grp) %>%
      mutate(myplate_group = grp, grams = grams_per_slot)
  })

  result <- compute_hei_products(
    fdc_ids   = basket$fdc_id,
    grams     = basket$grams,
    hei_input = classified_input
  )

  result$basket     <- select(basket, fdc_id, myplate_group, grams, calories_100g)
  result$total_kcal <- result$totals$kcal
  result
}


# =============================================================================
# 18. SIMULATE GLOBAL BASKETS (with kcal normalization)
# =============================================================================
# NOTE: HEI scoring is already density-based (every component divides by kcal),
# so the HEI *score* is the same regardless of basket kcal.
# kcal_scale and days_in_basket are for downstream pricing normalization only:
#   cost_per_day = basket_cost * kcal_scale

simulate_proportional_baskets <- function(classified_input,
                                          slot_allocation,
                                          n              = N_BASKETS_GLOBAL,
                                          grams_per_slot = GRAMS_PER_SLOT,
                                          target_kcal    = TARGET_KCAL) {
  raw <- replicate(n, {
    tryCatch(
      build_proportional_basket(classified_input, slot_allocation, grams_per_slot),
      error = function(e) NULL
    )
  }, simplify = FALSE)

  successes <- Filter(Negate(is.null), raw)
  if (length(successes) < n)
    message(n - length(successes), " baskets failed and were dropped")

  tibble(
    basket_id      = seq_along(successes),
    hei_total      = map_dbl(successes, "total_score"),
    total_kcal     = map_dbl(successes, "total_kcal"),
    days_in_basket = total_kcal / target_kcal,   # e.g. 4000 kcal basket → 2 days
    kcal_scale     = target_kcal / total_kcal,    # multiply basket cost by this for cost/day
    basket         = map(successes, "basket")
  )
}

set.seed(13)
basket_sims <- simulate_proportional_baskets(hei_input_classified, slot_allocation)

cat("\nGlobal basket HEI summary:\n")
summary(basket_sims$hei_total)
cat("\nGlobal basket kcal summary:\n")
summary(basket_sims$total_kcal)
cat("\nDays per basket summary:\n")
summary(basket_sims$days_in_basket)


# =============================================================================
# 19. HISTOGRAM 1 — Global simulated baskets
# =============================================================================

p1 <- ggplot(basket_sims, aes(x = hei_total)) +
  geom_histogram(binwidth = 2, fill = "#4472C4", color = "white", alpha = 0.85) +
  geom_vline(xintercept = median(basket_sims$hei_total),
             linetype = "dashed", color = "red", linewidth = 0.8) +
  annotate("text",
           x     = median(basket_sims$hei_total) + 1,
           y     = Inf, vjust = 2,
           label = paste0("Median = ", round(median(basket_sims$hei_total), 1)),
           color = "red", size = 3.5) +
  labs(
    title    = "HEI-2020 Score Distribution of Simulated Baskets",
    x = "HEI-2020 Total Score (0–100)",
    y = "Number of Baskets"
  ) +
  theme_minimal(base_size = 13)

p1
# another option is putting the summary statistics
summary(basket_sims$hei_total)


# =============================================================================
# 20. PER-STORE BASKET SIMULATION
# =============================================================================
# For each store:
#   1. Filter hei_input_classified to products that store carries (fdc_id match)
#   2. Run simulate_proportional_baskets on that store-specific pool
#   3. Tag results with store metadata

simulate_store_baskets <- function(store_df,
                                   hei_classified = hei_input_classified,
                                   slot_alloc     = slot_allocation,
                                   n_baskets      = N_BASKETS_PER_STORE,
                                   grams_per_slot = GRAMS_PER_SLOT) {

  store_code <- store_df$store_code_uc[1]

  # Step 1: which of this store's products are in the HEI pipeline?
  store_fdc_ids <- store_df %>%
    filter(!is.na(fdc_id)) %>%
    pull(fdc_id) %>%
    unique()

  store_classified <- hei_classified %>%
    filter(fdc_id %in% store_fdc_ids)

  n_matched <- nrow(store_classified)

  # Need at least as many products as basket slots
  if (n_matched < nrow(slot_alloc)) {
    message("Store ", store_code, ": only ", n_matched,
            " matched products (< ", nrow(slot_alloc), " slots needed) — skipped")
    return(NULL)
  }

  # Step 2: simulate baskets using only this store's product pool
  sims <- simulate_proportional_baskets(
    classified_input = store_classified,
    slot_allocation  = slot_alloc,
    n                = n_baskets,
    grams_per_slot   = grams_per_slot
  )

  # Step 3: tag with store metadata
  sims %>%
    mutate(
      store_code   = store_code,
      channel_name = store_df$channel_name[1],
      n_matched    = n_matched
    )
}


all_store_sims <- map(good_data, simulate_store_baskets, .progress = TRUE) %>%
  compact() %>%
  bind_rows()



# ── Coverage diagnostics ──────────────────────────────────────────────────────
cat("\nStores successfully simulated:",
    n_distinct(all_store_sims$store_code), "of", length(data_list), "\n")
# only 46 out of 337 stores had enough products matched to the HEI-classified dataset to successfully simulate baskets
# all other stores did not meet the slot allocation threshold

cat("\nn_matched per store (products in HEI pipeline):\n")
all_store_sims %>%
  distinct(store_code, n_matched) %>%
  pull(n_matched) %>%
  summary() %>%
  print()
# n-matched is the number of products in a store that were successfully matched to HEI-classified dataset


# ── Per-store summary ─────────────────────────────────────────────────────────
store_hei_summary <- all_store_sims %>%
  group_by(store_code, channel_name, n_matched) %>%
  summarise(
    mean_hei   = mean(hei_total),
    median_hei = median(hei_total),
    sd_hei     = sd(hei_total),      # high SD → increase N_BASKETS_PER_STORE
    mean_kcal  = mean(total_kcal),
    n_baskets  = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_hei))

print(store_hei_summary)

good_basket <- all_store_sims %>%
  filter(hei_total >= 80) # 282 obs

gg <- good_basket %>% #looks weird
  ggplot(aes(x = hei_total, y = total_kcal)) +
  labs(title = "HEI score per basket and total kcal",
       x = "HEI score",
       y = "total Kcal") + geom_smooth(method = "lm", color = "blue", se = FALSE) +
  geom_point() +
  theme_minimal(base_size = 13)
gg

# get the idx of the highest hei score, get basket
idx <- which.max(good_basket$hei_total)
best_basket <- good_basket$basket[[idx]]
best_basket

# hei score and kcal scale scatter plot of just good baskets?

g <- all_store_sims %>%
  ggplot(aes(x = hei_total, y = kcal_scale)) +
  geom_point()
g

# lower kcal count is associated with higher hei score?, looks at 46 out of 337 stores had enough products matched
# "Do stores with higher HEI baskets also have more calories?"
g <- store_hei_summary %>%
  ggplot(aes(x = mean_hei, y = mean_kcal)) +
  labs(title = "HEI score per store and its kcal means",
       x = "HEI mean",
       y = "Kcal mean") + geom_smooth(method = "lm", color = "blue", se = FALSE) +
  geom_point() +
  theme_minimal(base_size = 13)
g

# =============================================================================
# 21. HISTOGRAM 2 — Per-store mean HEI
# =============================================================================

# wonder if a table would be nicer to show...? only 46 obs

store_hei_summary %>%
  ggplot(aes(x = mean_hei)) +
  geom_histogram() +
  labs(title = "Distribution of Mean HEI by valid simulated basket") +
  theme_minimal(base_size = 13)

