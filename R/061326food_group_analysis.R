# 060726
# GOAL: For each branded_food product, identify which FPED food groups
# are present based on ingredient keywords --> map those to FPED FOODCODEs --> sum the FPED equivalent
# values per product --> combine with per-100g nutrient values to compute HEI scores

# ---------- Food Group Classification + FPED Nutrient Lookup -------


library(tidyverse)
library(readxl)


# -------- LOAD DATA ----------


# Qian's merge_nutrient df, (branded_food joined with nutrition)
df_nutrient <- read_csv("dataraw/merge_nutrient.csv")

# FPED food patterns equivalents —-> maps FOODCODE to food group servings
fped <- read_xls("dataraw/FPED_1720.xls")


# --- SAMPLE 50 BRANDED FOOD PRODUCTS (non-missing ingredients) ---

set.seed(67)
df <- df_nutrient %>%
  filter(!is.na(ingredients)) %>%
  slice_sample(n = 50)

# -------- Nutrients are per-serving --> normalize to per-100g (taken from Youcef's code) -------
df <- df %>%
  mutate(
    serving_size_g = case_when(
      serving_size_unit == "ml" ~ serving_size,   # assume ml ≈ g (water density)
      serving_size > 0          ~ serving_size,
      TRUE                      ~ NA_real_
    ),
    scale = 100 / serving_size_g,

    # Key nutrients per 100g
    calories_100g       = Energy                      * scale,
    protein_100g        = Protein                     * scale,
    total_fat_100g      = `Total lipid (fat)`         * scale,
    saturated_fat_100g  = `Fatty acids, total saturated` * scale,
    sodium_100g         = `Sodium, Na`                * scale,
    fiber_100g          = `Fiber, total dietary`      * scale,
    sugar_100g          = `Sugars, Total`             * scale,
    added_sugar_100g    = `Sugars, added`             * scale
  ) # %>%
  # # Remove extreme outliers (likely data entry errors)
  # filter(
  #   is.na(sodium_100g)    | sodium_100g    < 10000,   # >10g Na/100g is impossible
  #   is.na(calories_100g)  | calories_100g  < 2000     # >2000 kcal/100g is impossible
  # )

# x <- df2 %>%
#   filter(
#     sodium_100g   >= 10000 |
#       calories_100g >= 2000
#   ) %>%
#   select(fdc_id, ingredients, serving_size, serving_size_g, sodium_100g, calories_100g)



#  ----- KEYWORD-BASED FOOD GROUP CLASSIFICATION (from ingredients column) ---

# Helper: case-insensitive keyword search across multiple terms
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
  # Convert logical to integer (0/1) to match R convention
  mutate(across(starts_with("is_"), as.integer))



# ----- SUMMARIZE THE FOOD GROUP ----

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



# 5. INGREDIENT KEYWORD → FPED DESCRIPTION LOOKUP TABLE

# Each row: an ingredient keyword, the food group it belongs to, and the
# regex pattern to match against FPED DESCRIPTION. The best-matching FPED
# FOODCODE is selected per ingredient keyword (first/most specific match).

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

  # --- NUT SEED ---
  "is_nutseed", "almond",     "^almonds|almond, dry roasted",
  "is_nutseed", "flax",   "^flax seeds$",
  "is_nutseed", "chia",  "^chia seeds$",
  "is_nutseed", "hemp",  "^hemp seeds$",

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


#   --- oil ---
  "is_oil", "olive oil",      "^oil, olive|olive oil",
  "is_oil", "vegetable oil",  "^oil, vegetable, NFS|vegetable oil",
  "is_oil", "canola oil",     "^oil, canola|canola oil",

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



# ---- MATCH EACH INGREDIENT TO ITS BEST-FIT FPED FOODCODE --------
# For each keyword, find the first FPED row whose DESCRIPTION matches the
# pattern (case-insensitive) --> might be limitations with this approach.

fped_lookup <- ingredient_fped_map %>%
  rowwise() %>%
  mutate(
    matched = list(
      fped %>%
        filter(str_detect(str_to_lower(DESCRIPTION), str_to_lower(fped_desc_pattern))) %>%
        slice(1) %>%                            # take the single best match
        select(FOODCODE, DESCRIPTION)
    )
  ) %>%
  ungroup() %>%
  unnest(matched, names_sep = "_") %>%          # matched_FOODCODE, matched_DESCRIPTION
  rename(FOODCODE = matched_FOODCODE,
         fped_description = matched_DESCRIPTION) %>%
  filter(!is.na(FOODCODE)) %>%                  # drop keywords with no FPED match
  left_join(fped, by = "FOODCODE")              # attach all 37 FPED nutrient cols

cat("\nIngredient → FOODCODE lookup (sample):\n")
fped_lookup %>%
  select(food_group, ingredient_keyword, FOODCODE, fped_description) %>%
  print(n = 10)



#  ----- MATCH BRANDED PRODUCTS TO FPED FOODCODEs VIA INGREDIENTS ----
# For each branded product, tokenise its ingredients and check which keywords
# from fped_lookup appear.
# Each match yields one row with:
#   fdc_id | ingredient_keyword | food_group | FOODCODE | all FPED nutrients

match_ingredients <- function(fdc_id_val, ingredients_val, lookup) {
  keywords_found <- lookup %>%
    filter(str_detect(str_to_lower(ingredients_val),
                      str_to_lower(ingredient_keyword))) %>%
    mutate(fdc_id = fdc_id_val)
  keywords_found
}

df_matched <- df %>%
  select(fdc_id, gtin_upc, ingredients) %>%
  pmap_dfr(function(fdc_id, gtin_upc, ingredients) {
    match_ingredients(fdc_id, ingredients, fped_lookup) %>%
      mutate(gtin_upc = gtin_upc)
  })

# Keep all FPED nutrient columns in the final output
nutrient_cols <- setdiff(names(fped), c("FOODCODE", "DESCRIPTION"))

df_with_fped <- df_matched %>%
  select(fdc_id, gtin_upc, food_group, ingredient_keyword,
         FOODCODE, fped_description, all_of(nutrient_cols))

# --- DIAGNOSTICS: check match coverage before collapsing ---
cat("\nUnique products matched:", df_with_fped %>% distinct(fdc_id) %>% nrow(), "of", nrow(df), "\n")

df_with_fped %>%
  count(fdc_id, name = "n_matches") %>%
  arrange(desc(n_matches)) %>%
  print(n = 10)

unmatched <- df %>%
  filter(!fdc_id %in% df_with_fped$fdc_id) %>%
  select(fdc_id, gtin_upc, ingredients)

cat("\nUnmatched products (", nrow(unmatched), "):\n")
print(unmatched)





# --- COLLAPSE TO ONE ROW PER PRODUCT ----
# sum FPED nutrient equivalents across all matched ingredient FOODCODE pairs

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

# ---------------------------------------
cat("\n\nBranded products matched to FPED FOODCODEs (first 20 rows):\n")
df_with_fped %>%
  select(fdc_id, food_group, ingredient_keyword, FOODCODE, fped_description) %>%
  head(20) %>%
  print()

cat("\n\nCount of FPED matches per branded product:\n")
df_with_fped %>%
  count(fdc_id, name = "n_fped_matches") %>%
  arrange(desc(n_fped_matches)) %>%
  print(n = 43)

df_with_fped %>% distinct(fdc_id) %>% nrow()



# ---------------------------------- RESULTS -------------------------------

# A) Classified branded products (50 rows, binary food group flags)
df %>%
  select(fdc_id, gtin_upc, ingredients, all_of(food_group_cols))

# B) Food group summary (count + %)
food_group_summary

# C) Ingredient --> FOODCODE lookup table
ing <- fped_lookup %>%
            select(food_group, ingredient_keyword, FOODCODE, fped_description)

# D) Full match of all the matched products
df_with_fped



# 061326
# goal: using df_with_fped, calculate the hei score

# build hei input

# df_wide gives FPED food-group servings (cup/oz/tsp/g equivalents).
# It does NOT include KCAL/SODIUM/SATFAT/MUFA/PUFA -- those come from
# the branded product's own nutrition facts (df), which are more
# accurate for a specific product than anything derived from matched
# ingredient keywords.
#
# MUFA+PUFA are approximated as (total_fat - saturated_fat), since
# branded nutrition facts don't break out fatty acid types. This
# slightly overestimates mono/poly fat (it also captures trans fat),
# but is the best available proxy without a separate FNDDS join.

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


# filter out rows with missing/invalid nutrient data

# Traces back to missing serving_size_g in step 1 -- if calories_100g
# (or fat values) couldn't be computed, no HEI component can be scored
# for that product.

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



# HEI helper functions copied from HEI_SCORE

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

# Scoring combination of products
# HEI's density formula (intake / (kcal/1000)) is calibrated for a
# whole day's diet (~2000 kcal), not a single 100g product. Scoring
# one product alone causes most components to hit 0 or their max
# almost immediately. To get meaningful HEI scores, combine multiple
# products + gram amounts into one "diet" and score that combination.


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
      kcal          = sum(calories_100g, na.rm = TRUE),
      F_TOTAL       = sum(F_TOTAL, na.rm = TRUE),
      FWHOLEFRT     = sum(FWHOLEFRT, na.rm = TRUE),
      VTOTALLEG     = sum(VTOTALLEG, na.rm = TRUE),
      VDRKGRLEG     = sum(VDRKGRLEG, na.rm = TRUE),
      G_WHOLE       = sum(G_WHOLE, na.rm = TRUE),
      G_REFINED     = sum(G_REFINED, na.rm = TRUE),
      D_TOTAL       = sum(D_TOTAL, na.rm = TRUE),
      PFALLPROTLEG  = sum(PFALLPROTLEG, na.rm = TRUE),
      PFSEAPLANTLEG = sum(PFSEAPLANTLEG, na.rm = TRUE),
      ADD_SUGARS    = sum(ADD_SUGARS, na.rm = TRUE),
      SODIUM        = sum(sodium_100g, na.rm = TRUE),
      SATFAT        = sum(saturated_fat_100g, na.rm = TRUE),
      MONOPOLY      = sum(monopoly_g, na.rm = TRUE)
    )

  kcal <- totals$kcal
  if (kcal <= 0) stop("Invalid total kcal for selected products")

  scores <- c(
    fruit       = score_adequacy(totals$F_TOTAL,       kcal, 0.8, 5),
    wholefruit  = score_adequacy(totals$FWHOLEFRT,     kcal, 0.4, 5),
    veg         = score_adequacy(totals$VTOTALLEG,     kcal, 1.1, 5),
    greensbean  = score_adequacy(totals$VDRKGRLEG,     kcal, 0.2, 5),
    wholegrain  = score_adequacy(totals$G_WHOLE,       kcal, 1.5, 10),
    dairy       = score_adequacy(totals$D_TOTAL,       kcal, 1.3, 10),
    protein     = score_adequacy(totals$PFALLPROTLEG,  kcal, 2.5, 5),
    seaplant    = score_adequacy(totals$PFSEAPLANTLEG, kcal, 0.8, 5),
    fattyacid   = score_fattyacid(totals$MONOPOLY, totals$SATFAT),
    refinedgr   = score_moderation_density(totals$G_REFINED, kcal, 1.8, 4.3),
    sodium      = score_sodium(totals$SODIUM, kcal),
    addsug      = score_addsug(totals$ADD_SUGARS, kcal),
    satfat      = score_satfat(totals$SATFAT, kcal)
  )

  list(
    total_score = sum(scores),
    component_scores = scores,
    totals = totals
  )
}

df_wide_clean <- df_wide %>%
  rename_with(~ str_replace_all(., " \\(cup eq\\)| \\(oz eq\\)| \\(tsp eq\\)", ""))

hei_input <- df_wide_clean %>%
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

hei_results <- hei_input %>%
  filter(!is.na(calories_100g), calories_100g > 0,
         !is.na(saturated_fat_100g)) %>%
  rowwise() %>%
  mutate(
    kcal = calories_100g,
    score_fruit       = if (is.na(kcal) || kcal <= 0) NA_real_ else score_adequacy(F_TOTAL, kcal, 0.8, 5),
    score_wholefruit  = if (is.na(kcal) || kcal <= 0) NA_real_ else score_adequacy(FWHOLEFRT, kcal, 0.4, 5),
    score_veg         = if (is.na(kcal) || kcal <= 0) NA_real_ else score_adequacy(VTOTALLEG, kcal, 1.1, 5),
    score_greensbean  = if (is.na(kcal) || kcal <= 0) NA_real_ else score_adequacy(VDRKGRLEG, kcal, 0.2, 5),
    score_wholegrain  = if (is.na(kcal) || kcal <= 0) NA_real_ else score_adequacy(G_WHOLE, kcal, 1.5, 10),
    score_dairy       = if (is.na(kcal) || kcal <= 0) NA_real_ else score_adequacy(D_TOTAL, kcal, 1.3, 10),
    score_protein     = if (is.na(kcal) || kcal <= 0) NA_real_ else score_adequacy(PFALLPROTLEG, kcal, 2.5, 5),
    score_seaplant    = if (is.na(kcal) || kcal <= 0) NA_real_ else score_adequacy(PFSEAPLANTLEG, kcal, 0.8, 5),
    score_fattyacid   = score_fattyacid(monopoly_g, saturated_fat_100g),
    score_refinedgr   = if (is.na(kcal) || kcal <= 0) NA_real_ else score_moderation_density(G_REFINED, kcal, 1.8, 4.3),
    score_sodium      = if (is.na(kcal) || kcal <= 0) NA_real_ else score_sodium(sodium_100g, kcal),
    score_addsug      = if (is.na(kcal) || kcal <= 0) NA_real_ else score_addsug(ADD_SUGARS, kcal),
    score_satfat      = if (is.na(kcal) || kcal <= 0) NA_real_ else score_satfat(saturated_fat_100g, kcal),
    hei_total = sum(c_across(starts_with("score_")), na.rm = TRUE)
  ) %>%
  ungroup()

hei_results %>%
  select(fdc_id, gtin_upc, food_groups_matched, n_matches, hei_total, starts_with("score_"))


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

  return(list(
    total_score = sum(scores),
    component_scores = scores,
    totals = totals
  ))
}

# recall that food_group_summary is the breakdown of each component,
# want to use this as our constraint for choosing product for a basket
food_group_summary

# group all protein together
food_group_summary2 <- food_group_summary %>%
  mutate(group2 = case_when(
    group %in% c("is_meat", "is_seafood", "is_legume", "is_nutseed") ~ "protein",
    TRUE ~ group
  )) %>%
  group_by(group2) %>%
  summarise(
    n = sum(n),
    pct = sum(pct)
  ) %>%
  arrange(desc(n))

basket_items <- df %>%
  sample_n(10)

basket_fped <- df_with_fped %>%
  filter(fdc_id %in% basket_items$fdc_id)

simulate_baskets <- function(df, hei_input, n = 500, basket_size = 10) {

  replicate(n, {

    basket <- df %>% sample_n(basket_size)

    result <- compute_hei_products(
      fdc_ids = basket$fdc_id,
      grams   = rep(100, basket_size),  # or real grams if you have them
      hei_input = hei_input
    )

    result$total_score

  })
}

set.seed(13)
hei_dist <- simulate_baskets(df, hei_input, n = 1000)

hist(hei_dist,
     breaks = 30,
     main = "Distribution of Simulated HEI Scores",
     xlab = "HEI Score",
     col = "gray")

# --- Example usage ---
compute_hei_products(
  fdc_ids = c(953945, 1479204, 2734680),
  grams   = c(85, 60, 150), # this has score 70
  hei_input = hei_input_valid
)
