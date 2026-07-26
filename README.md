## Data Setup

Raw data files are not included in this repository due to file size and licensing restrictions.

### Public data (download and place in `dataraw/`)
From [USDA FoodData Central](https://fdc.nal.usda.gov/download-datasets):
- `dataraw/nutrient.csv` — USDA nutrient definitions
- `dataraw/branded_food.csv` — branded food metadata (ingredients, etc.)
- `dataraw/food_nutrient.csv` — per-product nutrient amounts

From the [FPED database](https://www.ars.usda.gov/northeast-area/beltsville-md-bhnrc/beltsville-human-nutrition-research-center/food-surveys-research-group/docs/fped-databases/):
- `dataraw/FPED_1720.xls` — FPED 2017-2020 food composition data

### Private data (not distributed)
- `baskets_by_store/*.csv` — Nielsen scanner data, one file per store

This is proprietary NielsenIQ retail scanner data and cannot be shared publicly.
Access requires a data use agreement with NielsenIQ. Contact the project maintainers
for details on obtaining access.

### Outputs
- `all_store_sims` — tibble of `N_BASKETS_PER_STORE` simulated baskets per store
- `store_hei_summary` — per-store mean/median/sd HEI summary
