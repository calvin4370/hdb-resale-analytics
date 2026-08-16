# Data

Provenance, schema and known caveats for every data file in this project.
Retrieval dates matter here: `data/raw/` is committed deliberately, because
data.gov.sg updates monthly and a later download would silently change every
number the project reports.

## How the files relate

```
data/raw/raw_resale_prices.csv          data.gov.sg, committed snapshot
        │
        │  01_data_cleaning.R
        ▼
data/processed/cleaned_resale_prices.csv ──┐
        │                                  │  02_geocoding.R  (+ 02b validates)
        │                                  ▼
        │                    data/external/hdb_coordinates.csv
        │                                  │
        │  03_feature_engineering.R  ◄─────┤
        │                                  │
        │                    data/external/mrt_lrt_stations.csv
        ▼
data/processed/enriched_resale_prices.csv
        │
        │  04_build_modelling_data.R   (applies the exclusion rules)
        ▼
data/processed/modelling_resale_prices.csv
```

`data/processed/` is **gitignored** — it is rebuilt from the two tracked inputs
above by `Rscript run_all.R 01 03 04`. A clone does not need to re-run the
geocoding, because `hdb_coordinates.csv` is tracked.

## Sources

| File | Source | Retrieved | Tracked |
|---|---|---|---|
| `raw/raw_resale_prices.csv` | [data.gov.sg dataset `d_8b84c4ee58e3cfc0ece0d773c8ca6abc`](https://data.gov.sg/datasets?query=hdb+resale&resultId=d_8b84c4ee58e3cfc0ece0d773c8ca6abc) | 2026-08-17 | yes |
| `external/hdb_coordinates.csv` | OneMap Search API, via `scripts/02_geocoding.R` | 2026-08-17 | yes |
| `external/mrt_lrt_stations.csv` | OneMap Search API + hand-entered opening dates | 2026-08-17 | yes |
| `processed/*.csv` | built by the pipeline | — | no |

**Resale prices.** Official name *"Resale flat prices based on registration date
from Jan-2017 onwards"*, published by HDB. Dataset created 2021-07-28; the
extract used here was last updated by the publisher **2026-08-16** and covers
**2017-01-01 to 2026-08-01**. 238,064 rows.

Refresh with `Rscript scripts/00_download_data.R`, which writes a dated file
rather than replacing the snapshot. That script is deliberately **not** part of
`run_all.R`.

Two publisher caveats worth carrying into any analysis, quoted from the dataset
description: the floor area *"includes any recess area purchased, space adding
item under HDB's upgrading programmes, roof terrace, etc."*, and the
transactions *"exclude resale transactions that may not reflect the full market
price such as resale between relatives and resale of part shares."*

**MRT/LRT stations.** 216 rows. Coordinates and address fields come from OneMap.
`OPENING_DATE` is **not** in OneMap and was entered by hand — see Caveats.

## Schema

### `raw/raw_resale_prices.csv` — 238,064 × 11

As published. Every column is text in the API except `resale_price`.

| Column | Type | Notes |
|---|---|---|
| `month` | chr | Transaction month, `YYYY-MM` |
| `town` | chr | 26 HDB towns |
| `flat_type` | chr | 7 levels, `1 ROOM` … `MULTI-GENERATION` |
| `block` | chr | Block number, may carry a letter (`409A`) |
| `street_name` | chr | 578 distinct |
| `storey_range` | chr | 17 bands, all 3 storeys wide (`10 TO 12`) |
| `floor_area_sqm` | dbl | 31 – 366.7 |
| `flat_model` | chr | 21 levels |
| `lease_commence_date` | dbl | Year the 99-year lease started, 1966 – 2022 |
| `remaining_lease` | chr | `"62 years 01 month"` — see Caveats |
| `resale_price` | dbl | SGD, 140,000 – 1,728,000 |

### `processed/cleaned_resale_prices.csv` — 238,064 × 13

Output of `01_data_cleaning.R`. Drops `month`, `block` and `remaining_lease`
once they have been parsed into the columns below; `block` survives inside
`address`.

| Column | Type | Notes |
|---|---|---|
| `town`, `flat_type`, `street_name`, `storey_range`, `floor_area_sqm`, `flat_model`, `lease_commence_date`, `resale_price` | | unchanged from raw |
| `address` | chr | `block` + `street_name`, e.g. `406 ANG MO KIO AVE 10`. 9,733 distinct. Join key to the geocodes |
| `remaining_lease_numeric` | dbl | Years, months expressed as a fraction. 39.33 – 97.75 |
| `resale_date` | date | First of the transaction month |
| `resale_year` | dbl | 2017 – 2026 |
| `storey_mid` | dbl | Midpoint of `storey_range`. 2 – 50 |

### `processed/enriched_resale_prices.csv` — 238,064 × 17

Output of `03_feature_engineering.R`: cleaned, plus four geospatial columns.

| Column | Type | Notes |
|---|---|---|
| `lat`, `long` | dbl | Block coordinates, WGS84, joined on `address` |
| `distance_to_cbd` | dbl | km to Downtown Core (103.851784, 1.287953), haversine. 0.72 – 19.86 |
| `distance_to_nearest_mrt` | dbl | km to the nearest station **that had opened on the transaction date**. 0.02 – 3.52 |

### `processed/modelling_resale_prices.csv` — 237,735 × 17

Output of `04_build_modelling_data.R`. Same columns as enriched; 329 rows
removed by the exclusion rules below.

The 12 columns the models actually use are listed once in
[`R/config.R`](../R/config.R) as `MODEL_PREDICTORS`. The remaining columns are
carried for the EDA figures (`street_name`), the app bundle (`address`,
`storey_range`, `resale_date`) and the target (`resale_price`).

### `external/hdb_coordinates.csv` — 9,734 × 3

`address`, `lat`, `long`. One row per distinct address. Doubles as the geocoder's
cache: `02_geocoding.R` queries only addresses absent from it.

Contains **one more row than the data has addresses** (9,734 vs 9,733).
`37 MARGARET DR` was geocoded under an earlier snapshot and its transactions have
since been withdrawn by HDB. A superset is harmless — `02b` checks that every
transaction address has a coordinate, not the reverse.

### `external/mrt_lrt_stations.csv` — 216 × 19

| Column | Type | Notes |
|---|---|---|
| `...1` | dbl | Unnamed index from the original export. Not used; readr renames it on load |
| `ALPHANUMERIC_CODE` | chr | `NS15`, `CC30`. Unique |
| `STATION_NAME_ENGLISH` | chr | 184 distinct — interchanges appear once per line |
| `STATION_NAME_CHINESE` | chr | Blank for the three CCL6 stations |
| `LINE_ENGLISH`, `LINE_CHINESE`, `LINE_COLOR` | chr | 11 lines |
| `OPENING_DATE` | date | 1987-11-07 – 2026-07-12. **Hand-entered**, see Caveats |
| `TRANSPORT_TYPE` | chr | `MRT` or `LRT` |
| `SEARCHVAL`, `BLK_NO`, `ROAD_NAME`, `BUILDING`, `ADDRESS`, `POSTAL` | chr | OneMap address fields |
| `X`, `Y` | dbl | SVY21 projected coordinates. Not used |
| `LATITUDE`, `LONGITUDE` | dbl | WGS84. Used for the distance matrix |

Only `STATION_NAME_ENGLISH`, `OPENING_DATE`, `LATITUDE` and `LONGITUDE` are read
by the pipeline.

## Exclusion rules

Applied by `04_build_modelling_data.R`, which logs the rows each rule removes.

| Rule | Rows dropped | Why |
|---|---|---|
| Exact duplicate rows | 317 | The source carries no transaction ID, so identical rows cannot be told apart from two genuine sales of matching units in the same block and month |
| `floor_area_sqm >= 200` | 12 | Terrace houses and outsized maisonettes: a different product, too few to learn from |
| **Total** | **329** | 238,064 → 237,735 |

The threshold is `MAX_FLOOR_AREA_SQM` in [`R/config.R`](../R/config.R), and the
EDA captions that describe it read the same constant.

## Caveats

**Lease strings use a singular "month".** The source writes a one-month
remainder as `"62 years 01 month"`, not `"01 months"` — 19,267 rows (8.1%). A
parser matching only the plural silently drops that month. `01_data_cleaning.R`
matches both.

**Station opening dates are hand-entered.** OneMap supplies coordinates but not
opening dates. Everything from 2024 onward was added by hand and **should be
re-verified against LTA before the numbers are published**:

| Opened | Line | Stations |
|---|---|---|
| 2024-06-23 | Thomson-East Coast | 7 (TE23–TE29) |
| 2024-08-15 | Punggol LRT | 1 (PW2 Teck Lee) |
| 2024-12-10 | North East | 1 (NE18 Punggol Coast) |
| 2025-02-28 | Downtown | 1 (DT4 Hume) |
| 2026-07-12 | Circle | 3 (CC30 Keppel, CC31 Cantonment, CC32 Prince Edward Road) |

**Stations not yet open at the snapshot date are excluded by design.** TEL5
(Bedok South, Sungei Bedok) and DTL3e (Xilin, Sungei Bedok) were still scheduled
for "2H 2026" as at 2026-08-17 and are absent from the file. They must be added
when they open, or rail distances in that corner of the island will drift stale
the way they had before CCL6 was added.

**`distance_to_nearest_mrt` is a property of a sale, not of a block.** Because it
is evaluated as at the transaction date, the same block takes different values in
different years. Any per-address summary has to choose which one it means — the
app bundle takes the most recent.

**2026 is a partial year.** Coverage ends 2026-08-01, so 2026 holds roughly eight
months of transactions. It is a test-only year under the current split, so the
partial coverage cannot bias training.
