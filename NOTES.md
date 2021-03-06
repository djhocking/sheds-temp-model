Notes for the SHEDS Stream Temperature Model
============================================

This repo contains various notes about the SHEDS stream temperature model.


## Original Model Scripts

The following code block contains a summary of all scripts and input/output files from the initial model version [conteStreamTemperature_northeast](https://github.com/Conte-Ecology/conteStreamTemperature_northeast).

```txt
# --------------
# KEY ----------
# --------------
# db: dbname@host
# <- input file
# -> output file

# --------------
# WORKING DIR --
# --------------

modelRun/modelRun_$(date +"%Y-%m-%d")

# --------------
# MAIN SCRIPT --
# --------------

run_model.sh - primary script

# --------------
# SCRIPTS ------
# --------------

current_model_run.txt - model run id
status_log.txt - logfile

id_impoundment_sites.sh - identify impounded locations
  db: sheds@felek
  -> impoundment_sites.csv - list of location.id that intersect with impoundments layer
id_tidal_sites.sh - identify tidal locations
  db: sheds@felek
  -> tidal_sites.csv - list of location.id that intersect with tidal layer
retrieve_db.R - fetch streamtemp data from db (exclude locations, qaqc)
  db: sheds@felek
  <- model_config.json
  <- current_model_run.txt
  <- impoundment_sites.csv
  <- tidal_sites.csv
  -> retreive_log.txt
  -> df_values.RData
  -> subdaily_flags.csv
  -> df_values_flags.RData
  -> diagnostics/plots/series_[id].png
  -> daily_flags.csv
  -> series_used.csv
  -> temp_temp.RData
  -> code/daymet_query.sql
  -> featureid_list_20160602.dbf
  -> temperatureData.RData
  -> covariateData.RData
daymet_query.sql - fetch daymet data
  db: sheds@felek
  -> daymet_results.csv
breakpoints.R - determine breakpoints
  db: sheds@felek
  <- current_model_run.txt
  <- temperatureData.RData
  <- daymet_results.csv
  -> springFallBPs.RData
prepare_model_data.R - prepare input data
  db: sheds@felek
  <- model_config.json
  <- current_model_run.txt
  <- temperatureData.RData
  <- daymet_results.csv
  <- covariateData.RData
  <- springFallBPs.RData
  <- series_used.csv
  -> warm_sites/series[id].png
  -> location_use.csv
  -> location_use.dbf
  -> tempDataSync.RData
run_model.R - run model
  <- current_model_run.txt
  <- tempDataSync.RData
  -> jags.RData
  -> covariate_list.RData
mcmc_diagnostics.R - generate MCMC diagnostics
  <- current_model_run.txt
  -> jags.RData
  -> figures/ggmcmc-B0.pdf
  -> figures/ggmcmc-mu-huc.pdf
  -> figures/ggmcmc-mu-year.pdf
  -> figures/ggmcmc-sigma-site.pdf
  -> figures/ggmcmc-sigma-huc.pdf
  -> figures/ggmcmc-sigma-year.pdf
  -> figures/ggmcmc-ar1-rho-huc.pdf
  -> figures/ggmcmc-ar1-B-ar1.pdf
summarize_iterations.R - generate model summary
  <- current_model_run.txt
  <- tempDataSync.RData
  <- jags.RData
  <- covariate_list.RData
  -> coef.RData
  -> coef_iters.RData
validate_model.R - validate model
  <- model_config.json
  <- current_model_run.txt
  <- tempDataSync.RData
  <- covariate_list.RData
  <- coef.RData
  -> rmse_table.RData
  -> obs_predicted.RData
  -> valid_results.RData
  -> RMSE_table.Rds
  -> Obs_Fitted.pdf
  -> Obs_Valid.pdf
predict_temperatures_parallel.R - calculate derived metrics
  db: sheds_new@osensei
  <- current_model_run.txt
  <- coef.RData
  <- tempDataSync.RData
  <- covariate_list.RData
  <- springFallBPs.RData
  -> db_pull_for_predictions.RData (testing)
  -> log_file.txt
  -> derived_site_metrics_full.RData
  -> derived_site_metrics_full.csv
  -> derived_site_metrics_arc.csv
  -> derived_site_metrics_arc.dbf
  -> derived_site_metrics.RData
  -> derived_site_metrics.csv
  -> derived_metrics_ct.csv
  -> derived_metrics_ct_arc.dbf
  -> derived_metrics_huc[##].csv
data_summary.R - calculate summary by state and agency
  db: sheds@felek
  <- current_model_run.txt
  <- series_used.csv
  <- df_values.RData
  -> agency_summary.Rds
  -> state_summary.Rds
  -> data_totals.Rds
```

### Step-By-Step

#### 1: Identify Impoundment Locations

Creates file `impoundment_sites.csv` containing the id of all `locations` that intersect the `gis.impoundment_zones_100m` table.

```bash
psql -h felek.cns.umass.edu -d $DB -w -c "{SQL}" > $FOLDER/impoundment_sites.csv
```

```sql
-- Create temporary table
SELECT * INTO TEMPORARY locations_temp FROM public.locations;

-- Add geometry
ALTER TABLE locations_temp ADD COLUMN geom geometry(POINT,4326);
UPDATE locations_temp SET geom = ST_SetSRID(ST_MakePoint(longitude,latitude),4326);
CREATE INDEX idx_locations_temp_geom ON locations_temp USING GIST(geom);

ALTER TABLE locations_temp ADD COLUMN buffer geometry(POLYGON,4326);
UPDATE locations_temp SET buffer = ST_Buffer(locations_temp.geom::geography, 10)::geometry;
CREATE INDEX idx_locations_temp_buffer ON locations_temp USING GIST(buffer);

-- Select points near impoundment zones
COPY (
SELECT id
  FROM locations_temp, gis.impoundment_zones_100m
  WHERE ST_Intersects(locations_temp.buffer, gis.impoundment_zones_100m.geom)
) TO STDOUT WITH CSV HEADER
```

Ideas:

- Use materialized view to save code for creating locations_temp
- Move `locations_tmp` to `gis.locations`
- Why create buffer (radius = 10?) on `locations_tmp`?

#### Step 2: Identify Tidal Locations

Creates file `impoundment_sites.csv` containing the id of all `locations` that intersect the `gis.impoundment_zones_100m` table.

```bash
psql -h felek.cns.umass.edu -d $DB -w -c "{SQL}" > $FOLDER/tidal_sites.csv
```

```sql
-- Create temporary table
SELECT * INTO TEMPORARY locations_temp FROM public.locations;

-- Add geometry
ALTER TABLE locations_temp ADD COLUMN geom geometry(POINT,4326);
UPDATE locations_temp SET geom = ST_SetSRID(ST_MakePoint(longitude,latitude),4326);
CREATE INDEX idx_loc_dum_geom ON locations_temp USING GIST(geom);

-- Find and output intersection
COPY (
  SELECT locations_temp.id
  FROM locations_temp, tidal_zones
  WHERE ST_Intersects(locations_temp.geom, tidal_zones.geom)
) TO STDOUT WITH CSV HEADER
```

Ideas:

- Creates locations_temp table, could have saved from step 1
- Combine location.id scripts into one script (or one sql statement, faster by scanning entire table once?)

#### Step 3: Retrieve Temperature Data

Fetch stream temperature data from database excluding impoundment and tidal locations.

```
Rscript code/retrieve_db.R $dirname"/temperatureData.RData" $dirname"/covariateData.RData" $dirname"/climateData.RData"
```

Output saved to `retrieve_log.txt`

```
get table references (locations, series, ...)
save reviewed series ids to series_reviewed
save impoundment/tidal location ids to exclude_locations
fetch values from values_flags
  join series
  join variables
  filter variable.name = "TEMP"
         series.id IN series_reviewed
         series.flagged = false
  set datetime tz to EST
  rename temp = value
fetch locations
  filter agency_name != "TEST"
  rename featureid = catchment_id
join values and locations
  filter is.na(feature)
         location_id IN exclude_locations

qaqc hourly values
  obs_freq()
  flag_incomplete()
  flag_hourly_rise()
  flag_cold_obs()
  flag_hot_obs()
  flag_extreme_obs()
  flag_interval()
  convert_neg()

create sd_flags df from values ->
  flag_incomplete == TRUE
  flag_hourly_rise == TRUE
  flag_cold_obs == TRUE
  flag_hot_obs == TRUE
  flag_extremes == TRUE
  flag_interval == TRUE
save flagged values to subdaily_flags.csv

use Median Absolute Deviation (MAD) to flag plots for manual inspection (generates png)

filter subdaily values -> df_values2
  flag_incomplete == "FALSE",
  flag_cold_obs == "FALSE",
  flag_hot_obs == "FALSE",
  flag_interval == "FALSE" | is.na(flag_interval),
  abs(d_temp) < 5 | is.na(d_temp)
  if mad_tf
    MAD_normalized < 5

compute daily[min, max, mean, n] -> df_values_3

qaqc daily values
  flag_daily_rise()
  flag_cold_days()
  flag_hot_days()
  flag_extreme_days()

filter flagged daily values
  flag_daily_rise == TRUE |
  flag_cold_days == TRUE |
  flag_hot_days == TRUE |
  flag_extreme_days == TRUE

compute MAD_normalized = MAD.roller(temp, median(median_freq)*10)
```


