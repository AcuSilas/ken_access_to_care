###################  Process all SNT foundational data  ########################

cli::cli_h1("Process all SNT foundational data")
## ---------------------------------------------------------------------------##
# Setup, data, and parameters --------------------------------------------------
## ---------------------------------------------------------------------------##

cli::cli_process_start(
  "Setting up environment and loading data",
  msg_done = "Environment set up and data loaded"
)

# set country iso3
iso3 <- "ken"
adm0_name <- "Kenya"

# Set up path
paths <- sntutils::setup_project_paths()

# set up file name for paths
shapefile_raw_path <- "ke_subcounty.shp"
hf_path <- "kenya_public_facilities_2019.csv"
pop_path <- "sub_county_pop.xlsx"

cli::cli_process_done()

## ---------------------------------------------------------------------------##
# Shapefiles                --------------------------------------------------##
## ---------------------------------------------------------------------------##
cli::cli_process_start(
  "Processing shapefile data"
)
# Add shapefile layer
adm3_shp <- sf::read_sf(
  here::here(
    paths$admin_shp, "raw", shapefile_raw_path
  )
  ) |>
  dplyr::mutate(
    adm0 = toupper(adm0_name)
  ) |> dplyr::select(
    adm0,
    adm1 = province,
    adm2 = county,
    adm3 = subcounty,
    geometry
  ) |>
  dplyr::mutate(
    adm1 = toupper(adm1),
    adm2 = toupper(adm2),
    adm3 = toupper(adm3)
  )

# validate and clean shapefile
shp_output <- sntutils::validate_process_spatial(
  shp = adm3_shp,
  name = glue::glue("{adm0_name}_boundaries"),
  adm0_col = "adm0",
  adm1_col = "adm1",
  adm2_col = "adm2",
  adm3_col = "adm3"
)

# save cleaned file in different formats
sntutils::write_snt_data(
  obj = shp_output,
  data_name = glue::glue("{iso3}_shp_list"),
  path = here::here(paths$admin_shp, "processed"),
  file_formats = c("qs2")
)

sntutils::write_snt_data(
  obj = shp_output$final_spat_vec$adm3,
  data_name = glue::glue("{iso3}_adm3_shp"),
  path = here::here(paths$admin_shp, "processed"),
  file_formats = c("geojson")
)

# adm2 validation plot of shapefile
sntutils::plot_admin_map_distinct(
  shp = shp_output$final_spat_vec$adm2,
  group_col = "adm2",
  id_col = "adm2_guid",
  out_png = here::here(
    paths$val_fig,
    glue::glue("{iso3}_adm2_val_map.png")
  )
)

## ---------------------------------------------------------------------------##
# Health facilities         --------------------------------------------------##
## ---------------------------------------------------------------------------##
cli::cli_process_start(
  "Processing health facility master list data"
)

# read in master facility list
shp_hf <- sntutils::read(
  here::here(
    paths$hf,
    "raw",
    hf_path
  )
) |>
dplyr::mutate(
  id = dplyr::row_number()
) |>
dplyr::select(
  id,
  hf = "Facility name",
  type_hf = "Facility type",
  hf_lat = Lat,
  hf_lon = Long,
  adm2 = Admin1,
  Ownership
) |> 
dplyr::mutate(
  hf = toupper(hf)
) |> 
dplyr::filter(
  dplyr::if_all(c(hf_lat, hf_lon), 
  ~ !is.na(.)))

# validate mfl
shp_hf_valid <- sntutils::validate_process_coordinates(
  data = shp_hf,
  name = glue::glue("MFL valdation for {adm0_name}"),
  id_col = "id",
  lon_col = "hf_lon",
  lat_col = "hf_lat",
  adm0_sf = shp_output$final_spat_vec$adm0,
  min_decimals = 3,
  fix_issues = TRUE
)

# hf with old admin names
shp_hf <- shp_hf_valid$final_points_df |>
  sf::st_join(shp_output$final_spat_vec$adm3) |>
  dplyr::distinct(id, .keep_all = TRUE) |>
  dplyr::mutate(
    hf_uid_mfl = sntutils::vdigest(
      paste0(adm0, adm1, adm2.y, adm3, hf),
      algo = "xxhash32"
    )
  ) |>
  dplyr::select(
    adm0,
    adm1,
    adm2 = adm2.y,
    adm3,
    adm0_guid,
    adm1_guid,
    adm2_guid,
    adm3_guid,
    hf,
    hf_uid_mfl,
    type_hf,
    hf_lon = lon,
    hf_lat = lat,
    Ownership,
    geometry_hash,
    geometry
  )

# make data dictionary (ensure persistent translation cache)
dict_hf <- sntutils::build_dictionary(
  data = shp_hf,
  language = "fr",
  trans_cache_path = here::here(paths$cache)
)

# make list to save
hf_data <- list(
  data = shp_hf,
  data_dictionary = dict_hf
)

# save mfl data
sntutils::write_snt_data(
  obj = hf_data,
  data_name = glue::glue("{iso3}_mfl_processed"),
  path = here::here(paths$hf, "processed"),
  file_formats = c("qs2", "xlsx")
)

# save validation output
sntutils::write_snt_data(
  obj = shp_hf_valid,
  data_name = glue::glue("{iso3}_hf_validation"),
  path = here::here(paths$val_tbl),
  file_formats = c("xlsx", "qs2")
)

## ---------------------------------------------------------------------------##
# Population data           --------------------------------------------------##
## ---------------------------------------------------------------------------##
cli::cli_process_start(
  "Processing population data",
  msg_done = "Population data processed"
)

# import pop data
pop_data <- readxl::read_xlsx(
  path = here::here(
    paths$pop_national,
    "raw",
    pop_path
  )
) |>
dplyr::rename(
  adm2 = County,
  adm3 = "Sub County",
  pop = Total
) |>
dplyr::select(
  adm2,
  adm3,
  pop,
  male = Male,
  female = Female,
  pop_density = "Pop density",
  sq_km = "Square Km"
) |>
dplyr::mutate(
  adm0 = toupper(adm0_name),
  adm2 = toupper(adm2),
  adm3 = toupper(adm3),
  year = 2019
) |>
dplyr::select(
  adm0,
  dplyr::everything()
)

# Use Shapefile to populate adm1 column in our dataset
adm_lookup <- sf::st_drop_geometry(shp_output$final_spat_vec$adm3) |>
  dplyr::distinct(adm1, adm2, adm3)

pop_data_processed <- pop_data |>
  dplyr::left_join(
    adm_lookup,
    by = c("adm2", "adm3")) |>
  dplyr::select(
    adm0,
    adm1,
    dplyr::everything()
  )

# extrapolate the future years,
# read the documentation on this function to explore more usage
pop_data_30 <- sntutils::extrapolate_pop(
  data = pop_data_processed,
  year_col = "year",
  pop_cols = c("Male", "Female"),
  multiplier = 1.023, # country growth rate used is 2.3%
  group_cols = c("adm0", "adm1", "adm2", "adm3"),
  years_to_extrap = c(2023:2030)
) |>
  dplyr::mutate(
    pop = dplyr::if_else(is.na(pop), Male + Female, pop)
  )

# process pop data and produce different
# files for each admin level
pop_df_list <- sntutils::snt_process_population(
  pop_data = pop_data_30,
  trans_cache_path = here::here(paths$cache),
  pop_cols = c("pop", "Male", "Female"),
  language = "fr",
  translate = TRUE
)

# Save validation output
sntutils::write_snt_data(
  obj = pop_data_processed,
  data_name = glue::glue("{iso3}_pop_processed"),
  path = here::here(paths$pop_national, "processed"),
  file_formats = c("qs2", "xlsx")
)

## ---------------------------------------------------------------------------##
# Urbanicity data           --------------------------------------------------##
## ---------------------------------------------------------------------------##

cli::cli_process_start(
  "Downloading and processing urbanicity data",
  msg_done = "Urbanicity data downloaded and processed"
)

# Download urbanicity rasters
sntutils::download_worldpop_urbanicity(
  country_codes = c("KEN"),
  years = 2015:2030,
  layers = "L1",
  dest_dir = here::here(paths$physical_feat, "raw", "urbanicity")
)

# dowmlpoad population rasters
sntutils::download_worldpop(
  country_codes = c("KEN"),
  years = 2000:2030,
  dest_dir = here::here(paths$pop_worldpop, "raw")
)

# population weight urbanicity
urbanicity_pop_weighted <-
sntutils::process_weighted_raster_collection(
  value_raster_dir = here::here(paths$physical_feat, "raw", "urbanicity"),
  pop_raster_dir = here::here(paths$pop_worldpop, "raw"),
  shapefile = shp_output$final_spat_vec$adm3,
  id_cols = c("adm0", "adm1", "adm2", "adm3"),
  value_pattern = "\\.tif$",
  pop_pattern = "\\.tif$",
  stat_type = "median"
)

# Save validation output
sntutils::write_snt_data(
  obj = urbanicity_pop_weighted,
  data_name = glue::glue("{iso3}_urbanicity_pop_weighted_processed"),
  path = here::here(paths$physical_feat, "processed", "urbanicity"),
  file_formats = c("qs2", "xlsx")
)

cli::cli_process_done()

# Finished ---------------------------------------------------------------------

# clean environment
invisible(gc())

cli::cli_rule(
  left = "All Processing is Complete",
  right = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
)
