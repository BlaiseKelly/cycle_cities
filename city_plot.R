library(sf)
library(osmactive)
library(httr)
library(mapview)
library(tmap)
library(dplyr)
library(terra)
library(exactextractr)

list_of_geos <- c("gemeente")

dir.create("plots")

# get dutch city shape files
for (geo_nam in list_of_geos){

  url <- parse_url("https://service.pdok.nl/cbs/gebiedsindelingen/2023/wfs/v1_0")
  url$query <- list(service = "WFS",
                    version = "2.0.0",
                    request = "GetFeature",
                    typename = paste0("gebiedsindelingen:", geo_nam, "_gegeneraliseerd"),
                    outputFormat = "application/json")
  request <- build_url(url)

  geo_sf <- st_read(request, quiet = TRUE)

  assign(geo_nam, geo_sf)

}

# get some UK city shape files
uk_cities <- st_read("https://open-geography-portalx-ons.hub.arcgis.com/api/download/v1/items/980da620a0264647bd679642f96b42c1/geoPackage?layers=0")

# get population data
pop_2024 <- rast("X:/population/landscan-global-2024.tif")

# pick cities to go with
cities <- c("Manchester","Liverpool", "London", "Bristol", "Cardiff", "Swansea", "Bath", "Newcastle", "Belfast", "Dublin", "Edinburgh", "Glasgow", "Southampton",
            "Birmingham", "Reading", "Vienna", "Berlin", "Hamburg", "Paris", "Zurich",gemeente$statnaam[c(44,66,84,90,97,103,107,165)])


  city_list <- list()
  for (c in cities){
    tryCatch({

      # for speed just go with boundsary boxes for cities
    cty_coords <- tmaptools::geocode_OSM(c)

    # convert bb to polygon representing city
    area <- cty_coords$bbox |>
      st_bbox() |>
      st_as_sfc() |>
      st_as_sf()

    # calc city plot for area polygon
    cty_pop <- exact_extract(pop_2024,area, "sum")

    #
    osm_data = osmactive::get_travel_network(
      place = area,
      boundary = area,
      boundary_type = "clipsrc",
      max_file_size = 9e999
      )

    drive_net = osmactive::get_driving_network(osm_data)
    cycle_net <- osmactive::get_cycling_network(osm_data)

    cycle_net_d = distance_to_road(cycle_net, drive_net)

    cycle_net_c = classify_cycle_infrastructure(cycle_net_d) |>
      select(osm_id,detailed_segregation, geometry)

    seg_paths <- cycle_net_c[cycle_net_c$detailed_segregation %in% c("Level track", "Off Road Path", "Light segregation"),]

    city_dat <- data.frame(city = c,
                           area_city_km2 = as.numeric(st_area(area))/1000000,
                           city_pop = cty_pop,
                           driving_routes = sum(as.numeric(st_length(drive_net)))/1000,
                           cycle_paths = sum(as.numeric(st_length(cycle_net_c)))/1000,
                           seg_cycle = sum(as.numeric(st_length(seg_paths)))/1000
                           )

    city_list[[c]] <- city_dat

    print(c)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }

  city_data <- do.call(rbind,city_list) |>
    mutate(pc_seg = seg_cycle/cycle_paths*100,
           cycle_pp = cycle_paths*1000/city_pop,
           seg_pp = seg_cycle*1000/city_pop,
           drive_pp = driving_routes*1000/city_pop,
           cycle_km2 = cycle_paths*1000/area_city_km2,
           seg_km2 = seg_cycle*1000/area_city_km2,
           drive_km2 = driving_routes*1000/area_city_km2)



cities2plot <- c("Paris", "Rotterdam", "Nieuwegein","Apeldoorn", "Utrecht", "Vienna", "London", "Manchester", "Zurich", "Bristol")
c <- "Utrecht"
#net_list <- list()
for (c in cities2plot){

    cty_coords <- tmaptools::geocode_OSM(c)

    area <- cty_coords$bbox |>
      st_bbox() |>
      st_as_sfc() |>
      st_as_sf()

    cty_pop <- exact_extract(pop_2024,area, "sum")

    osm_data = osmactive::get_travel_network(
      place = area,
      boundary = area,
      boundary_type = "clipsrc",
      max_file_size = 9e999
    )

    drive_net = osmactive::get_driving_network(osm_data)
    cycle_net <- osmactive::get_cycling_network(osm_data)

    cycle_net_d = distance_to_road(cycle_net, drive_net)

    cycle_net_c = classify_cycle_infrastructure(cycle_net_d) |>
      select(osm_id,detailed_segregation, geometry) |>
      mutate(segregated = ifelse(detailed_segregation %in% c("Level track", "Off Road Path", "Light segregation"),"YES","NO"))

    seg_paths <- cycle_net_c[cycle_net_c$detailed_segregation %in% c("Level track", "Off Road Path", "Light segregation"),]

    city_data1 <- data.frame(city = c,
               area_city_km2 = as.numeric(st_area(area))/1000000,
               city_pop = cty_pop,
               driving_routes = sum(as.numeric(st_length(drive_net)))/1000,
               cycle_paths = sum(as.numeric(st_length(cycle_net_c)))/1000,
               seg_cycle = sum(as.numeric(st_length(seg_paths)))/1000
    )

    city_data2 <- city_data1 |>
      mutate(pc_seg = seg_cycle/cycle_paths*100,
             cycle_pp = cycle_paths*1000/city_pop,
             seg_pp = seg_cycle*1000/city_pop,
             drive_pp = driving_routes*1000/city_pop,
             cycle_km2 = cycle_paths/area_city_km2,
             seg_km2 = seg_cycle/area_city_km2,
             drive_km2 = driving_routes/area_city_km2)



    #assign(paste0(c,"_net"), cycle_net_c)

    cycle_net_c$segregated <- factor(cycle_net_c$segregated)  # critical

    bg <- basemaps::basemap_raster(area, map_service = "carto", map_type = "light")

    #assign(paste0(c,"_map"), cycle_net_c)
    tmap_mode("plot")
    tm1 <- tm_shape(bg)+
      tm_rgb()+
      tm_shape(cycle_net_c) +
      tm_edges(
        col = "segregated",
        lwd = 3
      )+
      tm_legend(show = TRUE, position = c(0.85,0.15))+
      tm_title(text = paste0(c, " (boundary box)"), position = c(0,0.99))+
      tm_credits(paste0("Area population: ", format(round(city_data2$city_pop), big.mark = ",", scientific = FALSE), "\n",
                        "cycle path total km: ", round(city_data2$cycle_paths),"\n",
                        "segregated cycle paths total km: ", round(city_data2$seg_cycle),"\n",
                        "metres of total cycle path per person: ", round(city_data2$cycle_pp,2),"\n",
                        "metres of segregated cycle path per person: ", round(city_data2$seg_pp,2),"\n",
                        "km of total cycle path per km\u00B2: ", round(city_data2$cycle_km2,2),"\n",
                        "km of segregated cycle path per km\u00B2: ", round(city_data2$seg_km2,2),"\n",
                        "metres of road per person: ", round(city_data2$drive_pp,2),"\n",
                        "km of road per km\u00B2: ", round(city_data2$drive_km2,2),"\n"),bg = TRUE, size = 0.5,bg.alpha = 0.3, bg.color = "grey95", position = c(0.01,0.93))
      #tm_components(c("tm_legend", "tm_credits"), position = c("left", "top"), bg.color = "grey95")

    tmap_save(tm1, paste0("plots/", c, "_cycle_paths.png"), dpi = 400)

    assign(c, tm1)

    tmap_mode("view")
    tm2 <- tm_shape(cycle_net_c) +
      tm_edges(
        col = "segregated",
        lwd = 3
      )+
      tm_legend(show = TRUE)+
      tm_title(paste0(c, " (area)"))

    assign(paste0(c, "_view"), tm2)

}

tmap_mode("view")
tm_all <- tmap_arrange(Paris_view, Rotterdam_view, Utrecht_view, Vienna_view, London_view, Manchester_view, Zurich_view, Bristol_view, ncol = 4,nrow =4)
tm_all
