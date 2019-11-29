# load packages
library(tidyverse)
library(geojsonio)
library(sp)
library(leaflet)
library(DT)
library(knitr)

# urls for downloading assets
urls <- list(
	cs19_dt = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/820177/Statistical_tables_-_Civil_Service_Statistics_2019_-_machine_readable_headcounts_version.csv",
	nuts3_codes = "https://opendata.arcgis.com/datasets/3e1d40ce19494869b43a6997e7a539a2_0.geojson",
	nuts1_shapes = "https://opendata.arcgis.com/datasets/01fd6b2d7600446d8af768005992f76a_4.geojson",
	nuts2_shapes = "https://opendata.arcgis.com/datasets/48b6b85bb7ea43699ee85f4ecd12fd36_4.geojson",
	nuts3_shapes = "https://opendata.arcgis.com/datasets/473aefdcee19418da7e5dbfdeacf7b90_4.geojson"
)

# read in civil service stats and filter to table 15
cs19_t15 <- read_csv(urls$cs19_dt, col_types = "cccccccccccn", na = c("..", "-")) %>%
	filter(table == "t15")

# read in NUTS3 lookup codes
# rename the Western Isles to align with label is cs19
nuts3_codes <- geojson_read(urls$nuts3_codes, parse = TRUE) %>%
	pluck("features") %>%
	pluck("properties") %>%
	mutate(
		NUTS318NM = case_when(
			NUTS318CD == "UKM64" ~ "Na h-Eileanan Siar",
			TRUE ~ NUTS318NM))

# clean up cs stats table
cs_nuts3 <- cs19_t15 %>%
	filter(category_1 != "All employees", category_2 == "All employees", category_4 == "Total") %>%
	group_by(category_1) %>%
	summarise_at(vars(value), sum, na.rm = TRUE) %>%
	rename(NUTS318NM = category_1) %>%
	full_join(nuts3_codes) %>%
	mutate(pc = formattable::percent(
		value/445480),
		value = formattable::comma(value, digits = 0))

# Read in NUTS3 shapes
nuts3_spdf <- geojson_read(urls$nuts3_shapes, what = "sp")

# Read in NUTS1 shapes, select only London
nuts1_spdf <- geojson_read(urls$nuts1_shapes, what = "sp")
london_spdf <- nuts1_spdf[nuts1_spdf$nuts118cd == "UKI",]

# Read in NUTS2 shapes, select Greater Manchester
nuts2_spdf <- geojson_read(urls$nuts2_shapes, what = "sp")
manchester_spdf <- nuts2_spdf[nuts2_spdf$nuts218cd == "UKD3",]

# Select remaining Core Cities from NUTS3
core_cities <- c(Birmingham = "UKG31", Bristol = "UKK11", Cardiff = "UKL22", 
				 Glasgow = "UKM82", Leeds = "UKE42", Liverpool = "UKD72", 
				 Newcastle = "UKC22", Nottingham = "UKF14", Sheffield = "UKE32")
cities_spdf <- nuts3_spdf[nuts3_spdf$nuts318cd %in% core_cities,]

# merge nuts3 shapes with data
leaf_dt <- sp::merge(nuts3_spdf, cs_nuts3, by.x = "nuts318cd", by.y = "NUTS318CD")

# create colouring function
bincol <- colorBin(palette = "YlGnBu",
				   domain = leaf_dt$value,
				   bins = c(0, 500, 1000, 2500, 5000, 7000, 40000, 50000),
				   pretty = FALSE,
				   na.color = "#eeeeee")

leaflet(leaf_dt, width = "100%", height = 600) %>%
	addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
	addMapPane("dt", zIndex = 410) %>%
	addMapPane("labs", zIndex = 420) %>%
	addPolygons(color = "#aaaaaa",
				weight = 1,
				fillColor = ~bincol(value),
				popup = ~paste(NUTS318NM, value, sep = ": "),
				fillOpacity = 0.8,
				options = pathOptions(pane = "dt")) %>%
	addPolygons(data = london_spdf,
				color = "#F47738",
				opacity = 1,
				weight = 2,
				fill = FALSE,
				group = "London",
				options = pathOptions(pane = "dt")) %>%
	addPolygons(data = manchester_spdf,
				color = "#F47738",
				opacity = 1,
				weight = 3,
				fill = FALSE,
				group = "Core Cities",
				options = pathOptions(pane = "dt")) %>%
	addPolygons(data = cities_spdf,
				color = "#F47738",
				opacity = 1,
				weight = 3,
				fill = FALSE,
				group = "Core Cities",
				options = pathOptions(pane = "dt")) %>%
	addProviderTiles(providers$CartoDB.PositronOnlyLabels, 
					 options = providerTileOptions(pane = "labs")) %>%
	addLegend(position = "topright", pal = bincol, values = ~leaf_dt$value, 
			  title = "Headcount", opacity = 0.8) %>%
	addLayersControl(
		overlayGroups = c("London", "Core Cities"), position = "bottomright",
		options = layersControlOptions(collapsed = FALSE)
	) %>%
	hideGroup(c("London", "Core Cities"))