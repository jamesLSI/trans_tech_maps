library(UsefulFunctions)
library(leaflet)
library(htmltools)

dataWarehouse_function_specific("Applications Summaries")

transformative_technologies <- application_data %>% 
  filter(str_detect(CompetitionName,
                    "Innovate UK Transformative Technologies")) %>% 
  mutate(clean_postcode = if_else(is.na(ApplicantOrganisationWorkPostcode),
                                  ApplicantOrganisationRegisteredPostcode,
                                  ApplicantOrganisationWorkPostcode))

postcode_list <- transformative_technologies %>% 
  distinct(clean_postcode)

postcode_function(postcode_list)

transformative_technologies_clean <- transformative_technologies %>% 
  left_join(postcode_api_output,
            by = c("clean_postcode" = "postcode")) %>% 
  mutate(Region = if_else(result.european_electoral_region == "Eastern",
                          "East of England",
                          result.european_electoral_region))

regional_summary <- transformative_technologies_clean %>% 
  count(Region) %>% 
  mutate(percent = n/sum(n)*100) %>% 
  left_join(bus_pop_average) %>% 
  select(-average_pop) %>% 
  mutate(delta = percent - region_prop)

regional_summary %>% 
  write_csv("tt_apps_region.csv")

super_regions <- rgdal::readOGR("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/International_Territorial_Level_1_January_2021_UK_BUC_2022/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

super_regions@data$ITL121NM <- gsub(" \\(England\\)", "", super_regions@data$ITL121NM)

regions_order <- as.data.frame(super_regions@data$ITL121NM) %>% 
  mutate(regionOrder = row_number()) %>% 
  rename("region" =  "super_regions@data$ITL121NM") %>% 
  mutate(region = as.character(region))

### join summaies to region order to make the map ####
for_map <- regions_order %>% 
  left_join(regional_summary %>% 
              mutate(Region = if_else(Region == "East of England",
                                             "East",
                                      Region)),
            by = c("region" = "Region"))


total_pal <- colorBin("YlOrRd", domain = for_map$delta, bins = 9)

total_labels <- sprintf(
  "<strong>%s</strong><br/>%s",
  for_map$region, as.character(format(for_map$delta,
                                                 big.mark = ",",
                                                 digits = 2,
                                                 scientific = F))) %>% 
  lapply(htmltools::HTML)


{markerOpactiy <- 0.5
  markerRadius <- 6
  labelOpacity <- 1
  textOrNot <- F
  markerColour <- "#003255"}

### draw the map ####
super_regions %>% 
  leaflet() %>% 
  setView(lng = -2.4, lat = 55.5, zoom = 6) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas,
                   group = "Base Map") %>% 
  addPolygons(fillOpacity = 0.75,
              fillColor = "white",
              color = "#4C1354",
              weight = 2,
              group = "Region Outline") %>%
  addPolygons(fillColor = ~total_pal(for_map$delta),
              fillOpacity = 0.6,
              color = "#4C1354",
              weight = 2,
              group = "TT Apps delta from regional business population",
              label = total_labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend(pal = total_pal,
            values = for_map$delta,
            title = "TT Apps delta from regional business population",
            group = "TT Apps delta from regional business population",
            labFormat = labelFormat(suffix = "%"))
