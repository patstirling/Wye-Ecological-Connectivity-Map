library(sf)
library(leaflet)
library(htmlwidgets)
library(viridis)

# Load and clean
aoi <- st_read("Wye Catchment_2019-1990_ECA_output_EMcH.gpkg")
projects <- st_read("Mapped projects.kml")

aoi <- st_zm(aoi, drop = TRUE, what = "ZM") |> st_transform(4326)
projects <- st_zm(projects, drop = TRUE, what = "ZM") |> st_transform(4326)

aoi <- st_make_valid(aoi)
projects <- st_make_valid(projects)
aoi <- aoi[!is.na(st_geometry(aoi)), ]
projects <- projects[!is.na(st_geometry(projects)), ]

# Cap ECA values
aoi$eca_2019 <- pmin(aoi$hex.standardised.euclid.eca_2019, 500)
aoi$eca_1990 <- pmin(aoi$hex.standardised.euclid.eca_1990, 500)

# Custom palette with lighter 0–100
custom_viridis <- c(
  "#e0dced",  # light grey/purple for 0–100
  "#414487",  # 100–200
  "#2a788e",  # 200–300
  "#22a884",  # 300–400
  "#fde725"   # 400–500
)

bins <- c(0, 100, 200, 300, 400, 500)
pal <- colorBin(palette = custom_viridis, domain = c(0, 500), bins = bins, na.color = "transparent")

# Popup generator function
generate_popup <- function(yr) {
  paste0(
    "<strong>Grid ID:</strong> ", aoi$grid_id, "<br>",
    "<strong>ECA (", yr, "):</strong> ", round(aoi[[paste0("hex.standardised.euclid.eca_", yr)]], 1), " ha<br>",
    "<strong>Patches:</strong> ", aoi[[paste0("n.clumps_", yr)]], "<br>",
    "<strong>Total habitat:</strong> ", round(aoi[[paste0("tot.patch.ha_", yr)]], 1), " ha (",
    round(aoi[[paste0("tot.aw.patch.ha_", yr)]], 1), " ha ancient)<br>",
    "<strong>Core habitat:</strong> ", round(aoi[[paste0("tot.patch.ha_", yr)]] - aoi[[paste0("tot.edge.patch.ha_", yr)]], 1), " ha (",
    round(aoi[[paste0("tot.aw.patch.ha_", yr)]] - aoi[[paste0("tot.awedge.patch.ha_", yr)]], 1), " ha ancient)<br>",
    "<strong>Edge habitat:</strong> ", round(aoi[[paste0("tot.edge.patch.ha_", yr)]], 1), " ha (",
    round(aoi[[paste0("tot.awedge.patch.ha_", yr)]], 1), " ha ancient)"
  ) |> lapply(htmltools::HTML)
}

# Build map
m <- leaflet(options = leafletOptions(minZoom = 9)) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng = -3.05, lat = 51.9, zoom = 10) %>%
  
  # ECA 2019
  addPolygons(
    data = aoi,
    fillColor = ~pal(eca_2019),
    color = "#444444",
    weight = 0.5,
    fillOpacity = 0.8,
    group = "ECA 2019",
    popup = generate_popup("2019"),
    label = ~paste0("ECA 2019: ", round(hex.standardised.euclid.eca_2019, 1), " ha"),
    labelOptions = labelOptions(direction = "auto", textsize = "12px")
  ) %>%
  
  # ECA 1990
  addPolygons(
    data = aoi,
    fillColor = ~pal(eca_1990),
    color = "#444444",
    weight = 0.5,
    fillOpacity = 0.8,
    group = "ECA 1990",
    popup = generate_popup("1990"),
    label = ~paste0("ECA 1990: ", round(hex.standardised.euclid.eca_1990, 1), " ha"),
    labelOptions = labelOptions(direction = "auto", textsize = "12px")
  ) %>%
  
  # WVW Projects
  addPolygons(
    data = projects,
    color = "black",
    fillColor = "limegreen",
    fillOpacity = 0.8,
    weight = 2,
    group = "WVW Projects"
  ) %>%
  
  # Legend
  addLegend(
    position = "bottomright",
    pal = pal,
    values = c(0, 500),
    title = "ECA — ha",
    opacity = 1,
    labFormat = labelFormat(suffix = "")
  ) %>%
  
  # Layer toggle
  addLayersControl(
    overlayGroups = c("ECA 2019", "ECA 1990", "WVW Projects"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup("ECA 1990") %>%
  
  # Info box
  addControl(
    html = paste0(
      "<div style='
        background: white;
        padding: 1rem;
        font-size: 1rem;
        max-width: 55vw;
        line-height: 1.6;
        box-shadow: 0 2px 6px rgba(0,0,0,0.15);
        border-radius: 6px;
        word-wrap: break-word;
        z-index: 9999;
      '>
      <b>Wye Valley Wilding Project Map </b>
      <br>Overlaid with ecological connectivity analysis
      based on woodland habitat. Ecological connectivity mapping provided by Dr Ewan McHenry (Woodland Trust).<br><br>
      <b>ECA:</b> Effective Connected Area based on modelling of woodland ecological
      interconnectivity from 1990 and 2019. Habitat data used to assign ecological 
      characteristics to each hex comes primarily from the Land Cover Map (LCM) 
      datasets produced by the UK Centre for Ecology & Hydrology (UKCEH). Click on a hex for more information.
    </div>"
    ),
    position = "bottomleft"
  )

# Save output
output_path <- "/Users/patrickstirling/Wye_Connectivity_Map/map_with_projects.html"
saveWidget(m, file = output_path, selfcontained = TRUE, title = "Wye Connectivity Map")
browseURL(output_path)