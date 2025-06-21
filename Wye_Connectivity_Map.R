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

bins <- seq(0, 500, by = 25)

# Create base inverted viridis palette

vir_colors <- c(
  "#e8e3f4",  # 0–25 (very light lavender)
  "#dcd2ec",  # 25–50
  "#cfbee4",  # 50–75
  "#c2aada",  # 75–100
  "#b497d0",  # 100–125
  "#9e83c0",  # 125–150
  "#8770b0",  # 150–175
  "#6e5fa0",  # 175–200
  "#555091",  # 200–225
  "#3b4381",  # 225–250
  "#2b4a89",  # 250–275
  "#1d5b8f",  # 275–300
  "#13708f",  # 300–325
  "#198b85",  # 325–350
  "#35a37b",  # 350–375
  "#64bb6a",  # 375–400
  "#9dd946",  # 400–425
  "#cfee2c",  # 425–450
  "#f6f325",  # 450–475
  "#ffff29"   # 475–500
)

pal <- colorBin(
  palette = vir_colors,
  domain = c(0, 500),
  bins = bins,
  na.color = "transparent"
)

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