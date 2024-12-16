#-----------------#
# 0. Settings ####
#-----------------#


rm(list = ls())

packages <- c("readr", "dplyr", "stringr", "sf", "ggplot2", "forecast", "lubridate", "randomForest",
              "prophet", "inline", "openxlsx", "tidyr")

lapply(packages, function(x) {
  if(!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
  library(x, character.only = TRUE)
})


rm(packages)


path.data <- paste0(getwd(),"/app/")
setwd(path.data)



#--------------------------------#
# 1. load & prepare datasets ####
#--------------------------------#

# Datensatz für die regionale Zuweisung einlesen und aufbereiten
regionen <- read_tsv(paste0(path.data,"GrippeWeb_Zuordung_Regionen.tsv")) 
regionen <- regionen %>%
  select(!Land) %>%
  filter(Region != "Bundesweit")

# Bevölkerungsstand einlesen
folks <- read.xlsx(paste0(path.data,"Bevölkerungsstand.xlsx"))
folks <- folks %>%
  pivot_longer(
    cols = -Bundesland, # Alle Spalten außer "Bundesland" ins Long-Format umwandeln
    names_to = "Jahr",  # Die Namen der Spalten (z. B. 2011, 2012) werden zur neuen Spalte "Jahr"
    values_to = "Einwohner"  # Die Werte der Spalten werden zur neuen Spalte "Wert"
  ) %>%
  mutate(Bundesland = gsub("ü", "ue", Bundesland, fixed = TRUE)) %>%
  left_join(regionen, by = "Bundesland") %>%
  select(!Bundesland) %>%
  group_by(Jahr, Region) %>%
  summarise(Einwohner = sum(Einwohner)) %>%
  mutate(Jahr = as.numeric(Jahr))


  
# Datensatz für die Shapefiles der Bundesländer einlesen, aufbereiten und mergen
sf <- st_read(paste0(path.data,"Shapefile/vg2500_bld.shp"), options = "ENCODING=ISO-8859-1")
sf <- sf %>%
  rename("Bundesland" = "GEN") %>%
  mutate(Bundesland = gsub("ü", "ue", Bundesland, fixed = TRUE),
         geometry = st_make_valid(geometry)) %>%
  left_join(regionen, by = "Bundesland") %>%
  group_by(Region) %>%
  summarise(SHAPE_LENG = sum(SHAPE_LENG, na.rm = TRUE),
            SHAPE_AREA = sum(SHAPE_AREA, na.rm = TRUE),
            geometry = st_union(geometry))

plot(sf["geometry"], main = "Regionen Deutschlands", axes = TRUE, graticule = TRUE)


# Datensatz für die Inzidenzen einlesen, aufbereiten und mergen
data <- read_tsv(paste0(path.data,"GrippeWeb_Daten_des_Wochenberichts.tsv"))
data <- data %>%
  filter(Region != "Bundesweit") %>%
  mutate(Jahr = as.numeric(str_extract(Kalenderwoche, "^\\d{4}")),
         Woche = as.numeric(str_extract(Kalenderwoche, "(?<=W)\\d+")),
         Datum = as.Date(paste0(Jahr, "-1-1")) + weeks(Woche -1),
         Inzidenz = ifelse(Inzidenz == 0, 1, Inzidenz)
         ) %>% # 0en sollen ersetzt werden um die Berechnung von MAPE und anderen Metriken zu ermöglichen
 # left_join(sf, by = "Region") %>%
  select(Erkrankung, Region, Inzidenz, Jahr, Woche, Datum) #%>% #SHAPE_LENG, SHAPE_AREA, geometry)
  #left_join(folks, by = c("Jahr", "Region"))

rm(regionen, folks)


#--------------------#
# 2. Zeitreihenmodell
#--------------------#


# Ergebnisse-Datenframe initialisieren
forecast_results <- data.frame()

# Schleife für alle Regionen und Erkrankungen
for (region in unique(data$Region)) {
  for (erkrankung in unique(data$Erkrankung)) {
    
    # Daten filtern
    region_data <- data %>%
      filter(Region == region, Erkrankung == erkrankung) %>%
      arrange(Datum)
    
    # Daten für Prophet vorbereiten
    prophet_data <- region_data %>%
      select(ds = Datum, y = Inzidenz)
    
    # Modell erstellen
    m <- prophet(prophet_data, growth = "flat", yearly.seasonality = TRUE, weekly.seasonality = TRUE)
    
    # Zukunftsdaten erstellen (Vorhersage für 312 Wochen, bis 2030)
    future <- make_future_dataframe(m, periods = (2030 - max(year(region_data$Datum))) * 52, freq = "week")
    
    # Vorhersage durchführen
    forecast <- predict(m, future)
    
    # Ergebnisse speichern
    forecast_df <- forecast %>%
      select(ds, yhat) %>%
      rename(Datum = ds, Inzidenz = yhat) %>%
      mutate(Region = region, Erkrankung = erkrankung)
    
    # Ergebnisse anhängen
    forecast_results <- bind_rows(forecast_results, forecast_df)
  }
}

# Sicherstellen, dass Datum in beiden Datensätzen keine Zeitzone enthält
forecast_results <- forecast_results %>%
  mutate(Datum = as.Date(Datum))  # Entfernt mögliche Zeitzonen
data <- data %>%
  mutate(Datum = as.Date(Datum))  # Entfernt mögliche Zeitzonen

# Nur zukünftige Daten anhängen
forecast_results <- forecast_results %>%
  filter(Datum > max(data$Datum))


# Historische Daten und Vorhersagen kombinieren
data_prophet <- data %>%
  bind_rows(forecast_results) %>%
  left_join(sf, by = "Region")

# Plot der Inzidenz für eine Region und Erkrankung
ggplot(data_prophet %>% filter(Region == "Sueden", Erkrankung == "ARE"), aes(x = Datum)) +
  geom_line(aes(y = Inzidenz, color = ifelse(Datum <= max(data$Datum[data$Datum <= Sys.Date()]), 
                                             "Historische Daten", "Vorhersage"))) +
  labs(title = "Prophet-Vorhersage für Osten - ARE", x = "Datum", y = "Inzidenz") +
  theme_minimal() +
  scale_color_manual(values = c("Historische Daten" = "blue", "Vorhersage" = "red"))


rm(forecast, forecast_df, forecast_results, m, prophet_data, region_data)



#----------------------#
# 3. Random Forest ####
#---------------------#

# Daten für den Zeitraum der Vorhersage erstellen (2024 bis 2030)
forecast_dates <- seq.Date(from = as.Date("2024-12-02"), to = as.Date("2030-11-18"), by = "week")

# Anzahl Regionen und Erkrankungen bestimmen
unique_regions <- unique(data$Region)
unique_erkrankungen <- unique(data$Erkrankung)

# Vorhersagemerkmale erstellen
forecast_features <- expand.grid(
  Datum = forecast_dates,
  Region = unique_regions,
  Erkrankung = unique_erkrankungen
) %>%
  mutate(
    Jahr = year(Datum),
    Woche = week(Datum)
  )

# Daten vorbereiten: Historische Daten
train_data <- data %>%
  select(Datum, Inzidenz, Jahr, Woche, Region, Erkrankung) %>%
  na.omit()  # Entfernen von NA-Werten, falls vorhanden

# Sicherstellen, dass die Faktorniveaus konsistent sind
train_data$Region <- as.factor(train_data$Region)
forecast_features$Region <- factor(forecast_features$Region, levels = levels(train_data$Region))
train_data$Erkrankung <- as.factor(train_data$Erkrankung)
forecast_features$Erkrankung <- factor(forecast_features$Erkrankung, levels = levels(train_data$Erkrankung))

# Modelltraining
set.seed(123)  # Für Reproduzierbarkeit
rf_model <- randomForest(
  Inzidenz ~ Jahr + Woche + Region + Erkrankung,
  data = train_data,
  ntree = 500,
  mtry = 3,
  importance = TRUE
)

# Vorhersage durchführen
forecast_features$Inzidenz <- predict(rf_model, newdata = forecast_features)

# Ergebnisse speichern
forecast_results <- forecast_features %>%
  select(Datum, Region, Erkrankung, Inzidenz)

# Historische und vorhergesagte Daten kombinieren
data_rf <- train_data %>%
  select(Datum, Region, Erkrankung, Inzidenz) %>%
  bind_rows(forecast_results) %>%
  rename(Inzidenz_rf = Inzidenz)

ggplot(data_rf %>% filter(Region == "Osten", Erkrankung == "ARE"), aes(x = Datum, y = Inzidenz_rf, color = ifelse(Datum <= max(train_data$Datum), "Historische Daten", "Vorhersage"))) +
  geom_line() +
  labs(title = "Random-Forest-Vorhersage für Osten - ARE", x = "Datum", y = "Inzidenz", color = "Datenquelle") +
  theme_minimal() +
  scale_color_manual(values = c("Historische Daten" = "blue", "Vorhersage" = "red"))



#---------------------#
# 4. Daten mergen ####
#---------------------#

data <- data_prophet %>%
  rename(Inzidenz_pr = Inzidenz) %>%
  left_join(data_rf, by = c("Datum", "Region", "Erkrankung")) %>%
  mutate(Region = gsub("Mitte (West)", "Westen", Region, fixed = TRUE),
         Region = gsub("Norden (West)", "Norden", Region, fixed = TRUE))

saveRDS(data, paste0(path.data,"data.rds"))


