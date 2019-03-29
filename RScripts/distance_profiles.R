# Distance Decay for LSOA Commmutes.
library(sf)
library(readr)
library(dplyr)
library(stplanr)
library(ggplot2)
library(reshape2)

# Read In data
#lines <- readRDS("../cyipt-securedata/pct-lines-all.Rds")
lsoa_cents = st_read("D:/Users/earmmor/GitHub/cyipt-bigdata/centroids/LSOA/Lower_Layer_Super_Output_Areas_December_2011_Population_Weighted_Centroids.shp")
data_path = "D:/Users/earmmor/OneDrive - University of Leeds/LSOA Flow Data/Public/WM12EW[CT0489]_lsoa.zip"

# Prep Data
data_path = "D:/Users/earmmor/OneDrive - University of Leeds/LSOA Flow Data/Public/WM12EW[CT0489]_lsoa.zip"
unzip(data_path)
flow = read_csv("WM12EW[CT0489]_lsoa.csv")
file.remove(c("WM12EW[CT0489]_lsoa.csv",
              "reminder_on_the_conditions_of_use.txt",
              "ukda137-enduserlicence.pdf",
              "WM12EW[CT0489].txt"))

head(flow)

flow$id = paste0(flow$`Area of usual residence`," ", flow$`Area of Workplace`)
flow$`Area Name` = NULL
flow$`Area of Workplace name` = NULL

# Make lines
lsoa_cents = lsoa_cents[,c("lsoa11cd")]
flow = as.data.frame(flow)
flow = od2line(flow, lsoa_cents)
st_crs(flow) = 27700
flow$length = as.numeric(st_length(flow))
flow$length_km <- round(flow$length / 1000)
#flow$length_km <- round((flow$length / 1000)/5)*5

saveRDS(flow,"data_secure/LSOA_flow.Rds")

# Compare All Modes ages and Genders
overall = flow[!is.na(flow$length_km),]
st_geometry(overall) = NULL
overall$`Area of usual residence` = NULL
overall$`Area of Workplace` = NULL
overall$length = NULL
overall$id = NULL
overall = overall %>%
            group_by(length_km) %>%
            summarise_all(sum)
overall = overall[order(overall$length_km),]

saveRDS(overall,"data/national_decay_curves.Rds")
# exclude 
plot(overall$length_km, overall$AllMethods_AllSexes_Age16Plus, type = "l")
max(overall$AllMethods_AllSexes_Age16Plus)
#names(overall)

overall_mode = overall[,c("length_km",
                          "AllMethods_AllSexes_Age16Plus",
                          "Underground_AllSexes_Age16Plus",
                          "Train_AllSexes_Age16Plus",
                          "Bus_AllSexes_Age16Plus",
                          "Taxi_AllSexes_Age16Plus",
                          "Motorcycle_AllSexes_Age16Plus",
                          "CarOrVan_AllSexes_Age16Plus",
                          "Passenger_AllSexes_Age16Plus",
                          "Bicycle_AllSexes_Age16Plus",
                          "OnFoot_AllSexes_Age16Plus",
                          "OtherMethod_AllSexes_Age16Plus"
                          )]
names(overall_mode) = c("length_km",
                        "AllMethods",
                        "Underground",
                        "Train",
                        "Bus",
                        "Taxi",
                        "Motorcycle",
                        "CarOrVan",
                        "Passenger",
                        "Bicycle",
                        "OnFoot",
                        "OtherMethod"
                        )

# Convert to percentage of total for each column
for(i in 2:ncol(overall_mode)){
  message(i)
  overall_mode[[i]] <- overall_mode[[i]] / sum(overall_mode[[i]]) * 100
}

overall_mode <- melt(overall_mode, id.vars="length_km")

ggplot(overall_mode, aes(length_km,value, col=variable)) + 
  geom_line(size = 0.5) +
  xlab("Straight line distance in km") +
  ylab("Percentage of all trips by mode") +
  ggtitle("Distance decay curve by mode") +
  ggsave("plots/distance_deacy_lsoa.jpg")

ggplot(overall_mode, aes(length_km,value, col=variable)) + 
  geom_line(size = 0.5) +
  xlab("Straight line distance in km") +
  ylab("Percentage of all trips by mode") +
  ggtitle("Distance decay curve by mode, 20 km - 500 km") +
  xlim(20,500)+
  ylim(0,1.2) +
  ggsave("plots/distance_deacy_lsoa_20km-500km.jpg")

ggplot(overall_mode, aes(length_km,value, col=variable)) + 
  geom_line(size = 1.5) +
  scale_color_brewer(palette="Spectral") +
  xlab("Straight line distance in km") +
  ylab("Percentage of all trips by mode") +
  ggtitle("Distance decay curve by mode, under 20 km") +
  xlim(0,20)+
  #ylim(0,1.2) +
  ggsave("plots/distance_deacy_lsoa_under20km.jpg")

