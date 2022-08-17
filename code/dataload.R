Db <- read.csv("./dataraw/benthos_biomass.csv")
summary(Db)
str(Db)


git config --global user.email "lyubchich@umces.edu"
git config --global user.name "vlyubchich"

# 2022-08-10
Drca <- read.csv("./dataraw/rca_data_2012_2022-06-29.csv",
                 nrows = 10) # here load only 10 top rows
ls(Drca)

# Load and reduce size of ROMS-RCA ----
library(dplyr)# 
# Drca <- read.csv("./dataraw/rca_data_2012_2022-06-29.csv") %>% 
#     select(Date, CellID, WTEMP_avg) %>% 
#     mutate(Date = as.Date(Date)) %>% 
#     mutate(Month = as.integer(format(Date, "%m")),
#            Year = as.integer(format(Date, "%Y"))) %>%
#     filter(Year == 2012) %>% 
#     group_by(CellID, Month) %>% 
#     summarise(WTEMP_avg = mean(WTEMP_avg))
# summary(Drca)
# saveRDS(Drca, "./dataderived/Drca.RDS")
## The code above takes a long time to run, so 
## execute it once and comment-out. Then just
## reload the data from the RDS file.
Drca <- readRDS("./dataderived/Drca.RDS")

# Add lat-lon info and map ----
dcells <- read.csv("./dataraw/rca_cells_2022-06-29.csv")
Drca <- merge(Drca, dcells, by = "CellID")
with(Drca,
     plot(x = LON, y = LAT, col = "blue")
)

# Random subsampling of cells ----
CELLS <- unique(Drca$CellID)
set.seed(123)
cells <- sample(CELLS, 100)
# data with the selected cells:
drca <- Drca %>% 
    filter(is.element(CellID, cells))

# Load biomass ----
library(dplyr)
library(geosphere)

## Get bm of bivalves by station in 2012 ----
# load table with taxonimic groups, etc.; select bivalves
bmgroups <- read.csv("./dataraw/benthos_taxa_groups.csv",
                     na.strings = c(".")) %>% 
    mutate(LBL = tolower(LBL)) %>% 
    filter(Hypoxia_grp == "Bivalves")
# load biomass observations, create a new variable: biomass of bivalves
BM <- read.csv("./dataraw/benthos_biomass.csv") %>% 
    filter(Year == 2012) %>% 
    mutate(LBL = tolower(LBL),
           BM_bv = 0) 
# when LBL is in the "Bivalves", let BM_bv be the observed biomass.
BM$BM_bv[is.element(BM$LBL, bmgroups$LBL)] = 
    BM$VALUE[is.element(BM$LBL, bmgroups$LBL)]
# for each station, sum the biomass of bivalves
bm <- BM %>% 
    group_by(STATION, LATITUDE, LONGITUDE, SAMPLE_DATE) %>%  #Year if analyze >1year
    summarise(BM_bv = sum(BM_bv))

## Match stations in 2012 with the model grid cells ----
Dcells <- read.csv("./dataraw/rca_cells_2022-06-29.csv") %>% 
    filter(FSM == 1) # select only water cells

# create new empty variables with the CellID corresponding to the station
# and distance between the station and the cell center
bm$CellID <- bm$DistanceToNearestKm <- NA 

# https://www.r-bloggers.com/2019/10/geographic-distance/
# find geodesic distances from each cell to station
# and select the closest cell
# record that CellID and distance to the station
for (i in 1:nrow(bm)) { # i = 1
    # distance from this station to each cell
    d <- geosphere::distm(bm[i, c("LONGITUDE", "LATITUDE")],
                          Dcells[, c("LON", "LAT")]) / 1000L
    # index of the cell that is the closest to the station
    di <- which.min(d)
    # corresponding distance (smallest from all)
    bm$DistanceToNearestKm[i] <- d[di]
    # corresponding CellID
    bm$CellID[i] <- Dcells$CellID[di]
}
# make sure stations are not far from the cells, remove outliers >= 10 km
bm <- bm %>% 
    filter(DistanceToNearestKm < 10) %>% 
    rename(Date = SAMPLE_DATE) %>% 
    mutate(Date = as.Date(Date))
summary(bm)

# Data compilation complete!
write.csv(bm, './dataderived/Cell_Proximity.csv', 
          row.names = FALSE)
