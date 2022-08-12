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
bmgroups <- read.csv("./dataraw/benthos_taxa_groups.csv",
                     na.strings = c(".")) %>% 
    mutate(LBL = tolower(LBL)) %>% 
    filter(Hypoxia_grp == "Bivalves")

BM <- read.csv("./dataraw/benthos_biomass.csv") %>% 
    filter(Year == 2012) %>% 
    mutate(LBL = tolower(LBL),
           BM_bv = 0) 
BM$BM_bv[is.element(BM$LBL, bmgroups$LBL)] = 
    BM$VALUE[is.element(BM$LBL, bmgroups$LBL)]

bm <- BM %>% 
    group_by(STATION, LATITUDE, LONGITUDE) %>%  #Year if analyze >1year
    summarise(BM_bv = sum(BM_bv))


Dcells <- read.csv("./dataraw/rca_cells_2022-06-29.csv") %>% 
    filter(FSM == 1) #select only water cells

# Dben <- read.csv("./data_benthos/Station_Data.csv")

bm$CellID <- bm$DistanceToNearestKm <- NA #create a new empty variable

# https://www.r-bloggers.com/2019/10/geographic-distance/

# find geodesic distances from each cell to station
# and select the closest non-missing as a Replacement
for (i in 1:nrow(bm)) { # i = 1
    # distance from this station to each cell
    d <- geosphere::distm(bm[i, c("LONGITUDE", "LATITUDE")],
                          Dcells[, c("LON", "LAT")]) / 1000L
    di <- which.min(d)
    bm$DistanceToNearestKm[i] <- d[di]
    bm$CellID[i] <- Dcells$CellID[di]
}
bm <- bm %>% 
    # make sure stations are not far from the cells
    filter(DistanceToNearestKm < 10) 

# Data compilation complete!
write.csv(bm, './dataderived/Cell_Proximity.csv', 
          row.names = FALSE)


