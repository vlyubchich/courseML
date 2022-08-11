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
bmgroups <- read.csv("./dataraw/benthos_taxa_groups.csv",
                     na.strings = c("."))

BM <- read.csv("./dataraw/benthos_biomass.csv") %>% 
    filter(Year == 2012)

