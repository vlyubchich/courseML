Db <- read.csv("./dataraw/benthos_biomass.csv")
summary(Db)
str(Db)


git config --global user.email "lyubchich@umces.edu"
git config --global user.name "vlyubchich"

# 2022-08-10
Drca <- read.csv("./dataraw/rca_data_2012_2022-06-29.csv",
                nrows = 10) # here load only 10 top rows
ls(Drca)
