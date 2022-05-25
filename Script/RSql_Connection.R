library(odbc)
library(DBI)
library(tidyverse)

data("population")
data("who")

# Create database

con <- dbConnect(drv = RSQLite::SQLite(),dbname=':memory:')


# Store Sample data in Database

dbWriteTable(conn = con,name = 'population',value = population)

dbWriteTable(conn = con,name = 'who',value = who)

# Remove the local data from the environment

rm(who,population)

# Query data from the data base
# The source of the database connection profile
tbl(src = con, 'Who')

tbl(src = con,'population')
