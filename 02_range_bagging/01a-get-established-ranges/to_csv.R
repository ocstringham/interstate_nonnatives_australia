
birds = readRDS("data/establishments/birds/bird_gavia_records.rds")
herps1 = readRDS("data/establishments/herps/kraus_herps.rds")
herps2 = readRDS("data/establishments/herps/cahpina_herps.rds")

# to csv
write.csv(birds, 'data/establishments/birds/bird_gavia_records.csv')
write.csv(herps1, 'data/establishments/herps/kraus_herps.csv')
write.csv(herps2, 'data/establishments/herps/cahpina_herps.csv')
