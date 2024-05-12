library(sf)
library(ggplot2)
library(viridis)
library(ggfx)
library(raster)
library(foreign)
library(miscTools)

#Read in electoral and geography geometries.
fed_sf <- read_sf("FED_CA_2021_EN.shp")
fed_dbf <- read.dbf("FED_CA_2021_EN.dbf", as.is = F)
cdn_sf <- read_sf("lpr_000b21a_e.shp")

#Build data frame for electoral data.
fed <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(fed) <- c('FED_NUM', 'Bloc Québécois', 'Conservative', 'Green Party', 'Liberal', 'NDP-New Democratic Party', "People's Party - PPC")

#Loop through file list and extract electoral data.
for (i in seq_along(file_list)) {
  filename <- paste('~/R/FED_CA_2021/pollresults/',file_list[[i]], sep='')
  
  # Read data in
  df <- read.csv(filename, header = TRUE)
  
  # Extract district from filename
  FED_NUM = gsub("~/R/FED_CA_2021/pollresults/pollresults_resultatsbureau", "",
                  gsub(".csv", '', filename))
  df[["Filename"]] = FED_NUM
  
  # Add district number, name, vote totals(BQ, CPC, GPC, LPC, NDP, PPC) to data_list.
  rinsert <- c(FED_NUM,
                      sum(df[which(df$Political.Affiliation.Name_English.Appartenance.politique_Anglais=="Bloc Québécois"), 18]),
                      sum(df[which(df$Political.Affiliation.Name_English.Appartenance.politique_Anglais=='Conservative'), 18]),
                      sum(df[which(df$Political.Affiliation.Name_English.Appartenance.politique_Anglais=="Green Party"), 18]),
                      sum(df[which(df$Political.Affiliation.Name_English.Appartenance.politique_Anglais=='Liberal'), 18]),
                      sum(df[which(df$Political.Affiliation.Name_English.Appartenance.politique_Anglais=='NDP-New Democratic Party'), 18]),
                      sum(df[which(df$Political.Affiliation.Name_English.Appartenance.politique_Anglais=="People's Party - PPC"), 18]))
  test[nrow(fed) + 1,] = rinsert
}

#Clean up output
fed[] <- lapply(fed, as.numeric)
fed$max <- apply(fed, 1, max, na.rm = TRUE)
fed$max_party <- colnames(fed[,2:7])[max.col(fed[,2:7], ties.method = 'first')]
fedFinal <- merge(fed, fed_sf, by='FED_NUM')

#Assign colours to parties.
party_colour <- c(
  "Bloc Québécois" = "#4ab0e1",
  "Conservative" = "#0856a1",
  "Green Party" = "#00b04e",
  "Liberal" = "#e81f27",
  "NDP-New Democratic Party" = "#f18225"
)

#Plotting the results.
ggplot() +
  as_reference(
    geom_sf(data = cdn_sf, color = "white", fill = "black"),
    id = 'cdn') +
  with_mask(
    geom_sf(data = test2$geometry, color = NA, aes(fill = test2$max_party)),
    mask = ch_alpha('cdn')) +
  scale_fill_manual(drop = FALSE,
    values = party_colour,
    limits = names(party_colour)) +
  labs(title = "44th Canadian      Parlement\n         Parliament   Canadien  44e",
       fill = element_blank()) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 18, face = "bold"),
        legend.position = "bottom")
