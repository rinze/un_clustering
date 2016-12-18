library(dplyr)
library(readr)
library(reshape2)
library(tsne)
library(ggplot2)
library(lsa) # cosine function

# From https://dataverse.harvard.edu/dataset.xhtml?persistentId=hdl:1902.1/12379
un_data <- read_tsv("~/Dropbox/data/un/Dyadicdata.tab.gz")
# Raw voting data
raw_data <- read_tsv("~/Dropbox/data/un/RawVotingdata.tab.gz")
# Use it for country codes
cc_data <- read_tsv("~/Dropbox/data/un/Idealpoints.tab.gz")
cc_data <- cc_data[, c("ccode", "CountryName")]
cc_data <- cc_data[!duplicated(cc_data$ccode), ]

# Get raw voting data and obtain year and country name
raw_data$year <- 1945 + raw_data$session
raw_data <- left_join(raw_data, cc_data, by = "ccode")
# Let's suppose I can get rid of country 511 (don't have name) and NA.
# Can't find them in the codebook (?)
raw_data <- raw_data[!is.na(raw_data$CountryName), ]

# t-SNE for clustering!
# Color: distance to USA / URSS until 1991. Later: ?

# Test
raw_data_1946 <- raw_data[raw_data$year == 1946, ]
# Or:
# raw_data_1946_wide <- dcast(raw_data_1946, CountryName ~ rcid + vote)
# And use that directly (save for the first column)
raw_data_1946_wide <- dcast(raw_data_1946, rcid + vote ~ CountryName)
cos <- cosine(as.matrix(raw_data_1946_wide[, -c(1, 2)]))
t_1946 <- tsne(cos, k = 2, initial_dims = 30,
               perplexity = 30, whiten = FALSE)

t_1946 <- data.frame(x = t_1946[, 1], y = t_1946[, 2], 
                     #country = raw_data_1946_wide$CountryName)
                     country = names(raw_data_1946_wide[-c(1, 2)]))

plt1 <- ggplot(t_1946) + geom_text(aes(x = x, y = y, label = country))
plot(plt1)