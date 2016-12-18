library(dplyr)
library(readr)
library(reshape2)
library(tsne)
library(ggplot2)
library(ggrepel)
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
# Remove non-members (vote code 9)
raw_data <- raw_data[which(raw_data$vote != 9), ]

# Vote recoding.
# Ternary vote:
# 1 -> Yes
# -1 -> No
# 0 -> Abstain or no vote
raw_data$vote3 <- ifelse(raw_data$vote == 1, 1,
                         ifelse(raw_data$vote == 3, -1, 0))
# Binary vote:
# 1 -> Yes
# 0 -> Everything else
raw_data$vote2 <- ifelse(raw_data$vote == 1, 1, 0)
raw_data <- left_join(raw_data, cc_data, by = "ccode")
# Let's suppose I can get rid of country 511 (don't have name) and NA.
# Can't find it in the codebook (?)
raw_data <- raw_data[!is.na(raw_data$CountryName), ]

# Very simple check. Should be similar to this table:
# http://www.un.org/en/sections/member-states/growth-united-nations-membership-1945-present/index.html
l <- raw_data %>% group_by(year) %>% summarise(n = length(unique(CountryName)))
plot(l$year, l$n)

# t-SNE for clustering!
# Color: distance to USA / URSS until 1991. Later: ?

# Test
year <- 1950
raw_data_year <- raw_data[raw_data$year == year, ]
raw_data_year$CountryName <- factor(raw_data_year$CountryName)
# Or:
# raw_data_year_wide <- dcast(raw_data_year, CountryName ~ rcid + vote, fun.aggregate = length)
# And use that directly (save for the first column)
raw_data_year_wide <- dcast(raw_data_year, rcid + vote2 ~ CountryName, fun.aggregate = length)
cos <- cosine(as.matrix(raw_data_year_wide[, -c(1, 2)]))
t_year <- tsne(cos, k = 2, initial_dims = 50, max_iter = 3000,
               perplexity = 20, whiten = FALSE)

t_year <- data.frame(x = t_year[, 1], y = t_year[, 2], 
                     #country = raw_data_year_wide$CountryName)
                     country = names(raw_data_year_wide[-c(1, 2)]))

plt1 <- ggplot(t_year) + geom_text_repel(aes(x = x, y = y, label = country)) +
        geom_point(aes(x = x, y = y)) +
        ggtitle(sprintf("The world in %d", year))
plot(plt1)
