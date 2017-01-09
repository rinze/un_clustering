library(dplyr)
library(readr)
library(reshape2)
library(tsne)
library(ggplot2)
theme_set(theme_bw(16))
library(ggrepel)
library(lsa) # cosine function

get_time_evolution <- function(cosined_data, country1, country2) {
    # Returns the evolution of the "agreement" between country1 and country2
    # through the different years. If one year does not contain data, put NA
    # in place.
    
    offset <- 1945 # year offset
    c_str <- paste(country1, country2, sep = " - ")
    
    agreement <- sapply(cosined_data, function(m) {
        # m is a matrix with the proper row / column names. Let's try to
        # find the requested countries in there.
        country1 <- which(grepl(country1, rownames(m)))
        country2 <- which(grepl(country2, colnames(m)))
        if (length(country1) == 1 && length(country2) == 1) {
            return(m[country1, country2])
        } else {
            return(NA)
        }
    })
    
    # Build data.frame
    res <- data.frame(agreement = agreement,
                      countries = c_str,
                      years = 1:length(cosined_data) + offset)
    return(res)
}

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
# Binary vote (unused):
# 1 -> Yes
# 0 -> Everything else
raw_data$vote2 <- ifelse(raw_data$vote == 1, 1, 0)
raw_data <- left_join(raw_data, cc_data, by = "ccode")
# Let's suppose I can get rid of country 511 (doesn't have name) and NA.
# Can't find it in the codebook (?)
raw_data <- raw_data[!is.na(raw_data$CountryName), ]

# Very simple check. Should be similar to this table:
# http://www.un.org/en/sections/member-states/growth-united-nations-membership-1945-present/index.html
l <- raw_data %>% group_by(year) %>% summarise(n = length(unique(CountryName)))
plot(l$year, l$n)

# Generate the cosine matrices. One per year, returned as a list
years <- unique(raw_data$year)
cosined_data <- lapply(years, function(year) {
    cat("Computing cosine metrics for year", year, "\n")
    raw_data_year <- raw_data[raw_data$year == year, ]
    raw_data_year$CountryName <- factor(raw_data_year$CountryName)
    raw_data_year_wide <- dcast(raw_data_year, rcid + vote3 ~ CountryName, fun.aggregate = length)
    cos <- cosine(as.matrix(raw_data_year_wide[, -c(1, 2)]))
    return(cos)
    })

# Plotting example
ccdata <- rbind(get_time_evolution(cosined_data, "Russia", "Cuba"),
                get_time_evolution(cosined_data, "China", "North Korea"),
                get_time_evolution(cosined_data, "Venezuela", "Cuba"),
                get_time_evolution(cosined_data, "Spain", "Morocco"))

plt1 <- ggplot(ccdata) + geom_line(aes(x = years, y = agreement, color = countries), size = 1.2) +
        scale_color_brewer(palette = "Set1", name = "") +
        scale_x_continuous(breaks = seq(min(ccdata$years), max(ccdata$years), 5),
                           minor_breaks = seq(min(ccdata$years), max(ccdata$years), 1)) +
        theme(legend.position = "bottom", legend.direction = "vertical")
plot(plt1)

# Proceed with the clustering
for (year in years) {
    cat("Processing year", year, "...\n")
    cos <- cosined_data[[year - min(raw_data$year) + 1]] # 1946 -> 1, and so on
    t_year <- tsne(dist(1 - cos), k = 2, max_iter = 3000,
                   perplexity = 100, whiten = FALSE)
    
    t_year <- data.frame(x = t_year[, 1], y = t_year[, 2],
                         country = rownames(cos))
    
    plt1 <- ggplot(t_year) + geom_text_repel(aes(x = x, y = y, label = country)) +
            geom_point(aes(x = x, y = y)) +
            ggtitle(sprintf("The world in %d", year))
    plot(plt1)
    ggsave(sprintf("/tmp/un_cluster_%s.pdf", year), plot = plt1,
           width = 15, height = 15)
}
