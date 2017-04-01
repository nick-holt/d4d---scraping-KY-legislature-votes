# KY State Legislature 2017 General Session Bill Sponsor Scrape
# Nick Holt and Robert Kahne - D4D Hackathon - March 31, 2017

# libraries
library(XML)
library(maps)
library(tidyverse)
library(stringr)

# http://www.lrc.ky.gov/record/17RS/bills_S.htm

# parse main sponsor menu page
senate_sponsor_main_parsed <- htmlParse("http://www.lrc.ky.gov/record/17RS/bills_S.htm")
house_sponsor_main_parsed <- htmlParse("http://www.lrc.ky.gov/record/17RS/bills_H.htm")


# extract bill links to vectors
senate_sponsor_links <- getHTMLLinks(senate_sponsor_main_parsed)[3:251]
house_sponsor_links1 <- getHTMLLinks(house_sponsor_main_parsed)[3:100]
house_sponsor_links2 <- getHTMLLinks(house_sponsor_main_parsed)[101:200]
house_sponsor_links3 <- getHTMLLinks(house_sponsor_main_parsed)[201:300]
house_sponsor_links4 <- getHTMLLinks(house_sponsor_main_parsed)[301:400]
house_sponsor_links5 <- getHTMLLinks(house_sponsor_main_parsed)[401:535]

# function to pull all sponsor names for each bill
sponsor_scraper <- function(x) {
        master <- as.data.frame(matrix(ncol = 2))
        colnames(master) <- c("bill", "sponsors")
        for(i in 1:length(x)) {
                link <- x[i]
                link_content <- htmlParse(paste0("http://www.lrc.ky.gov/record/17RS/", link), isURL = T) %>% as('character')
                sponsors <- str_extract_all(link_content, ">\\S+\\s+\\S+<")
                sponsors <- str_extract_all(sponsors, "[A-Z].\\s[A-Z,a-z]{1,}")
                sponsors <- str_extract_all(sponsors, "[A-Z,a-z].\\s[A-Z,a-z]{1,}")
                if(length(sponsors[[1]]) < 1) {
                        sponsors[[1]] <- NA
                } else {
                }
                target <- as.data.frame(matrix(nrow = length(sponsors[[1]]), ncol = 2))
                colnames(target) <- c("bill", "sponsors")
                bill_name <- str_extract(link, "\\w+")
                target$bill <- bill_name
                target$sponsors <- sponsors[[1]]
                master <- rbind(master, target)
        }
        return(master)
}

# scrape in chunks separately (might be getting shut off by server)
house_sponsors1 <- sponsor_scraper(house_sponsor_links1)
house_sponsors2 <- sponsor_scraper(house_sponsor_links2)
house_sponsors3 <- sponsor_scraper(house_sponsor_links3)
house_sponsors4 <- sponsor_scraper(house_sponsor_links4)
house_sponsors5 <- sponsor_scraper(house_sponsor_links5)

# bind house sponsor dfs
house_sponsors <- rbind(house_sponsors1, house_sponsors2, house_sponsors3, house_sponsors4, house_sponsors5)

# scrape senate sponsor info
senate_sponsors <- sponsor_scraper(senate_sponsor_links)

# write to csv
write_csv(house_sponsors, "house_sponsors.csv")
write_csv(senate_sponsors, "senate_sponsors.csv")
