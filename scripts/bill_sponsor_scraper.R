library(XML)
library(maps)
library(tidyverse)
library(stringr)

# http://www.lrc.ky.gov/record/17RS/bills_S.htm

# parse main sponsor menu page
senate_sponsor_main_parsed <- htmlParse("http://www.lrc.ky.gov/record/17RS/bills_S.htm")
house_sponsor_main_parsed <- htmlParse("http://www.lrc.ky.gov/record/17RS/bills_H.htm")


# extract bill links to vector
senate_sponsor_links <- getHTMLLinks(senate_sponsor_main_parsed)[3:251]
house_sponsor_links <- getHTMLLinks(house_sponsor_main_parsed)[3:535]

# function to pull all sponsor names for each bill
sponsor_scraper <- function(x) {
        master <- as.data.frame(matrix(ncol = 2))
        colnames(master) <- c("bill", "sponsors")
        for(i in 1:length(bill_sponsor_links)) {
                link <- bill_sponsor_links[i]
                link_content <- htmlParse(paste0("http://www.lrc.ky.gov/record/17RS/", link)) %>% as('character')
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

house_sponsors <- sponsor_scraper(house_sponsor_links)
senate_sponsors <- sponsor_scraper(senate_sponsor_links)

write_csv(house_sponsors, "house_sponsors.csv")
write_csv(senate_sponsors, "senate_sponsors.csv")
