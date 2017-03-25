# Scrape pdfs of Kentucky State Legislature Vote Outcomes
# Nick Holt - 3/25/17

# libraries
library(stringr)
library(XML)
library(maps)

# parse web page for bills that are now law
kylaw_parsed <- htmlParse("http://www.lrc.ky.gov/record/17RS/law.htm")

# parse web page for enrolled bills
kyenrolled_parsed <- htmlParse("http://www.lrc.ky.gov/record/17RS/enrolled.htm")

# parse web page for bills that only passed one house
ky_onehouse_parsed <- htmlParse("http://www.lrc.ky.gov/record/17RS/passed_one_house.htm")

# extract bill links to vector
law_bill_addresses <- getHTMLLinks(kylaw_parsed)[3:109]
enrolled_bill_addresses <- getHTMLLinks(kyenrolled_parsed)[3:52]
onehouse_house_bill_addresses <- getHTMLLinks(ky_onehouse_parsed)[3:77]
onehouse_senate_bill_addresses <- getHTMLLinks(ky_onehouse_parsed)[177:203]

# loop to download each bill in directory
bill_scraper <- function(x){
        link <- x
        bill_name <- str_split(link, pattern = "\\.", simplify = T)[1]
        vote_history_link <- paste0("http://www.lrc.ky.gov/record/17RS/", bill_name, "/vote_history.pdf")
        bill_name <- str_split(link, pattern = "\\.", simplify = T)[1]
        download_folder <- paste0(getwd(), "/pdfs/")
        pdf_name <- paste0(download_folder, bill_name, ".pdf")
        download.file(vote_history_link, pdf_name, method = "auto", quiet = F, mode = "wb")
}

# scrape bills that are now signed law
lapply(law_bill_addresses, bill_scraper)

# scrape enrolled bills
lapply(enrolled_bill_addresses, bill_scraper)

# scrape bills that passed one house (house of reps)
lapply(onehouse_house_bill_addresses, bill_scraper)

# scrape bills that passed one house (senate)
lapply(onehouse_senate_bill_addresses, bill_scraper)


