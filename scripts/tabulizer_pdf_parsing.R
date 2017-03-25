# Parse scraped pdfs of Kentucky State Legislature Vote Outcomes
# Nick Holt - 3/25/17

# libraries
library(rJava)
library(tabulizer)
library(dplyr)
library(tidyverse)

# after installing java via chocolately and the rJava package, this will install tabulizer

#if (!require("ghit")) {
#        install.packages("ghit")
#}
# on 64-bit Windows
#ghit::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")

#-------------------------------------------------------------------------
# build functions
#-------------------------------------------------------------------------

# create paster function
paster <- function(x,y) {
        y[is.na(y)] <- ""
        str_c(x,y, sep = " ")
}

# create parser function
parser <- function(x){
        
        # extract all text from .pdf
        text <- extract_text(x)
        
        # clean text
        text <- str_replace_all(text, "[\r\n]" , " ")
        text <- str_split(text, pattern = " ")
        text <- as_vector(text)
        
        # detect vector index locations for YEAS, NAYS, ABSTAINED, and NOT VOTING
        yay_begin <- max(which(str_detect(text, "YEAS")))
        nay_begin <- max(which(str_detect(text, "NAYS")))
        abs_begin <- max(which(str_detect(text, "ABSTAINED")))
        nv_begin <- max(which(str_detect(text, "NOT")))
        
        # nv begin location begins with NOT, so concatenate NOT with VOTING located in nv_begin+1
        text[nv_begin+1] <- str_c(text[nv_begin], " ", text[nv_begin+1])
        
        # create vectors containing names of those who voted each way by subsetting between index locations
        yay <- text[yay_begin:nay_begin-1]
        nay <- text[nay_begin:abs_begin-1]
        abs <- text[abs_begin:nv_begin-1]
        nv <- text[nv_begin:length(text)]
        
        # clean up first name characters into new column for merging
        yfnc <- str_extract(yay, "^\\w{1,2}$")
        yfnc <- yfnc[2:length(yfnc)]
        yfnc[length(yfnc)+1] <- NA
        
        nfnc <- str_extract(nay, "^\\w{1,2}$")
        nfnc <- nfnc[2:length(nfnc)]
        nfnc[length(nfnc)+1] <- NA
        
        afnc <- str_extract(abs, "^\\w{1,2}$")
        afnc <- afnc[2:length(afnc)]
        afnc[length(afnc)+1] <- NA
        
        nvfnc <- str_extract(nv, "^\\w{1,2}$")
        nvfnc <- nvfnc[2:length(nvfnc)]
        nvfnc[length(nvfnc)+1] <- NA
        
        # create dfs
        yay <- as.data.frame(paster(yay, yfnc))
        yay$vote <- "Yay"
        nay <- as.data.frame(paster(nay, nfnc))
        nay$vote <- "Nay"
        abs <- as.data.frame(paster(abs, afnc))
        abs$vote <- "Abs"
        nv <- as.data.frame(paster(nv, nvfnc))
        nv$vote <- "NV"
        
        # create and add column names
        var_names <- c("Name", "Vote")
        
        colnames(yay) <- var_names
        colnames(nay) <- var_names
        colnames(abs) <- var_names
        colnames(nv) <- var_names
        
        # bind and clean up
        all <- rbind(yay, nay, abs, nv)
        all$Name <- str_replace_all(all$Name, "^\\S{1,2}\\s$|..\\d+|\\d+|ABSTAINED|NOT|\\s+VOTING|NAYS|YEAS|\\s+", " ")
        all$Name <- str_replace_all(all$Name, "\\s+", " ")
        all <- all[!(all$Name == " "),]
        all <- all[!(all$Name == "St. " | all$Name == "St."),]
        all$Name[all$Name == "Onge" | all$Name == "Onge "] <- "St. Onge"
        all$Name <- str_replace(all$Name, "\\s+$", "")
        
        colnames(all)[2] <- str_sub(x, -9, -4)
        
        # left join to master
        master <<- left_join(master, all)
}

#-------------------------------------------------------------------------------------------------
# Parse files
#-------------------------------------------------------------------------------------------------

# create list of file names for enrolled bills and bills that passed only in the house of reps
law_files <- list.files(path=paste0(getwd(), "/pdfs/signed_law"), pattern="*.pdf", full.names=T, recursive=FALSE)
enrolled_files <-list.files(path=paste0(getwd(), "/pdfs/passedbills_enrolled"), pattern="*.pdf", full.names=T, recursive=FALSE)
onehouse_files <- list.files(path=paste0(getwd(), "/pdfs/onehouse_house_bills"), pattern="*.pdf", full.names=T, recursive=FALSE)
        
# import rep names
master <- read_csv("rep_names.csv") %>%
arrange(Name)

# parse all signed law files    
        
        # set custom file ranges (2 of the files in the directory won't parse and throw errors)
        #enrolled_files1 <- files[1:19]
        #enrolled_files2 <- files[21:length(files)]
        
        # parse
        try(lapply(law_files, parser))
        
        # copy master df into new df
        master_law <- master
        
        # clean up column names
        colnames(master_law) <- str_replace_all(colnames(master_law), "/|\\.", "")
        
        # write enrolled bill house of rep votes to csv
        write_csv(master_law, "all_signed_law_house_votes.csv")

# reset master (import rep names)
master <- read_csv("rep_names.csv") %>%
        arrange(Name)

# parse all enrolled files    
        
        # set custom file ranges (2 of the files in the directory won't parse and throw errors)
        enrolled_files1 <- files[1:19]
        enrolled_files2 <- files[21:length(files)]
        
        # parse
        try(lapply(enrolled_files1, parser))
        try(lapply(enrolled_files2, parser))
        
        # copy master df into new df
        master_enrolled <- master
        
        # clean up column names
        colnames(master_enrolled) <- str_replace_all(colnames(master_enrolled), "/|\\.", "")
        
        # write enrolled bill house of rep votes to csv
        write_csv(master_enrolled, "all_enrolled_house_rep_votes_only.csv")

# reset master (import rep names)
master <- read_csv("rep_names.csv") %>%
        arrange(Name)

# parse all onehouse files from the house of reps
        
        # parse
        try(lapply(onehouse_files, parser))
        
        # copy master df to new df
        master_onehouse <- master

        # clean up column names
        colnames(master_onehouse) <- str_replace_all(colnames(master_onehouse), "/|\\.", "")

        # write onehouse bill house of rep votes to csv
        write_csv(master_onehouse, "all_passed_onehouse_house_bills.csv")






