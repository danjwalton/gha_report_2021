required.packages <- c("data.table", "httr", "XML")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/gha_report_2021")

base_link <- "https://dhsprogram.com"
datasets_page_source <- htmlParse(content(GET("https://dhsprogram.com/data/available-datasets.cfm",headers=T,follow=T,parse=T)))
link_elems <- getNodeSet(datasets_page_source,"//td[@id='survey']/a")
link_suffixes <- sapply(link_elems,xmlGetAttr,"href")
link_suffixes <- subset(link_suffixes,link_suffixes!="#footNotesLink")

data.list <- list()
data.index <- 1

full_links <- paste0(base_link,link_suffixes)
for(full_link in full_links){
  message(full_link)
  country_page_source <- htmlParse(content(GET(full_link,headers=T,follow=T,parse=T)))
  
  meta_data_list <- list()
  
  left_table_trs <- getNodeSet(country_page_source,"//div[@id='surveyLeftSide']/table/tr")
  for(i in 1:length(left_table_trs)){
    field_name_elems <- getNodeSet(country_page_source,paste0("//div[@id='surveyLeftSide']/table/tr[",i,"]/td"))
    field_names <- trimws(sapply(field_name_elems,xmlValue))
    field_name <- make.names(field_names[1])
    field_value <- gsub("\n"," ",field_names[2])
    meta_data_list[[field_name]]=field_value
  }
  
  survey_char_elems <- getNodeSet(country_page_source, "//li[@class='alt1']")
  survey_chars <- trimws(gsub("\n"," ",sapply(survey_char_elems,xmlValue)))
  survey_chars <- make.names(survey_chars)
  for(survey_char in survey_chars){
    meta_data_list[[survey_char]] <- 1
  }
  
  data_available_link_elem <- getNodeSet(country_page_source,"//div[@id='datset_div'][1]/a")
  data_available_link_suffix <- sapply(data_available_link_elem,xmlGetAttr,"href")
  data_available_link <- paste0(base_link,data_available_link_suffix)
  
  data_available_page_source <- htmlParse(content(GET(data_available_link,headers=T,follow=T,parse=T)))
  first_td_elem <- getNodeSet(data_available_page_source,"//td[1]")
  first_td_text <- sapply(first_td_elem,xmlValue)
  zips <- trimws(subset(first_td_text,grepl(".zip",first_td_text,ignore.case=T)))
  correct_zips <- subset(zips,nchar(zips)==12)
  dhs_cc <- substr(correct_zips[1],1,2)
  dhs_recode_code <- substr(correct_zips[1],5,6)
  
  survey_id <- substr(full_link, gregexpr("-",full_link)[[1]][4]+1, gregexpr("cfm",full_link)[[1]]-2)
  
  meta_data_list[["dhs_cc"]] <- dhs_cc
  meta_data_list[["dhs_recode_code"]] <- dhs_recode_code
  meta_data_list[["survey_id"]] <- survey_id
  
  meta_data <- data.frame(meta_data_list)
  
  data.list[[data.index]] <- meta_data
  data.index <- data.index + 1
}

all_meta_data <- rbindlist(data.list,fill=T)

all_meta_data[, c("BeginDate","EndDate") := tstrsplit(Fieldwork., " - ")]

all_meta_data$BeginDate=as.Date(paste("01",all_meta_data$BeginDate), format="%d %B %Y")
all_meta_data$EndDate=as.Date(paste("01",all_meta_data$EndDate), format="%d %B %Y")

all_meta_data$dayslength=all_meta_data$EndDate-all_meta_data$BeginDate
all_meta_data$middate=all_meta_data$BeginDate+round(all_meta_data$dayslength/2)
all_meta_data$BeginYear=format(all_meta_data$BeginDate,"%Y")
all_meta_data$EndYear=format(all_meta_data$EndDate,"%Y")
all_meta_data$surveyyr=round(as.numeric(all_meta_data$middate)/365.25)+1970
all_meta_data$surveyyr[which(all_meta_data$BeginYear==all_meta_data$EndYear)]=all_meta_data$BeginYear[which(all_meta_data$BeginYear==all_meta_data$EndYear)]

#List of wealth index surveys
datasets_page_source <- htmlParse(content(GET("https://dhsprogram.com/topics/wealth-index/Wealth-Index-Construction.cfm")))
link_elems <- getNodeSet(datasets_page_source,"//tr/td/a")
link_suffixes <- sapply(link_elems,xmlGetAttr,"href")
link_suffixes <- link_suffixes[grepl("what-we",link_suffixes)]
wealth_ids <- sapply(link_suffixes, function(x) substr(x, gregexpr("-",x)[[1]][4]+1, gregexpr("cfm",x)[[1]]-2))

#Recent surveys which don't appear on list yet
wealth_ids <- c(wealth_ids, "524")

all_meta_data$WealthIndex <- NA
all_meta_data$WealthIndex[which(all_meta_data$survey_id %in% wealth_ids)] <- 1

download.name <- paste0("dhs_metadata_", format(Sys.time(), "%Y%m%d"), ".csv")

fwrite(all_meta_data, paste0("datasets/DHSMICS/", download.name))
