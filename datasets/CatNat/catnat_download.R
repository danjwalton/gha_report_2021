required.packages <- c("data.table", "httr", "rvest")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/gha_report_2021")

isos <- fread("datasets/Countrynames/isos.csv", encoding = "UTF-8")
catnat <- fread("datasets/CatNat/catnat_raw.csv", encoding = "UTF-8")

last_date <- as.Date(max(catnat$`Date de début`),"%d/%m/%Y")
margin <- 0 #days to re-read in case of revisions

last_date <- format.Date(last_date - margin,"%d/%m/%Y")

from <- last_date
to <- format.Date(Sys.time(),"%d/%m/%Y")

pre <- paste0("https://www.catnat.net/index.php?option=com_bdcatnat&controller=ajaxsearch&format=raw&action=catalogue&datefrom=", from,"&dateto=", to, "&limit=5&sort=date_debut&order=asc&limitstart=")

retry_read_html <- function(url, errors=500){
  i <- 0
  while(i < 20){
    i <- i + 1
    response <- GET(url)
    if(response$status == 200) break
    if(response$status %in% errors) next
  }
  return(read_html(response))
}

data.list <- list()
lim <- 0
i <- 1
url <- paste0(pre, as.character(lim))
check <- html_table(html_nodes(retry_read_html(url), "table")[2])[[1]]
max.lim <- (as.numeric(substr(check$X2, 8, nchar(check$X2)))-1)*5
rm(check)
while(lim <= max.lim){
  message(paste0("Page ", lim/5+1, "/", max.lim/5+1))
  url <- paste0(pre, as.character(lim))
  ids <- data.table(id = unlist(lapply(grep("view=event", xml_nodes(retry_read_html(url), "a[href]"), value = T), function(x) substr(strsplit(x, "&")[[1]][2], 8, 100))))
  long.data <- list()
  for(j in 1:nrow(ids)){
    tmp <- paste0("https://www.catnat.net/component/bdcatnat/?view=event&id=", ids$id[j], "&tmpl=component")
    id.data <- data.table(t(html_table(html_nodes(retry_read_html(tmp), "table")[1], header = F)[[1]]))
    id.data <- setnames(id.data[2], as.character(id.data[1]))[]
    id.data <- cbind(id = ids$id[j], id.data)
    long.data[[j]] <- id.data
  }
  data <- rbindlist(long.data)
  data.list[[i]] <- data
  i <- i + 1
  lim <- lim + 5
}

catnat_new <- rbindlist(data.list)

catnat_new$`Date de début` <- as.IDate(catnat_new$`Date de début`, format = "%d/%m/%Y")
catnat_new$`Date de fin` <- as.IDate(catnat_new$`Date de fin`, format = "%d/%m/%Y")
catnat_new$Year <- year((catnat_new$`Date de fin` - catnat_new$`Date de début`)/2+catnat_new$`Date de début`)

catnat_new$pays_temp <- tolower(catnat_new$Pays)
isos_temp <- isos[,c("countryname", "countryname_catnat", "iso3", "iso2")]
isos_temp$countryname_catnat <- tolower(isos_temp$countryname_catnat)

catnat_new <- merge(isos_temp, catnat_new, by.x = "countryname_catnat", by.y = "pays_temp", all.y = T)
catnat_new[,c("countryname_catnat","Localisation","Détails","Conséquences humaines et matérielles") := NULL]

nbres <- grep("Nbre", names(catnat_new), value = T)
catnat_new[, (nbres) := lapply(.SD, as.numeric), .SDcols = (nbres)]

catnat <- catnat[!(id %in% catnat_new$id)]
catnat <- rbind(catnat, catnat_new, fill =T)
catnat <- unique(catnat)

fwrite(catnat, "datasets/CatNat/catnat_raw.csv")
