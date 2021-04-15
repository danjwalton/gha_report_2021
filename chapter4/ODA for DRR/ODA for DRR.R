#danjwalton 2019

required.packages <- c("data.table")
lapply(required.packages, require, character.only = T)

#wd <- "H:/GitHub/crs_kws"
#setwd(wd)

load_crs <- function(dataname="crs", path="project_data"){
  require("data.table")
  files.bz <- list.files(path, pattern=paste0(dataname, "_part.+[.]bz"))
  files.csv <- list.files(path, pattern=paste0(dataname, "_part.+[.]csv"))
  if(length(files.bz) > 0 & length(files.csv) > 0){
    files <- files.csv
    read.crs <- function(x){return(fread(x))}
  } else {
    if(length(files.bz) > 0){
      files <- files.bz
      read.crs <- function(x){return(read.csv(x))}
    } else {
      files <- files.csv
      read.crs <- function(x){return(fread(x))}
    }
  }
  crs <- list()
  for(i in 1:length(files)){
    print(paste0("Loading part ", i, " of ", length(files)))
    filepath <- paste0(path, "/", files[i])
    crs[[i]] <- read.crs(filepath)
  }
  crs <- rbindlist(crs)
  return(crs)
}

crs <- fread("H:/Desktop/DRR/R code/R/project_data.csv")

keep <- c(
  "crs_id",
  "project_number",
  "year",
  "aid_type",
  "flow_name",
  "donor_name",
  "recipient_name",
  "usd_commitment_deflated",
  "usd_disbursement_deflated",
  "purpose_name",
  "project_title",
  "short_description",
  "long_description",
  "drr",
  "channel_reported_name"
)

crs <- crs[, ..keep]
crs <- crs[
  flow_name == "ODA Loans" 
  |
    flow_name == "ODA Grants"
  | 
    flow_name == "Equity Investment"
  | 
    flow_name == "Private Development Finance"
  ]

crs <- crs[as.character(year) >= 2018]

major.keywords <- c(
  "anti-seismic adaption",
  "cbdrm",
  "climate protection",
  "climate resilience",
  "climate vulnerability",
  "coastal protection",
  "cyclone preparedness",
  "disaster management",
  "disaster reduction",
  "disaster resilience",
  "disaster risk mitigation",
  "disaster risk reduction",
  "disaster vulnerability reduction",
  "disaster risk management",
  "drm",
  "drr",
  "early warning",
  "earthquake-resistant",
  "earthquake resistant",
  "earthquake resistance",
  "embankment",
  "flood control",
  "flood mitigation plan",
  "flood prevention",
  "flood protection",
  "flood risk management",
  "fpi",
  "gfdrr",
  "hyogo framework of action",
  "lutte contre les inondations",
  "réduction des risques de catastrophes",
  "resilience to disasters",
  "resilience to natural",
  "resilience to shock",
  "shock resilience",
  "storm warning",
  "vulnerability and capacity assessment",
  "disaster risk assessment",
  "multi-hazard risk mapping",
  "resilient infrastructure",
  "disaster insurance",
  "disaster risk insurance",
  "disaster risk analysis",
  "flood risk",
  "resilience to earthquakes",
  "seismically safe standards",
  "disaster preparedness plan",
  "disaster preparedness policy",
  "disaster preparedness",
  "disaster resistant construction",
  "disaster resilient building",
  "vulnerability to natural hazards",
  "disaster-resilient",
  "forest fire prevention",
  "hazard monitoring",
  "katastrophenvorsorge",
  "vorhersagebasiert",
  "frühwarnsystem",
  "klimaanpassung",
  "katastrophenrisik",
  "katastrophenvorbeugung",
  "evakuierungsplan",
  "réduction des risques de catastrophe",
  "changement climatique",
  "résilience climatique",
  "préparation aux catastrophes",
  "prévention des catastrophes",
  "résistante aux catastrophes",
  "cadre de sendai",
  "résilience aux risques naturels",
  "vulnérabilité aux risques naturels",
  "construction résistantes aux catastrophes",
  "alerte précoce",
  "preparación y prevención desastre",
  "prevención y preparación en caso de desastre",
  "cambio climático",
  "resiliencia a amenazas naturales",
  "reducción del riesgo de desastres",
  "resiliencia a los desastres",
  "vulnerabilidad frente a los desastres",
  "marco de sendai",
  "variabilidad climática",
  "risk financing",
  "sendai framework"
)

#minor.keywords <- c(
  #"keyword"

 #)

disqualifying.keywords <- c(
"serendipity"
)

disqualifying.sectors <- c(
  "Disaster Risk Reduction"
  
)

crs$relevance <- "None"
#crs[grepl(paste(minor.keywords, collapse = "|"), tolower(paste(crs$ProjectTitle, crs$ShortDescription, crs$LongDescription)))]$relevance <- "Minor"
crs[grepl(paste(major.keywords, collapse = "|"), tolower(crs$long_description))]$relevance <- "Minor"
crs[grepl(paste(major.keywords, collapse = "|"), tolower(paste(crs$short_description, crs$project_title)))]$relevance <- "Major"

crs$check <- "No"
crs[relevance == "Minor"]$check <- "potential false positive"
crs[relevance != "None"][purpose_name %in% disqualifying.sectors]$check <- "potential false negative"
crs[relevance != "None"][grepl(paste(disqualifying.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$project_title, crs[relevance != "None"]$short_description, crs[relevance != "None"]$long_description)))]$check <- "potential false negative"

crs[relevance != "None"][grepl(paste(disqualifying.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$project_title, crs[relevance != "None"]$short_description, crs[relevance != "None"]$long_description)))]$relevance <- "None"
crs[relevance != "None"][purpose_name %in% disqualifying.sectors]$relevance <- "None"

crs$DRR <- as.character(crs$DRR)
crs[is.na(DRR)]$DRR <- "0"
crs[DRR != "1" & DRR != "2"]$DRR <- "No DRR component"
crs[DRR == "1"]$DRR <- "Partial DRR component"
crs[DRR == "2"]$DRR <- "Major DRR component"

crs_output <- crs
fwrite(crs_output, "H:/Desktop/DRR/R code/R/crs output.csv")
rm(crs)

#split_and_save <- function(crs, path="project_data", compression_level = 0){
#  require("data.table")
#  dataname <- deparse(substitute(crs))
#  if(compression_level > 0){
#    existingfiles <- paste0(path,"/",list.files(path, pattern = paste0(dataname, "_part.+[.]bz")))
#    if(length(existingfiles)>0)invisible(lapply(existingfiles, file.remove))
#    size <- object.size(crs)
#    parts <- ceiling(size/500000000)
#    crs.splits <- split(crs, factor(sort(rank(row.names(crs))%%parts)))
#    invisible(sapply(1:length(crs.splits), function(x, i) write.csv(x[[i]], bzfile(paste0(path, "/",dataname,"_part", i, ".bz"), compression = compression_level)), x=crs.splits))
#  } else {
#    existingfiles <- paste0(path,"/",list.files(path, pattern = paste0(dataname, "_part.+[.]csv")))
#    if(length(existingfiles)>0)invisible(lapply(existingfiles, file.remove))
#    size <- object.size(crs)
#    parts <- ceiling(size/75000000)
#    crs.splits <- split(crs, factor(sort(rank(row.names(crs))%%parts)))
#    invisible(sapply(1:length(crs.splits), function(x, i) fwrite(x[[i]], paste0(path, "/",dataname,"_part", i, ".csv")), x=crs.splits))
#  }
#}

#split_and_save(crs_output, "output", 0)

#crs.years <- dcast.data.table(crs_output, Year ~ relevance, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
#crs.donors <- dcast.data.table(crs_output, Year + DonorName ~ relevance, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
#crs.recipients <- dcast.data.table(crs_output, Year + RecipientName ~ relevance, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
#crs.sectors <- dcast.data.table(crs_output, Year + PurposeName ~ relevance, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
#crs.flows <- dcast.data.table(crs_output, Year + FlowName ~ relevance, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))

#fwrite(crs.years, "output/crs years.csv")
#fwrite(crs.sectors, "output/crs sectors.csv")
#fwrite(crs.flows, "output/crs flows.csv")
#fwrite(crs.donors, "output/crs donors.csv")
#fwrite(crs.recipients, "output/crs recipients.csv")

tocheck.positive <- crs_output[check == "potential false positive"]
tocheck.negative <- crs_output[check == "potential false negative"]
fwrite(tocheck.positive, "H:/Desktop/DRR/R code/R/crs check positives.csv")
fwrite(tocheck.negative, "H:/Desktop/DRR/R code/R//crs check negatives.csv")