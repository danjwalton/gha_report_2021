required.packages <- c("data.table")
lapply(required.packages, require, character.only = T)

setwd("G:/My Drive/Work/GitHub/gha_report_2021/")

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

crs <- load_crs(path = "datasets/CRS/")

keep <- c(
  "CrsID",
  "Year",
  "FlowName",
  "DonorName",
  "RecipientName",
  "USD_Disbursement_Defl",
  "PurposeName",
  "ProjectTitle",
  "ShortDescription",
  "LongDescription",
  "DRR"
)

crs <- crs[, ..keep]
crs <- crs[
  FlowName == "ODA Loans" 
  |
    FlowName == "ODA Grants"
  | 
    FlowName == "Equity Investment"
  | 
    FlowName == "Private Development Finance"
  ]

crs <- crs[Year >= 2019]

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
  #"Disaster Risk Reduction"
  
)

crs$relevance <- "None"
crs[grepl(paste(major.keywords, collapse = "|"), tolower(crs$LongDescription))]$relevance <- "Minor"
crs[grepl(paste(major.keywords, collapse = "|"), tolower(paste(crs$ShortDescription, crs$ProjectTitle)))]$relevance <- "Major"

crs$check <- "No"
crs[relevance == "Minor"]$check <- "potential false positive"
crs[relevance != "None"][PurposeName %in% disqualifying.sectors]$check <- "potential false negative"
crs[relevance != "None"][grepl(paste(disqualifying.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$ProjectTitle, crs[relevance != "None"]$ShortDescription, crs[relevance != "None"]$LongDescription)))]$check <- "potential false negative"

crs[relevance != "None"][grepl(paste(disqualifying.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$ProjectTitle, crs[relevance != "None"]$ShortDescription, crs[relevance != "None"]$LongDescription)))]$relevance <- "None"
crs[relevance != "None"][PurposeName %in% disqualifying.sectors]$relevance <- "None"

crs$DRR <- as.character(crs$DRR)
crs[is.na(DRR)]$DRR <- "0"
crs[DRR != "1" & DRR != "2"]$DRR <- "No DRR component"
crs[DRR == "1"]$DRR <- "Minor DRR component"
crs[DRR == "2"]$DRR <- "Major DRR component"

crs_drr <- crs[!is.na(USD_Disbursement_Defl) & ((relevance != "None" & check == "No") | DRR != "No DRR component")]

fwrite(crs_drr, )