required.packages <- c("data.table", "readxl")
lapply(required.packages, require, character.only = T)

setwd("G:/My Drive/Work/GitHub/gha_report_2021/")

countrynames <- fread("datasets/Countrynames/isos.csv", encoding = "UTF-8")

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

crs_raw <- load_crs(path = "datasets/CRS/")

keep <- c(
  "CrsID",
  "Year",
  "FlowName",
  "Bi_Multi",
  "DonorName",
  "RecipientName",
  "USD_Disbursement_Defl",
  "SectorCode",
  "PurposeCode"
)

crs <- crs_raw[, ..keep]
crs <- crs[
  FlowName == "ODA Loans" 
  |
    FlowName == "ODA Grants"
  | 
    FlowName == "Equity Investment"
  #| 
  #  FlowName == "Private Development Finance"
]

crs <- crs[Year >= 2010]

crs[, Humanitarian := "No"]
crs[grepl("^7", SectorCode), Humanitarian := "Yes"] #All sectors codes which begin with '7' (i.e. HA)

mdbs <- c(
  #RDBs
  "African Development Bank"
  ,"African Development Fund"
  ,"Asian Development Bank"
  ,"Asian Infrastructure Investment Bank"
  ,"Caribbean Development Bank"
  ,"Council of Europe Development Bank"
  ,"Development Bank of Latin America"
  ,"Inter-American Development Bank"
  ,"Islamic Development Bank"
  ,"European Bank for Reconstruction and Development"
  ,"International Investment Bank"
  ,"IDB Invest"
  
  #WB
  ,"International Development Association"
  ,"International Bank for Reconstruction and Development"
  ,"International Finance Corporation"
  
  #IMF
  #,"IMF (Concessional Trust Funds)"
  
  #Other IFIs
  #,"Arab Bank for Economic Development in Africa"
  #,"Arab Fund (AFESD)"
  #,"OPEC Fund for International Development"
  #,"Nordic Development Fund"
  )

crs <- merge(crs, countrynames[, c("iso3", "countryname_oecd")], by.x = "RecipientName", by.y = "countryname_oecd", all.x = T)

pcc <- as.data.table(read_excel("datasets/PCC/Protracted Response, 2000-2021.xlsx", sheet = "Calcs."))
pcc <- pcc[, -c(3:24)]
pcc <- merge(pcc, countrynames[, c("iso2", "iso3")], by.x = "Country ID", by.y = "iso2", all.x = T)

pcc <- melt(pcc, id.vars = c("Country ID", "iso3", "Country Name", "Protracted Crisis in 2020", "Recurrent Crisis in 2020"))
pcc[is.na(value), value := 0]

pcc[, variable := as.numeric(gsub("[...].*", "", variable))][, `:=` (`Country Name` = NULL, `Country ID` = NULL)]

setnames(pcc, c("Recurrent Crisis in 2020", "Protracted Crisis in 2020", "variable", "value"), c("rec_crisis", "pro_crisis", "Year", "con_crisis"))

crs <- merge(crs, pcc, by = c("iso3", "Year"), all.x = T)
crs[is.na(pro_crisis)]$pro_crisis <- "No"
crs[is.na(rec_crisis)]$rec_crisis <- "No"
crs[is.na(con_crisis)]$con_crisis <- 0

# crs_mdb <- crs[DonorName %in% mdbs]
# crs_mdb_cast <- crs_mdb[`Protracted Crisis in 2020` == "Yes", .(disbursements = sum(USD_Disbursement_Defl, na.rm = T)), by = .(Year, `Protracted Crisis in 2020`, Humanitarian, FlowName)][order(Year, `Protracted Crisis in 2020`, Humanitarian, FlowName)]
# fwrite(crs_mdb_cast, "chapter2/MDBs HA/mdbs_ha.csv")
# 
# crs_mdb_pcc <- crs_mdb[`Protracted Crisis in 2020` == "Yes", .(disbursements = sum(USD_Disbursement_Defl, na.rm = T)), by = .(Year, DonorName, `Protracted Crisis in 2020`, FlowName)]
# fwrite(crs_mdb_pcc, "chapter2/MDBs HA/mdbs_ha_pcc.csv")

crs_ha <- crs[Humanitarian == "Yes"]

crs_ha_rank <- crs_ha[!is.na(iso3), .(total = sum(USD_Disbursement_Defl, na.rm = T)), by = .(Year, iso3, FlowName, SectorCode, RecipientName, DonorName)][, rank := frank(-total), by = Year][]
crs_ha_rank <- crs_ha_rank[rank <= 20, .(Year, iso3, RecipientName)]

crs_top20_ha <- merge(crs, crs_ha_rank, by = c("iso3", "RecipientName", "Year"))

crs_mdbs_rec <- crs_top20_ha[DonorName %in% mdbs, .(total = sum(USD_Disbursement_Defl, na.rm = T), len = length(unique(iso3))), by = .(Year, iso3, FlowName)]
crs_mdbs_don <- crs_top20_ha[DonorName %in% mdbs, .(total = sum(USD_Disbursement_Defl, na.rm = T), len = length(unique(iso3))), by = .(Year, DonorName)]

crs_crisis_ha_split <- crs[con_crisis >= 1, .(ha = sum(USD_Disbursement_Defl[Humanitarian == "Yes"], na.rm = T), oda = sum(USD_Disbursement_Defl[Humanitarian == "No"], na.rm = T), n = length(unique(iso3))), by = .(Year)]

crs_pcrisis_ha_split <- crs[con_crisis >= 5, .(ha = sum(USD_Disbursement_Defl[Humanitarian == "Yes"], na.rm = T), oda = sum(USD_Disbursement_Defl[Humanitarian == "No"], na.rm = T), n = length(unique(iso3))), by = .(Year)]

crs_crisis_ha_split_dons <- dcast(crs[con_crisis >= 1], Year + DonorName ~ Humanitarian, value.var = "USD_Disbursement_Defl", fun.aggregate = function(x) sum(x, na.rm = T))
crs_crisis_ha_split_dons[, share := Yes/(No + Yes)]

crs_crisis_ha_share_don_t <- dcast(crs_crisis_ha_split_dons, DonorName ~ Year, value.var = "share")

crs_bm <- crs[Bi_Multi %in% c(1, 8), .(loans = sum(USD_Disbursement_Defl[FlowName == "ODA Loans"], na.rm = T), grants = sum(USD_Disbursement_Defl[FlowName == "ODA Grants"], na.rm = T)), by = Year]

crs_mdbs_split <- merge(crs, crs_ha_rank, by = c("iso3", "Year"))[DonorName %in% mdbs, .(oda = sum(USD_Disbursement_Defl[Humanitarian == "No"], na.rm = T), ha = sum(USD_Disbursement_Defl[Humanitarian == "Yes"], na.rm = T)), by = Year]
fwrite(crs_mdbs_split, "chapter2/MDBs HA/mdbs_oda_ha_split_top20.csv")

crs_mdbs <- crs_top20_ha[DonorName %in% mdbs, .(total = sum(USD_Disbursement_Defl, na.rm = T), len = length(unique(iso3))), by = .(Year, FlowName)]
fwrite(crs_ha_mdbs, "chapter2/MDBs HA/mdbs_top20.csv")

crs_mdbs_total <- crs[DonorName %in% mdbs, .(total = sum(USD_Disbursement_Defl, na.rm = T), loans = sum(USD_Disbursement_Defl[FlowName == "ODA Loans"], na.rm = T), grants = sum(USD_Disbursement_Defl[FlowName == "ODA Grants"], na.rm = T)), by = .(Year)][, share_grants := grants/(loans+grants)]
fwrite(crs_mdbs_total, "chapter2/MDBs HA/mdbs_total.csv")

##Narrative calcs

#Total country-allocable ODA to crisis countries
n1 <- crs[!is.na(iso3), .(crisis_oda = sum(USD_Disbursement_Defl[con_crisis >= 1], na.rm = T), all_oda = sum(USD_Disbursement_Defl, na.rm = T)), by = Year][order(Year)][, crisis_share := crisis_oda/all_oda][]
fwrite(n1, "chapter2/MDBs HA/total_oda_to_crisis.csv")

#HA to crisis countries
n2 <- crs[con_crisis >= 1, .(ha = sum(USD_Disbursement_Defl[Humanitarian == "Yes"], na.rm = T), oda = sum(USD_Disbursement_Defl[Humanitarian == "No"], na.rm = T)), by = .(Year)][order(Year)][, ha_share := ha/(ha+oda)][]
fwrite(n2, "chapter2/MDBs HA/ha_oda_to_crisis.csv")

#Total country-allocable ODA to protracted crisis countries
n3 <- crs[!is.na(iso3), .(crisis_oda = sum(USD_Disbursement_Defl[con_crisis >= 5], na.rm = T), all_oda = sum(USD_Disbursement_Defl, na.rm = T)), by = Year][order(Year)][, crisis_share := crisis_oda/all_oda][]
fwrite(n3, "chapter2/MDBs HA/total_oda_to_proc_crisis.csv")

#HA to protracted crisis countries
n4 <- crs[con_crisis >= 5, .(ha = sum(USD_Disbursement_Defl[Humanitarian == "Yes"], na.rm = T), oda = sum(USD_Disbursement_Defl[Humanitarian == "No"], na.rm = T)), by = .(Year)][order(Year)][, ha_share := ha/(ha+oda)][]
fwrite(n4, "chapter2/MDBs HA/ha_oda_to_proc_crisis.csv")

#Total MDB funding
n5 <- crs[DonorName %in% mdbs, .(total_oda = sum(USD_Disbursement_Defl, na.rm = T)), by = Year][order(Year)]
fwrite(n5, "chapter2/MDBs HA/total_mdb.csv")

#MDB funding by grants and loans
n6 <- crs[DonorName %in% mdbs, .(grants_oda = sum(USD_Disbursement_Defl[FlowName == "ODA Grants"], na.rm = T), loans_oda = sum(USD_Disbursement_Defl[FlowName == "ODA Loans"], na.rm = T)), by = Year][order(Year)]
fwrite(n6, "chapter2/MDBs HA/mdb_grants_loans.csv")
