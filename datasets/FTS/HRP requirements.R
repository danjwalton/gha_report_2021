required.packages <- c("data.table","jsonlite","httr","XML")
lapply(required.packages, require, character.only=T)

wd <- "G:/My Drive/Work/GitHub/gha_report_2021/"
setwd(wd)

base.url <- "https://fts.unocha.org/appeals/overview/2020"

data <- htmlParse(GET(base.url))

plans <- data.table(Plan = xpathSApply(data, "//td/a", xmlValue), base.link = xpathSApply(data, "//td/a", xmlAttrs))
plans <- plans[grepl("appeals", base.link)]
plans[, base.link := gsub("summary", "", base.link)]

out <- list()
for(i in 1:nrow(plans)){
  plan.name <- plans$Plan[i]
  message(plan.name)
  planlink <- paste0('https://fts.unocha.org/', plans$base.link[i], "summary")
  data <- htmlParse(GET(planlink))
  tables <- readHTMLTable(xpathSApply(data, "//div[@class='funding-progress-bar']", xmlGetAttr, "data-content"))
  names.tables <- xpathSApply(data, "//div[@class='funding-info']", xmlValue)
  
  if(any(grepl("COVID-19", names.tables))){
    covid <- data.table(transpose(tables[grepl(" COVID-19",names.tables)][[1]]))
    non.covid <- data.table(transpose(tables[grepl("-COVID-19",names.tables)][[1]]))
    names(covid) <- paste0("COVID.",unlist(covid[1]))
    names(non.covid) <- paste0("Non-COVID.",unlist(non.covid[1]))
  } else {
    if(grepl("COVID", plan.name)){
      covid <- data.table(transpose(tables[[1]]))
      names(covid) <- paste0("COVID.",unlist(covid[1]))
      non.covid <- NULL
    } else {
      non.covid <- data.table(transpose(tables[[1]]))
      names(non.covid) <- paste0("Non-COVID.",unlist(non.covid[1]))
      covid <- NULL
    }
  }
  
  covid <- covid[-1]
  non.covid <- non.covid[-1]
  
  out[[i]] <- cbind(plan.name = plan.name, covid, non.covid)
}

agg <- rbindlist(out, fill = T)

agg <- agg[,lapply(.SD, function(x) gsub("US[$]| COVID?.*|,| Flash?.*| Intersectoral?.*| 2019?.*","", x))]
agg$plan.name <- gsub("Cabo Delgado Province ", "", agg$plan.name)
agg$plan.name <- gsub("DPR Korea", "DPRK", agg$plan.name)

agg <- agg[, lapply(.SD, function(x) sum(as.numeric(x),na.rm=T)), .SDcols = names(agg)[-1], by = plan.name]
fwrite(agg, "datasets/FTS/HRP requirements_RRP_sep.csv")
