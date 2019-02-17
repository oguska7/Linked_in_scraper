library(rvest)
library(XML)
library(RCurl)
library(rjson)
library(rlist)
library(dplyr)
user_url <- "url to the person's page"
username <- "email"
password <- "password"
linkedin_url <- "http://linkedin.com/"
pgsession <- html_session(linkedin_url) 
pgform <- html_form(pgsession)[[1]]
filled_form <- set_values(pgform,
                          session_key = username, 
                          session_password = password)
submit_form(pgsession, filled_form)
pgsession <- jump_to(pgsession, user_url)
page_html <- read_html(pgsession)
a=htmlParse(page_html)
doc.text = unlist(xpathSApply(a, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue))
doc.text = doc.text[duplicated(doc.text) == FALSE]
doc=list()
for (i in 1:length(doc.text)){
  d=tryCatch(fromJSON(doc.text[i]), error = function(e) NA)
  doc[[i]]=tryCatch(d$included, error = function(e) NA)
  
}
d=doc[!sapply(doc, function(x) all(is.na(x)))]
droplist=c( "$deletedFields", "entityUrn", "schoolUrn", "companyUrn", "active" , "showcase","logo",  "profileId" ,     "*elements" ,      "paging"   ,  "trackingId", "objectUrn",   "$type")
result = d[[which(sapply(d, function(x) length(x)) == max(sapply(d, function(x) length(x))))]]
r=list()
for (i in (1:length(result))){
  r[[i]]=as.data.frame(list.remove(result[[i]], droplist))
}
r=r[!sapply(r, function(x) all(is.na(x)))]

## summary
index = which(sapply(r, function(x) "summary" %in% names(x))==TRUE)
summary=r[[index]][, c("lastName", "firstName", "summary" , "industryName" , "defaultLocale.country" , "locationName", "location.basicLocation.postalCode", "supportedLocales.language" , "student" ,  "location.preferredGeoPlace", "headline")]

## languages
index = which(sapply(r, function(x) "proficiency" %in% names(x))==TRUE)
languages = r[[index[1]]]
 for (i in (index[2]):(index[length(index)])){
   languages = rbind(languages, r[[i]])
 }
## schools
index = which(sapply(r, function(x) "schoolName" %in% names(x))==TRUE)
schools = r[[index[1]]]
for (i in index[2]: index[length(index)]){
  schools = bind_rows(list(schools, r[[i]]))
}
schools= schools[, c("timePeriod.endDate.year",  "timePeriod.startDate.year" , "schoolName" ,   "fieldOfStudy"  )]

## jobs
index = which(sapply(r, function(x) "companyName" %in% names(x))==TRUE)
jobs = r[[index[1]]]
for (i in index[2]: index[length(index)]){
  jobs = bind_rows(jobs, r[[i]])
}
schools= schools[, c("timePeriod.endDate.year",  "timePeriod.startDate.year" , "schoolName" ,   "fieldOfStudy"  )]

## honors and awards
index = which(sapply(r, function(x) "issuer" %in% names(x))==TRUE)
honors = r[[index[1]]]
for (i in index[2]: index[length(index)]){
  honors = bind_rows(honors, r[[i]])
}
honors = honors[, c( "title" ,  "issueDate.month" , "issueDate.year" , "description")]




