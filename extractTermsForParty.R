#using "most important issue" and "party voted for" questions
#to generate extreme term use or lack of use by party voted for

if(!require(haven)){
  install.packages("haven")
  require(haven)
}
if(!require(tidyr)){
  install.packages("tidyr")
  require(tidyr)
}
if(!require(ggplot2)){
  install.packages("ggplot2")
  require(ggplot2)
}
if(!require(dplyr)){
  install.packages("dplyr")
  require(dplyr)
}
if(!require(knitr)){
  install.packages("knitr")
  require(knitr)
}

applyValLabels <- function(column){
  knownvalues <- attributes(column)$labels
  valuelabels <- names(knownvalues)
  lookup <- data.frame(column = as.character(knownvalues), valuelabels, stringsAsFactors = FALSE)
  target <- data.frame(column = as.character(column), index=1:length(column), stringsAsFactors = FALSE)
  combined <- merge(target, lookup, all.x=TRUE)
  combined <- combined[order(combined$index),]
  output <- as.character(column)
  output[!(is.na(combined$valuelabels))] <- combined$valuelabels[!(is.na(combined$valuelabels))]
  return(output)
}

#load in the spss file from whereever it happens to be saved
ds <- read_spss("~/Desktop/nzes2014/NZES2014GeneralReleaseApril16.sav")
ds2 <- ds[, c("dimpissue", "dpartyvote")]
ds2$dpartyvote <- applyValLabels(ds2$dpartyvote)
ds2$dimpissue <- tolower(as.character(ds2$dimpissue))

ds2$dpartyvote[ds2$dpartyvote == "M?ori Party"] <- "Māori_Party"
ds2$dpartyvote[ds2$dpartyvote == "Internet?Mana Party"] <- "Internet_Mana_Party"
ds2$dpartyvote[ds2$dpartyvote == "No Vote"] <- "No_Vote"
ds2$dpartyvote[ds2$dpartyvote == "NZ First"] <- "NZ_First"
ds2$dpartyvote[ds2$dpartyvote == "United Future"] <- "United_Future"
ds2$dpartyvote[ds2$dpartyvote == "Another party"] <- "Another_party"
ds2$dpartyvote[ds2$dpartyvote == "Democrats for Social Credit"] <- "Democrats_for_Social_Credit"
ds2$dpartyvote[ds2$dpartyvote == "Ban 1080"] <- "Ban_1080"
ds2$dpartyvote[ds2$dpartyvote == "Don't know"] <- "Do_not_know"
ds2$dpartyvote[is.na(ds2$dpartyvote)] <- "No_Vote"

ds2$dimpissue <- gsub("don't know", "don't_know", ds2$dimpissue)
ds2$dimpissue <- gsub("dirty politics", "dirty_politics", ds2$dimpissue)
ds2$dimpissue <- gsub("\\.", "", ds2$dimpissue)
ds2$dimpissue <- gsub(",", "", ds2$dimpissue)


aggByParty <- ds2 %>% group_by(dpartyvote) %>% 
  summarise(text=paste(dimpissue, collapse=" ")) %>%
  filter(dpartyvote %in% c("Māori_Party", "Internet_Mana_Party", "No_Vote", "NZ_First", "Labour", "National", "Green", "Conservative"))


sepText <- function(x){
  words <- unlist(strsplit(x[2], split=" "))
  terms <- data.frame(words)
  terms$party <- x[1]
  return(terms)
}

disaggregated <- do.call(rbind, apply(aggByParty,1,sepText))
disaggregated$words <- gsub("&", "and", disaggregated$words)

unwanted <- c("", "and", "or", "-", "the", "of", "i", "to", "in", "our", "no", "on", "a",
              "there", "are", "for", "not", "that")
disaggregated <- disaggregated[!(disaggregated$words %in% unwanted),]

partywords <- disaggregated %>% group_by(party) %>% mutate(partycount = n()) %>%
  group_by(party, words) %>% summarise(wordfreq = 100* n()/ mean(partycount)) %>%
  spread(party,wordfreq, fill=0)
meanuse <- apply(partywords[,2:9],1,mean)
for(i in 2:9){
  partywords[,i] <- partywords[,i] - meanuse
}

partywords %>% gather(party, worddiff, Conservative:NZ_First) %>%
  mutate(diffword = abs(worddiff)) %>% arrange(desc(diffword)) %>%
  slice(1:100) %>% mutate(wsign=ifelse(worddiff < 0, "-", "+"), output = paste(wsign,words,sep="")) %>%
  group_by(party) %>% summarise(terms = paste(output, collapse=", ")) %>% 
  mutate(result = paste(party, terms, sep=":")) %>% select(result) %>%
  write.table(file="terms.txt", row.names=FALSE, col.names=FALSE, quote=FALSE)
