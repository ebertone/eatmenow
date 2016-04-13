
getwd()
setwd("C:/Users/ReginaAparecidaBerto/Desktop/Ernst/timesup/Expiration data")

library(XML)




######## getting index pages #########

###first getting links for first pages###

cat = htmlParse('http://www.stilltasty.com/')

numcat = length(getNodeSet(cat, "//td[@class='categoryName']/a"))

getcatlink = function(i) {

catlink = getNodeSet(cat, "//td[@class='categoryName']/a")[[i]]

paste0("http://www.stilltasty.com/", xmlAttrs(catlink) )

}

catfirstpg = sapply(1:numcat, getcatlink)



### now getting pages 2, 3 ....


allinks = function(u) {

tasty = htmlParse(u)

numpage = length(getNodeSet(tasty, "//span/a[contains(@href, 'index')]")) + 1

link = function(i) { paste0(u, "/page:", i )}

sapply(1:numpage, link)

}

index = sapply(catfirstpg, allinks)

indexao = unlist(index)





######## getting links for each product from index pages ######


rustix = function(u) {

tasty = htmlParse(u)

numprod = length(getNodeSet(tasty, "//li/a"))

salop = function(i) {

cabron = getNodeSet(tasty, "//li/a")[[i]]

xmlAttrs(cabron)

}

sapply(1:numprod, salop)

}


xupalist = unlist(sapply(indexao, rustix))

#xupa = as.data.frame(unlist(sapply(indexao, rustix)))


###### getting info for each product

bitch = function(u) {
  
  pu = htmlParse(u)
  
  #getNodeSet(pu, "//strong") #getting names
  
  name = xmlValue(getNodeSet(pu, "//strong")[[1]])
  
  
  #getNodeSet(pu, "//tr/td/span") #getting storage modes
  
  numsto = length(getNodeSet(pu, "//tr/td/span")) 
  
  storage = sapply(1:numsto, function(i) xmlValue(getNodeSet(pu, "//tr/td/span")[[i]])[[1]])
  
  
  #getNodeSet(pu, "//tr/td[@class = 'days']") #getting expiration dates
  
  exp = sapply(1:numsto, function(i) xmlValue(getNodeSet(pu, "//tr/td[@class = 'days']")[[i]])[[1]])
  
  
  pantry = ifelse(storage[1]=="Pantry", exp[1], ifelse(storage[2]=="Pantry", exp[2], ifelse(storage[3]=="Pantry", exp[3], "NA")))
  
  refri = ifelse(storage[1]=="Refrigerator", exp[1], ifelse(storage[2]=="Refrigerator", exp[2], ifelse(storage[3]=="Refrigerator", exp[3], "NA")))
  
  freez = ifelse(storage[1]=="Freezer", exp[1], ifelse(storage[2]=="Freezer", exp[2], ifelse(storage[3]=="Freezer", exp[3], "NA")))
  
  c(name, pantry, refri, freez)
  
  
}

#finally = as.data.frame(t(sapply(xupalist[1:10], bitch)))

finally = as.data.frame(t(sapply(xupalist, bitch)))

names(finally) = c("prod_name", "pantry", "refrigerator", "freezer")





# use strsplit to obtain status of each product

strsplit(as.character(finally$prod_name), " . ") 

totalen = length(strsplit(as.character(finally$prod_name), " . ") )

#getting number of split cells (spli by dash)

splen = function(i) {
  
  length(strsplit(as.character(finally$prod_name), " . ")[[i]] )
  
}

finally$splitlen = sapply(1:totalen, splen)


#now using length to get status (last split cell)

funstatus = function(i) {
  
  len = finally$splitlen[[i]]
  
  ifelse(len==0 | len==1, "NA", strsplit(as.character(finally$prod_name), " . ")[[i]][len] )
  
}

finally$status = sapply(1:totalen, funstatus)


#obtaining new product names (without status)

#gsub(" . ", "", gsub(finally$status[1], "", finally$prod_name[1]))


newp = function(i) {

  gsub(" . ", "", gsub(finally$status[i], "", finally$prod_name[i]))

}

finally$new_prod = sapply(1:totalen, newp)



#getting raw product names (first names)

rawp = function(i) {
  
  strsplit(as.character(finally$new_prod), ",")[[i]][1]  
}

finally$raw_prod = sapply(1:totalen, rawp)



#getting all status per prod names

finally$new_prod[finally$new_prod==finally$new_prod[2]]

finally$status[finally$new_prod==finally$new_prod[2]]

table(finally$new_prod[finally$new_prod==finally$new_prod[2]], finally$status[finally$new_prod==finally$new_prod[2]]) 

colnames(table(finally$new_prod[finally$new_prod==finally$new_prod[2]], finally$status[finally$new_prod==finally$new_prod[2]]))

# use table + sapply



###

table(finally$new_prod) # need to clean up repeated names... also deal w/ commercialy, homemade...

table(finally$status) #there is a lot of diffeerent status. need to clean up


#spliting max and min days

strsplit(as.character(finally$pantry), "-") #working !!!!

#SAVING FILE

write.csv(finally, file = "rustix.csv")


################################################################################

############################################ old stuff using reg ex ########


#get html data

catcher = readLines("C:/Users/ReginaAparecidaBerto/Desktop/Ernst/timesup/Expiration data/16383.htm") #data contains all 3 colors of arrows

newcatcher = readLines("C:/Users/ReginaAparecidaBerto/Desktop/Ernst/timesup/Expiration data/16392.htm") #data contains green and blue arrow


#finding name

catcher[grepl("bigBlackHeading", catcher)] #then use "strong"


#finding days

catcher[grepl("days", catcher)]


#finding red arrows(pantry)


catcher[grepl("redArrow", catcher)] #yes

sum(as.numeric(grepl("days", catcher))) #yes

newcatcher[grepl("redArrow", newcatcher)] #doesnt work for this one because doesnt have any red arrow on it


#for name: str split by "bigBlackheading", then "strong"

#for values: maybe use arrows?



#fuzzy matching: agrep !!!