library(plyr)
library(reshape2)

# Set our WD and read in the data.
getwd()
setwd("/home/steve/Dropbox/data/usaid/2012")
EconAid <- read.csv("us_economic_constant-2012.csv", na.strings = "")
summary(EconAid)
MilitAid <- read.csv("us_military_constant-2012.csv", na.strings = "")
summary(MilitAid)

# Because USAID data has commas in the values, R will read the data as characters. Let's get rid of the commas.
col2cvt <- 3:length(EconAid)
EconAid[,col2cvt] <- lapply(EconAid[,col2cvt],function(EconAid){as.numeric(gsub(",", "", EconAid))})



# We want to sum all programs for every cross-sectional unit. This function will do that.
# The end result is a dataframe that I title EconAidTotal.
sumNA <- function(x) ifelse(all(is.na(x)), NA, sum(x, na.rm = TRUE))
EconAidTotal <- rbind(EconAid,
             ddply(within(EconAid, Program.Name <- "TOTAL"), .(Country.Name, Program.Name), 
                           numcolwise(sumNA))
             )

# This step is optional, but it will shuffle the rows and put the TOTAL as last row for the cross-sectional unit.
EconAidTotal <- EconAidTotal[order(EconAidTotal$Country.Name), ]
summary(EconAidTotal)

# The US transitioned its fiscal quarters in 1976, creating a transitional quarter. 
# This will add FY1976tq to FY1976 and drop FY1976tq.
# First, we'll need to recode 1976tq to = 0 if 1976 > 0, otherwise the resulting column will be NA.
EconAidTotal$FY1976tq[EconAidTotal$FY1976 > 0 & is.na(EconAidTotal$FY1976tq)] <- 0
EconAidTotal$FY1976 <- EconAidTotal$FY1976 + EconAidTotal$FY1976tq
EconAidTotal$FY1976tq <- NULL

# This will subset EconAidTotal and get rid of all the individual programs. We want (and have) the sum total.
# Next, we'll get rid of the Program.Name column, since we no longer need it.
EconAidTotal <- EconAidTotal[EconAidTotal$Program.Name == "TOTAL",]
EconAidTotal$Program.Name <- NULL
write.table(EconAidTotal,file="useconaidtotal.csv",sep=",",row.names=F,na="")

# Next, we will convert the data from wide format to long.
EconAidMelt <- melt(EconAidTotal, id = "Country.Name")
summary(EconAidMelt)

# This was a super messy "melt", creating "variable" and "value" for important units of fiscal year and economic aid total respectively.
# First, let's rename "value" as "econaid".
# Next, let's just create a "year" value for "variable" that also strips out the "FY" in the time unit.
# Afterward, we can drop "variable" and convert year to numeric, because it's driving me nuts for the meantime.
# Also, convert Country.Name from factor to character, otherwise we can't easily recode it.
EconAidMelt <- rename(EconAidMelt, c(value="econaid"))
EconAidMelt$year <- gsub("FY","",EconAidMelt$variable)
EconAidMelt$variable <- NULL
EconAidMelt$year <- as.numeric(EconAidMelt$year)
EconAidMelt$Country.Name <- as.character(EconAidMelt$Country.Name)


# Now, for my least favorite part. This will occur in multiple phases. First, let's get rid of observations that aren't countries.
# I'm commenting out Kosovo for the meantime, since COW does recognize it as a state.
EconAidMelt <- EconAidMelt[EconAidMelt$Country.Name != "Asia (not specified)",]
EconAidMelt <- EconAidMelt[EconAidMelt$Country.Name != "Caribbean (not specified)",]
EconAidMelt <- EconAidMelt[EconAidMelt$Country.Name != "Central America (not specified)",]
EconAidMelt <- EconAidMelt[EconAidMelt$Country.Name != "China (Hong Kong)",]
EconAidMelt <- EconAidMelt[EconAidMelt$Country.Name != "East Africa (not specified)",]
EconAidMelt <- EconAidMelt[EconAidMelt$Country.Name != "Eastern Europe (not specified)",]
EconAidMelt <- EconAidMelt[EconAidMelt$Country.Name != "Eurasia (not specified)",]
# EconAidMelt <- EconAidMelt[EconAidMelt$Country.Name != "Kosovo",]
EconAidMelt <- EconAidMelt[EconAidMelt$Country.Name != "Latin America & Caribbean (not specified)",]
EconAidMelt <- EconAidMelt[EconAidMelt$Country.Name != "Middle East & North Africa (not specified)",]
EconAidMelt <- EconAidMelt[EconAidMelt$Country.Name != "Oceania (not specified)",]
EconAidMelt <- EconAidMelt[EconAidMelt$Country.Name != "Southern Africa (not specified)",]
EconAidMelt <- EconAidMelt[EconAidMelt$Country.Name != "Sub-Saharan Africa (not specified)",]
EconAidMelt <- EconAidMelt[EconAidMelt$Country.Name != "West Africa (not specified)",]
EconAidMelt <- EconAidMelt[EconAidMelt$Country.Name != "West Bank/Gaza",]
EconAidMelt <- EconAidMelt[EconAidMelt$Country.Name != "Western Europe (not specified)",]
EconAidMelt <- EconAidMelt[EconAidMelt$Country.Name != "World (not specified)",]

# Next, we need to create some ccodes in lieu of Country.Names.
# I'm going to code Germany as 260, since it appears only after WWII in this sample.
EconAidMelt$ccode <- NA
EconAidMelt$ccode[EconAidMelt$Country.Name == "Afghanistan"] <- 700
EconAidMelt$ccode[EconAidMelt$Country.Name == "Albania"] <- 339
EconAidMelt$ccode[EconAidMelt$Country.Name == "Algeria"] <- 615
EconAidMelt$ccode[EconAidMelt$Country.Name == "Angola"] <- 540
EconAidMelt$ccode[EconAidMelt$Country.Name == "Antigua and Barbuda"] <- 58
EconAidMelt$ccode[EconAidMelt$Country.Name == "Argentina"] <- 160
EconAidMelt$ccode[EconAidMelt$Country.Name == "Armenia"] <- 371
EconAidMelt$ccode[EconAidMelt$Country.Name == "Australia"] <- 900
EconAidMelt$ccode[EconAidMelt$Country.Name == "Austria"] <- 305
EconAidMelt$ccode[EconAidMelt$Country.Name == "Azerbaijan"] <- 373
EconAidMelt$ccode[EconAidMelt$Country.Name == "Bahamas, The"] <- 31
EconAidMelt$ccode[EconAidMelt$Country.Name == "Bahrain"] <- 692
EconAidMelt$ccode[EconAidMelt$Country.Name == "Bangladesh"] <- 771
EconAidMelt$ccode[EconAidMelt$Country.Name == "Barbados"] <- 53
EconAidMelt$ccode[EconAidMelt$Country.Name == "Belarus"] <- 370
EconAidMelt$ccode[EconAidMelt$Country.Name == "Belgium"] <- 211
EconAidMelt$ccode[EconAidMelt$Country.Name == "Belize"] <- 80
EconAidMelt$ccode[EconAidMelt$Country.Name == "Benin"] <- 434
EconAidMelt$ccode[EconAidMelt$Country.Name == "Bhutan"] <- 760
EconAidMelt$ccode[EconAidMelt$Country.Name == "Bolivia"] <- 145
EconAidMelt$ccode[EconAidMelt$Country.Name == "Bosnia & Herzegovina"] <- 346
EconAidMelt$ccode[EconAidMelt$Country.Name == "Botswana"] <- 571
EconAidMelt$ccode[EconAidMelt$Country.Name == "Brazil"] <- 140
EconAidMelt$ccode[EconAidMelt$Country.Name == "Brunei"] <- 835
EconAidMelt$ccode[EconAidMelt$Country.Name == "Bulgaria"] <- 355
EconAidMelt$ccode[EconAidMelt$Country.Name == "Burkina Faso"] <- 439
EconAidMelt$ccode[EconAidMelt$Country.Name == "Burma (Myanmar)"] <- 775
EconAidMelt$ccode[EconAidMelt$Country.Name == "Burundi"] <- 516
EconAidMelt$ccode[EconAidMelt$Country.Name == "Cambodia"] <- 811
EconAidMelt$ccode[EconAidMelt$Country.Name == "Cameroon"] <- 471
EconAidMelt$ccode[EconAidMelt$Country.Name == "Canada"] <- 20
EconAidMelt$ccode[EconAidMelt$Country.Name == "Cape Verde"] <- 402
EconAidMelt$ccode[EconAidMelt$Country.Name == "Central African Republic"] <- 482
EconAidMelt$ccode[EconAidMelt$Country.Name == "Chad"] <- 483
EconAidMelt$ccode[EconAidMelt$Country.Name == "Chile"] <- 155
EconAidMelt$ccode[EconAidMelt$Country.Name == "China (P.R.C.)"] <- 710
EconAidMelt$ccode[EconAidMelt$Country.Name == "China (Taiwan)"] <- 713
EconAidMelt$ccode[EconAidMelt$Country.Name == "Colombia"] <- 100
EconAidMelt$ccode[EconAidMelt$Country.Name == "Comoros"] <- 581
EconAidMelt$ccode[EconAidMelt$Country.Name == "Congo (Brazzaville)"] <- 484
EconAidMelt$ccode[EconAidMelt$Country.Name == "Congo (Kinshasa)"] <- 490
EconAidMelt$ccode[EconAidMelt$Country.Name == "Costa Rica"] <- 94
EconAidMelt$ccode[EconAidMelt$Country.Name == "Cote d'Ivoire"] <- 437
EconAidMelt$ccode[EconAidMelt$Country.Name == "Croatia"] <- 344
EconAidMelt$ccode[EconAidMelt$Country.Name == "Cuba"] <- 40
EconAidMelt$ccode[EconAidMelt$Country.Name == "Cyprus"] <- 352
EconAidMelt$ccode[EconAidMelt$Country.Name == "Czech Republic"] <- 316
EconAidMelt$ccode[EconAidMelt$Country.Name == "Denmark"] <- 390
EconAidMelt$ccode[EconAidMelt$Country.Name == "Djibouti"] <- 522
EconAidMelt$ccode[EconAidMelt$Country.Name == "Dominica"] <- 54
EconAidMelt$ccode[EconAidMelt$Country.Name == "Dominican Republic"] <- 42
EconAidMelt$ccode[EconAidMelt$Country.Name == "Ecuador"] <- 130
EconAidMelt$ccode[EconAidMelt$Country.Name == "Egypt"] <- 651
EconAidMelt$ccode[EconAidMelt$Country.Name == "El Salvador"] <- 92
EconAidMelt$ccode[EconAidMelt$Country.Name == "Equatorial Guinea"] <- 411
EconAidMelt$ccode[EconAidMelt$Country.Name == "Eritrea"] <- 531
EconAidMelt$ccode[EconAidMelt$Country.Name == "Estonia"] <- 366
EconAidMelt$ccode[EconAidMelt$Country.Name == "Ethiopia"] <- 530
EconAidMelt$ccode[EconAidMelt$Country.Name == "Fiji"] <- 950
EconAidMelt$ccode[EconAidMelt$Country.Name == "Finland"] <- 375
EconAidMelt$ccode[EconAidMelt$Country.Name == "France"] <- 220
EconAidMelt$ccode[EconAidMelt$Country.Name == "Gabon"] <- 481
EconAidMelt$ccode[EconAidMelt$Country.Name == "Gambia, The"] <- 420
EconAidMelt$ccode[EconAidMelt$Country.Name == "Georgia"] <- 372
EconAidMelt$ccode[EconAidMelt$Country.Name == "Germany"] <- 260
EconAidMelt$ccode[EconAidMelt$Country.Name == "Ghana"] <- 452
EconAidMelt$ccode[EconAidMelt$Country.Name == "Greece"] <- 350
EconAidMelt$ccode[EconAidMelt$Country.Name == "Grenada"] <- 55
EconAidMelt$ccode[EconAidMelt$Country.Name == "Guatemala"] <- 90
EconAidMelt$ccode[EconAidMelt$Country.Name == "Guinea"] <- 438
EconAidMelt$ccode[EconAidMelt$Country.Name == "Guinea-Bissau"] <- 404
EconAidMelt$ccode[EconAidMelt$Country.Name == "Guyana"] <- 110
EconAidMelt$ccode[EconAidMelt$Country.Name == "Haiti"] <- 41
EconAidMelt$ccode[EconAidMelt$Country.Name == "Honduras"] <- 91
EconAidMelt$ccode[EconAidMelt$Country.Name == "Hungary"] <- 310
EconAidMelt$ccode[EconAidMelt$Country.Name == "Iceland"] <- 395
EconAidMelt$ccode[EconAidMelt$Country.Name == "India"] <- 750
EconAidMelt$ccode[EconAidMelt$Country.Name == "Iceland"] <- 395
EconAidMelt$ccode[EconAidMelt$Country.Name == "Indonesia"] <- 850
EconAidMelt$ccode[EconAidMelt$Country.Name == "Iran"] <- 630
EconAidMelt$ccode[EconAidMelt$Country.Name == "Iraq"] <- 645
EconAidMelt$ccode[EconAidMelt$Country.Name == "Ireland"] <- 205
EconAidMelt$ccode[EconAidMelt$Country.Name == "Israel"] <- 666
EconAidMelt$ccode[EconAidMelt$Country.Name == "Italy"] <- 325
EconAidMelt$ccode[EconAidMelt$Country.Name == "Jamaica"] <- 51
EconAidMelt$ccode[EconAidMelt$Country.Name == "Japan"] <- 740
EconAidMelt$ccode[EconAidMelt$Country.Name == "Jordan"] <- 663
EconAidMelt$ccode[EconAidMelt$Country.Name == "Kazakhstan"] <- 705
EconAidMelt$ccode[EconAidMelt$Country.Name == "Kenya"] <- 501
EconAidMelt$ccode[EconAidMelt$Country.Name == "Kiribati"] <- 946
EconAidMelt$ccode[EconAidMelt$Country.Name == "Korea, North"] <- 731
EconAidMelt$ccode[EconAidMelt$Country.Name == "Korea, South"] <- 732
EconAidMelt$ccode[EconAidMelt$Country.Name == "Kosovo"] <- 347
EconAidMelt$ccode[EconAidMelt$Country.Name == "Kuwait"] <- 690
EconAidMelt$ccode[EconAidMelt$Country.Name == "Kyrgyzstan"] <- 703
EconAidMelt$ccode[EconAidMelt$Country.Name == "Laos"] <- 812
EconAidMelt$ccode[EconAidMelt$Country.Name == "Latvia"] <- 367
EconAidMelt$ccode[EconAidMelt$Country.Name == "Lebanon"] <- 660
EconAidMelt$ccode[EconAidMelt$Country.Name == "Lesotho"] <- 570
EconAidMelt$ccode[EconAidMelt$Country.Name == "Liberia"] <- 450
EconAidMelt$ccode[EconAidMelt$Country.Name == "Libya"] <- 620
EconAidMelt$ccode[EconAidMelt$Country.Name == "Lithuania"] <- 368
EconAidMelt$ccode[EconAidMelt$Country.Name == "Macedonia, Former Yugoslav Republic"] <- 343
EconAidMelt$ccode[EconAidMelt$Country.Name == "Madagascar"] <- 580
EconAidMelt$ccode[EconAidMelt$Country.Name == "Malawi"] <- 553
EconAidMelt$ccode[EconAidMelt$Country.Name == "Malaysia"] <- 820
EconAidMelt$ccode[EconAidMelt$Country.Name == "Maldives"] <- 781
EconAidMelt$ccode[EconAidMelt$Country.Name == "Mali"] <- 432
EconAidMelt$ccode[EconAidMelt$Country.Name == "Malta"] <- 338
EconAidMelt$ccode[EconAidMelt$Country.Name == "Marshall Islands"] <- 983
EconAidMelt$ccode[EconAidMelt$Country.Name == "Mauritania"] <- 435
EconAidMelt$ccode[EconAidMelt$Country.Name == "Mauritius"] <- 590
EconAidMelt$ccode[EconAidMelt$Country.Name == "Mexico"] <- 70
EconAidMelt$ccode[EconAidMelt$Country.Name == "Micronesia (Federated States of)"] <- 987
EconAidMelt$ccode[EconAidMelt$Country.Name == "Moldova"] <- 359
EconAidMelt$ccode[EconAidMelt$Country.Name == "Mongolia"] <- 712
EconAidMelt$ccode[EconAidMelt$Country.Name == "Montenegro"] <- 341
EconAidMelt$ccode[EconAidMelt$Country.Name == "Morocco"] <- 600
EconAidMelt$ccode[EconAidMelt$Country.Name == "Mozambique"] <- 541
EconAidMelt$ccode[EconAidMelt$Country.Name == "Namibia"] <- 565
EconAidMelt$ccode[EconAidMelt$Country.Name == "Nepal"] <- 790
EconAidMelt$ccode[EconAidMelt$Country.Name == "Netherlands"] <- 210
EconAidMelt$ccode[EconAidMelt$Country.Name == "New Zealand"] <- 920
EconAidMelt$ccode[EconAidMelt$Country.Name == "Nicaragua"] <- 93
EconAidMelt$ccode[EconAidMelt$Country.Name == "Niger"] <- 436
EconAidMelt$ccode[EconAidMelt$Country.Name == "Nigeria"] <- 475
EconAidMelt$ccode[EconAidMelt$Country.Name == "Norway"] <- 385
EconAidMelt$ccode[EconAidMelt$Country.Name == "Oman"] <- 698
EconAidMelt$ccode[EconAidMelt$Country.Name == "Pakistan"] <- 770
EconAidMelt$ccode[EconAidMelt$Country.Name == "Palau"] <- 986
EconAidMelt$ccode[EconAidMelt$Country.Name == "Panama"] <- 95
EconAidMelt$ccode[EconAidMelt$Country.Name == "Papua New Guinea"] <- 910
EconAidMelt$ccode[EconAidMelt$Country.Name == "Paraguay"] <- 150
EconAidMelt$ccode[EconAidMelt$Country.Name == "Peru"] <- 135
EconAidMelt$ccode[EconAidMelt$Country.Name == "Philippines"] <- 840
EconAidMelt$ccode[EconAidMelt$Country.Name == "Poland"] <- 290
EconAidMelt$ccode[EconAidMelt$Country.Name == "Portugal"] <- 235
EconAidMelt$ccode[EconAidMelt$Country.Name == "Qatar"] <- 694
EconAidMelt$ccode[EconAidMelt$Country.Name == "Romania"] <- 360
EconAidMelt$ccode[EconAidMelt$Country.Name == "Russia"] <- 365
EconAidMelt$ccode[EconAidMelt$Country.Name == "Rwanda"] <- 517
EconAidMelt$ccode[EconAidMelt$Country.Name == "Samoa"] <- 990
EconAidMelt$ccode[EconAidMelt$Country.Name == "Sao Tome & Principe"] <- 403
EconAidMelt$ccode[EconAidMelt$Country.Name == "Saudi Arabia"] <- 670
EconAidMelt$ccode[EconAidMelt$Country.Name == "Senegal"] <- 433
EconAidMelt$ccode[EconAidMelt$Country.Name == "Serbia"] <- 345
EconAidMelt$ccode[EconAidMelt$Country.Name == "Serbia and Montenegro, Former"] <- 345
EconAidMelt$ccode[EconAidMelt$Country.Name == "Seychelles"] <- 591
EconAidMelt$ccode[EconAidMelt$Country.Name == "Sierra Leone"] <- 451
EconAidMelt$ccode[EconAidMelt$Country.Name == "Singapore"] <- 830
EconAidMelt$ccode[EconAidMelt$Country.Name == "Slovakia"] <- 317
EconAidMelt$ccode[EconAidMelt$Country.Name == "Slovenia"] <- 349
EconAidMelt$ccode[EconAidMelt$Country.Name == "Solomon Islands"] <- 940
EconAidMelt$ccode[EconAidMelt$Country.Name == "Somalia"] <- 520
EconAidMelt$ccode[EconAidMelt$Country.Name == "South Africa"] <- 560
EconAidMelt$ccode[EconAidMelt$Country.Name == "South Sudan"] <- 626
EconAidMelt$ccode[EconAidMelt$Country.Name == "Spain"] <- 230
EconAidMelt$ccode[EconAidMelt$Country.Name == "Sri Lanka"] <- 780
EconAidMelt$ccode[EconAidMelt$Country.Name == "St. Lucia"] <- 56
EconAidMelt$ccode[EconAidMelt$Country.Name == "St. Vincent and Grenadines"] <- 57
EconAidMelt$ccode[EconAidMelt$Country.Name == "St. Kitts and Nevis"] <- 60
EconAidMelt$ccode[EconAidMelt$Country.Name == "Sudan"] <- 625
EconAidMelt$ccode[EconAidMelt$Country.Name == "Suriname"] <- 115
EconAidMelt$ccode[EconAidMelt$Country.Name == "Swaziland"] <- 572
EconAidMelt$ccode[EconAidMelt$Country.Name == "Sweden"] <- 380
EconAidMelt$ccode[EconAidMelt$Country.Name == "Switzerland"] <- 225
EconAidMelt$ccode[EconAidMelt$Country.Name == "Syria"] <- 652
EconAidMelt$ccode[EconAidMelt$Country.Name == "Tajikistan"] <- 702
EconAidMelt$ccode[EconAidMelt$Country.Name == "Tanzania"] <- 510
EconAidMelt$ccode[EconAidMelt$Country.Name == "Thailand"] <- 800
EconAidMelt$ccode[EconAidMelt$Country.Name == "Timor-Leste"] <- 860
EconAidMelt$ccode[EconAidMelt$Country.Name == "Togo"] <- 461
EconAidMelt$ccode[EconAidMelt$Country.Name == "Tonga"] <- 955
EconAidMelt$ccode[EconAidMelt$Country.Name == "Trinidad & Tobago"] <- 52
EconAidMelt$ccode[EconAidMelt$Country.Name == "Tunisia"] <- 616
EconAidMelt$ccode[EconAidMelt$Country.Name == "Turkey"] <- 640
EconAidMelt$ccode[EconAidMelt$Country.Name == "Turkmenistan"] <- 701
EconAidMelt$ccode[EconAidMelt$Country.Name == "Uganda"] <- 500
EconAidMelt$ccode[EconAidMelt$Country.Name == "Ukraine"] <- 369
EconAidMelt$ccode[EconAidMelt$Country.Name == "United Arab Emirates"] <- 696
EconAidMelt$ccode[EconAidMelt$Country.Name == "United Kingdom"] <- 200
EconAidMelt$ccode[EconAidMelt$Country.Name == "Uruguay"] <- 165
EconAidMelt$ccode[EconAidMelt$Country.Name == "Uzbekistan"] <- 704
EconAidMelt$ccode[EconAidMelt$Country.Name == "Vanuatu"] <- 935
EconAidMelt$ccode[EconAidMelt$Country.Name == "Venezuela"] <- 101
EconAidMelt$ccode[EconAidMelt$Country.Name == "Vietnam"] <- 816
EconAidMelt$ccode[EconAidMelt$Country.Name == "Yemen"] <- 679
EconAidMelt$ccode[EconAidMelt$Country.Name == "Zambia"] <- 551
EconAidMelt$ccode[EconAidMelt$Country.Name == "Zanzibar"] <- 511
EconAidMelt$ccode[EconAidMelt$Country.Name == "Zimbabwe"] <- 552

# We need to do multiple things in this section.
# First, let's isolate South Vietnam from Vietnam.
# Then, let's unify the Serbias in the Greenbook data.
# We should also want to separate Yemen from Yemen Arab Republic before 1990.
# We will also need to separate China from Taiwan before the revolution.
# We should also create blank frames for E Germany and Czechoslovakia and add them.
# We also need to account for the short-lived United Arab Republic between 1958-61 (Egypt + Syria).


# Isolating South Vietnam from Vietnam is going to be a little tricky, but it needs to accomplish the following.
# First, South Vietnam, not Vietnam, was receiving USAID before 1976. But, Vietnam was eligible, and not receiving it. We need to account for this.
# We will recode Vietnam as South Vietnam for all observations before 1976. Saigon fell in 1975, though S. Vietnam has an aid commitment for FY1976.

EconAidMelt$ccode[EconAidMelt$Country.Name == "Vietnam" & EconAidMelt$year < 1976] <- 817
EconAidMelt[EconAidMelt[, "Country.Name"] == "Vietnam" & EconAidMelt[, "year"] < 1976 , "Country.Name"] <- "South Vietnam"

Viet5475 <- data.frame(year=seq(1954,1975),econaid=NA,Country.Name="Vietnam",ccode=816)
summary(Viet5475)
EconAidMelt = rbind(EconAidMelt,Viet5475)

# Let's take care of Taiwan next. All USAID to Taiwan before 1950 should be given to China (PRC). We will delete China's observations before 1950 in order to do this.
EconAidMelt <- subset(EconAidMelt, !( ccode == 710 & year < 1950))
EconAidMelt$ccode[EconAidMelt$ccode == 713 & EconAidMelt$year < 1950] <- 710
EconAidMelt[EconAidMelt[, "Country.Name"] == "China (Taiwan)" & EconAidMelt[, "year"] < 1950 , "Country.Name"] <- "China (P.R.C.)"

# Let's handle Yemen now. First, let's code Yemen as starting in 1990. Before 1990, Yemen was YAR.
# Then, we recode Yemen's country.name to reflect this.
# Then, create an empty data frame for YPR and add it.
EconAidMelt$ccode[EconAidMelt$ccode == 679 & EconAidMelt$year < 1990] <- 678
EconAidMelt[EconAidMelt[, "Country.Name"] == "Yemen" & EconAidMelt[, "year"] < 1990 , "Country.Name"] <- "Yemen Arab Republic"

YPR6789 <- data.frame(year=seq(1967,1989),econaid=NA,Country.Name="Yemen People's Republic",ccode=680)
EconAidMelt = rbind(EconAidMelt,YPR6789)

# Add East Germany.

GDR5490 <- data.frame(year=seq(1954,1990),econaid=NA,Country.Name="East Germany",ccode=265)
EconAidMelt = rbind(EconAidMelt,GDR5490)

# Add Czechoslovakia

CZE4692 <- data.frame(year=seq(1946,1992),econaid=NA,Country.Name="Czechoslovakia",ccode=315)
EconAidMelt = rbind(EconAidMelt,CZE4692)

# There are a few weird things happening in the Greenbook data for Serbia.
# Greenbook lists "Serbia and Montenegro, Former" as Serbia/Yugoslavia through 2006.
# Serbia and Montenegro split in (FY)2006, but Greenbook lists separate aid commitments for all three entities.
# So here's what we'll do: add Serbia to "Serbia and Montenegro, Former" in 2006 and treat Montenegro as its own entity in FY2006. Montenegro was a full state for half of that fiscal year.
# Then, we'll drop the "Serbia" observations before 2006 and drop the "Serbia and Montengro, Former" observations after 2006.


Serbia2006 <- data.frame(year=2006,econaid=sum(EconAidMelt$econaid[(EconAidMelt$year == 2006) & (EconAidMelt$ccode == 345)]),Country.Name="Serbia 2006 (adjusted)",ccode=345)
EconAidMelt = rbind(EconAidMelt,Serbia2006)

EconAidMelt <- subset(EconAidMelt, !( Country.Name == "Serbia" & year < 2007))
EconAidMelt <- subset(EconAidMelt, !( Country.Name == "Serbia and Montenegro, Former" & year > 2005))

# Now that we have deleted all extraneous "Serbia" observations before 2006, and all "Serbia and Montenegro, Former" observations after 2005, let's standardize the country names.

EconAidMelt$Country.Name[EconAidMelt$Country.Name == "Serbia and Montenegro, Former"] <- "Yugoslavia/Serbia"
EconAidMelt$Country.Name[EconAidMelt$Country.Name == "Serbia"] <- "Yugoslavia/Serbia"
EconAidMelt$Country.Name[EconAidMelt$Country.Name == "Serbia 2006 (adjusted)"] <- "Yugoslavia/Serbia"


# We need to account for the United Arab Republic, which Greenbook does not do.
# Let's create a data.frame combining Egypt and Syria for each year between 1958 and 1961. So: 1959 and 1960.
# Then, let's drop Egypt and Syria for those years and rbind our data.frames.

UAR58 <- data.frame(year=1958,econaid=sum(EconAidMelt$econaid[(EconAidMelt$year == 1958) & (EconAidMelt$ccode == 651 | EconAidMelt$ccode == 652)]),Country.Name="United Arab Republic",ccode=651)
UAR59 <- data.frame(year=1959,econaid=sum(EconAidMelt$econaid[(EconAidMelt$year == 1959) & (EconAidMelt$ccode == 651 | EconAidMelt$ccode == 652)]),Country.Name="United Arab Republic",ccode=651)
UAR60 <- data.frame(year=1960,econaid=sum(EconAidMelt$econaid[(EconAidMelt$year == 1960) & (EconAidMelt$ccode == 651 | EconAidMelt$ccode == 652)]),Country.Name="United Arab Republic",ccode=651)
UAR61 <- data.frame(year=1961,econaid=sum(EconAidMelt$econaid[(EconAidMelt$year == 1961) & (EconAidMelt$ccode == 651 | EconAidMelt$ccode == 652)]),Country.Name="United Arab Republic",ccode=651)

EconAidMelt = rbind(EconAidMelt,UAR58,UAR59,UAR60,UAR61)

EconAidMelt <- subset(EconAidMelt, !( Country.Name == "Egypt" & year > 1957 & year < 1962))
EconAidMelt <- subset(EconAidMelt, !( Country.Name == "Syria" & year > 1957 & year < 1962))
EconAidMelt$Country.Name[EconAidMelt$Country.Name == "United Arab Republic"] <- "Egypt"

# St. Kitts and Nevis actually doesn't exist in the econaid data, but does in the militaid data. Let's create a blank frame for St. Kitts and Nevis.

SKN8311 <- data.frame(year=seq(1983,2011),econaid=NA,Country.Name="St. Kitts and Nevis",ccode=60)
EconAidMelt = rbind(EconAidMelt,SKN8311)

# Now, for something that's necessary: dealing with the missingness created by the "totaling" process earlier in the script.
# We are going to deterministically impute zero for missingness, THEN, importantly, delete observations before they were COW states.
# If you wish, you can add "- 1" in front of these years if you're interested in the fiscal year before formal independence.

EconAidMelt[is.na(EconAidMelt$econaid),"econaid"] <- 0
EconAidMelt <- subset(EconAidMelt, !(ccode == 31 & year < 1973))
EconAidMelt <- subset(EconAidMelt, !(ccode == 51 & year < 1962))
EconAidMelt <- subset(EconAidMelt, !(ccode == 52 & year < 1962))
EconAidMelt <- subset(EconAidMelt, !(ccode == 53 & year < 1966))
EconAidMelt <- subset(EconAidMelt, !(ccode == 54 & year < 1978))
EconAidMelt <- subset(EconAidMelt, !(ccode == 55 & year < 1974))
EconAidMelt <- subset(EconAidMelt, !(ccode == 56 & year < 1979))
EconAidMelt <- subset(EconAidMelt, !(ccode == 57 & year < 1979))
EconAidMelt <- subset(EconAidMelt, !(ccode == 58 & year < 1981))
EconAidMelt <- subset(EconAidMelt, !(ccode == 60 & year < 1983))
EconAidMelt <- subset(EconAidMelt, !(ccode == 80 & year < 1981))
EconAidMelt <- subset(EconAidMelt, !(ccode == 110 & year < 1966))
EconAidMelt <- subset(EconAidMelt, !(ccode == 115 & year < 1975))
EconAidMelt <- subset(EconAidMelt, !(ccode == 221 & year < 1993))
EconAidMelt <- subset(EconAidMelt, !(ccode == 223 & year < 1990))
EconAidMelt <- subset(EconAidMelt, !(ccode == 232 & year < 1993))
EconAidMelt <- subset(EconAidMelt, !(ccode == 260 & year < 1955)) # This is a coding decision on West Germany/Germany
EconAidMelt <- subset(EconAidMelt, !(ccode == 305 & year < 1955))
EconAidMelt <- subset(EconAidMelt, !(ccode == 316 & year < 1993))
EconAidMelt <- subset(EconAidMelt, !(ccode == 317 & year < 1993))
EconAidMelt <- subset(EconAidMelt, !(ccode == 331 & year < 1992))
EconAidMelt <- subset(EconAidMelt, !(ccode == 338 & year < 1964))
EconAidMelt <- subset(EconAidMelt, !(ccode == 341 & year < 2006))
EconAidMelt <- subset(EconAidMelt, !(ccode == 343 & year < 1993))
EconAidMelt <- subset(EconAidMelt, !(ccode == 344 & year < 1992))
EconAidMelt <- subset(EconAidMelt, !(ccode == 346 & year < 1992))
EconAidMelt <- subset(EconAidMelt, !(ccode == 347 & year < 2008))
EconAidMelt <- subset(EconAidMelt, !(ccode == 349 & year < 1992))
EconAidMelt <- subset(EconAidMelt, !(ccode == 352 & year < 1960))
EconAidMelt <- subset(EconAidMelt, !(ccode == 359 & year < 1991))
EconAidMelt <- subset(EconAidMelt, !(ccode == 366 & year < 1991))
EconAidMelt <- subset(EconAidMelt, !(ccode == 367 & year < 1991))
EconAidMelt <- subset(EconAidMelt, !(ccode == 368 & year < 1991))
EconAidMelt <- subset(EconAidMelt, !(ccode == 369 & year < 1991))
EconAidMelt <- subset(EconAidMelt, !(ccode == 370 & year < 1991))
EconAidMelt <- subset(EconAidMelt, !(ccode == 371 & year < 1991))
EconAidMelt <- subset(EconAidMelt, !(ccode == 372 & year < 1991))
EconAidMelt <- subset(EconAidMelt, !(ccode == 373 & year < 1991))
EconAidMelt <- subset(EconAidMelt, !(ccode == 402 & year < 1975))
EconAidMelt <- subset(EconAidMelt, !(ccode == 403 & year < 1975))
EconAidMelt <- subset(EconAidMelt, !(ccode == 404 & year < 1974))
EconAidMelt <- subset(EconAidMelt, !(ccode == 411 & year < 1968))
EconAidMelt <- subset(EconAidMelt, !(ccode == 420 & year < 1965))
EconAidMelt <- subset(EconAidMelt, !(ccode == 432 & year < 1960))
EconAidMelt <- subset(EconAidMelt, !(ccode == 433 & year < 1960))
EconAidMelt <- subset(EconAidMelt, !(ccode == 434 & year < 1960))
EconAidMelt <- subset(EconAidMelt, !(ccode == 435 & year < 1960))
EconAidMelt <- subset(EconAidMelt, !(ccode == 436 & year < 1960))
EconAidMelt <- subset(EconAidMelt, !(ccode == 437 & year < 1960))
EconAidMelt <- subset(EconAidMelt, !(ccode == 438 & year < 1958))
EconAidMelt <- subset(EconAidMelt, !(ccode == 439 & year < 1960))
EconAidMelt <- subset(EconAidMelt, !(ccode == 451 & year < 1961))
EconAidMelt <- subset(EconAidMelt, !(ccode == 452 & year < 1957))
EconAidMelt <- subset(EconAidMelt, !(ccode == 461 & year < 1960))
EconAidMelt <- subset(EconAidMelt, !(ccode == 471 & year < 1960))
EconAidMelt <- subset(EconAidMelt, !(ccode == 475 & year < 1960))
EconAidMelt <- subset(EconAidMelt, !(ccode == 481 & year < 1960))
EconAidMelt <- subset(EconAidMelt, !(ccode == 482 & year < 1960))
EconAidMelt <- subset(EconAidMelt, !(ccode == 483 & year < 1960))
EconAidMelt <- subset(EconAidMelt, !(ccode == 484 & year < 1960))
EconAidMelt <- subset(EconAidMelt, !(ccode == 490 & year < 1960))
EconAidMelt <- subset(EconAidMelt, !(ccode == 500 & year < 1962))
EconAidMelt <- subset(EconAidMelt, !(ccode == 501 & year < 1963))
EconAidMelt <- subset(EconAidMelt, !(ccode == 510 & year < 1961))
EconAidMelt <- subset(EconAidMelt, !(ccode == 511 & year < 1963))
EconAidMelt <- subset(EconAidMelt, !(ccode == 516 & year < 1962))
EconAidMelt <- subset(EconAidMelt, !(ccode == 517 & year < 1962))
EconAidMelt <- subset(EconAidMelt, !(ccode == 520 & year < 1960))
EconAidMelt <- subset(EconAidMelt, !(ccode == 522 & year < 1977))
EconAidMelt <- subset(EconAidMelt, !(ccode == 531 & year < 1993))
EconAidMelt <- subset(EconAidMelt, !(ccode == 540 & year < 1975))
EconAidMelt <- subset(EconAidMelt, !(ccode == 541 & year < 1975))
EconAidMelt <- subset(EconAidMelt, !(ccode == 551 & year < 1964))
EconAidMelt <- subset(EconAidMelt, !(ccode == 552 & year < 1965))
EconAidMelt <- subset(EconAidMelt, !(ccode == 553 & year < 1964))
EconAidMelt <- subset(EconAidMelt, !(ccode == 565 & year < 1990))
EconAidMelt <- subset(EconAidMelt, !(ccode == 570 & year < 1966))
EconAidMelt <- subset(EconAidMelt, !(ccode == 571 & year < 1966))
EconAidMelt <- subset(EconAidMelt, !(ccode == 572 & year < 1968))
EconAidMelt <- subset(EconAidMelt, !(ccode == 580 & year < 1960))
EconAidMelt <- subset(EconAidMelt, !(ccode == 581 & year < 1975))
EconAidMelt <- subset(EconAidMelt, !(ccode == 590 & year < 1968))
EconAidMelt <- subset(EconAidMelt, !(ccode == 591 & year < 1976))
EconAidMelt <- subset(EconAidMelt, !(ccode == 600 & year < 1956))
EconAidMelt <- subset(EconAidMelt, !(ccode == 615 & year < 1962))
EconAidMelt <- subset(EconAidMelt, !(ccode == 616 & year < 1956))
EconAidMelt <- subset(EconAidMelt, !(ccode == 620 & year < 1951))
EconAidMelt <- subset(EconAidMelt, !(ccode == 625 & year < 1956))
EconAidMelt <- subset(EconAidMelt, !(ccode == 626 & year < 2011))
EconAidMelt <- subset(EconAidMelt, !(ccode == 666 & year < 1948))
EconAidMelt <- subset(EconAidMelt, !(ccode == 690 & year < 1961))
EconAidMelt <- subset(EconAidMelt, !(ccode == 692 & year < 1971))
EconAidMelt <- subset(EconAidMelt, !(ccode == 694 & year < 1971))
EconAidMelt <- subset(EconAidMelt, !(ccode == 696 & year < 1971))
EconAidMelt <- subset(EconAidMelt, !(ccode == 698 & year < 1971))
EconAidMelt <- subset(EconAidMelt, !(ccode == 701 & year < 1991))
EconAidMelt <- subset(EconAidMelt, !(ccode == 702 & year < 1991))
EconAidMelt <- subset(EconAidMelt, !(ccode == 703 & year < 1991))
EconAidMelt <- subset(EconAidMelt, !(ccode == 704 & year < 1991))
EconAidMelt <- subset(EconAidMelt, !(ccode == 705 & year < 1991))
EconAidMelt <- subset(EconAidMelt, !(ccode == 731 & year < 1948))
EconAidMelt <- subset(EconAidMelt, !(ccode == 732 & year < 1949))
EconAidMelt <- subset(EconAidMelt, !(ccode == 740 & year < 1952))
EconAidMelt <- subset(EconAidMelt, !(ccode == 750 & year < 1947))
EconAidMelt <- subset(EconAidMelt, !(ccode == 760 & year < 1971))
EconAidMelt <- subset(EconAidMelt, !(ccode == 770 & year < 1947))
EconAidMelt <- subset(EconAidMelt, !(ccode == 771 & year < 1972))
EconAidMelt <- subset(EconAidMelt, !(ccode == 775 & year < 1948))
EconAidMelt <- subset(EconAidMelt, !(ccode == 780 & year < 1948))
EconAidMelt <- subset(EconAidMelt, !(ccode == 781 & year < 1965))
EconAidMelt <- subset(EconAidMelt, !(ccode == 811 & year < 1953))
EconAidMelt <- subset(EconAidMelt, !(ccode == 812 & year < 1953))
EconAidMelt <- subset(EconAidMelt, !(ccode == 817 & year < 1954))
EconAidMelt <- subset(EconAidMelt, !(ccode == 820 & year < 1957))
EconAidMelt <- subset(EconAidMelt, !(ccode == 830 & year < 1965))
EconAidMelt <- subset(EconAidMelt, !(ccode == 835 & year < 1984))
EconAidMelt <- subset(EconAidMelt, !(ccode == 850 & year < 1949))
EconAidMelt <- subset(EconAidMelt, !(ccode == 860 & year < 2002))
EconAidMelt <- subset(EconAidMelt, !(ccode == 910 & year < 1975))
EconAidMelt <- subset(EconAidMelt, !(ccode == 935 & year < 1981))
EconAidMelt <- subset(EconAidMelt, !(ccode == 940 & year < 1978))
EconAidMelt <- subset(EconAidMelt, !(ccode == 946 & year < 1999))
EconAidMelt <- subset(EconAidMelt, !(ccode == 947 & year < 2000))
EconAidMelt <- subset(EconAidMelt, !(ccode == 950 & year < 1970))
EconAidMelt <- subset(EconAidMelt, !(ccode == 955 & year < 1999))
EconAidMelt <- subset(EconAidMelt, !(ccode == 970 & year < 1999))
EconAidMelt <- subset(EconAidMelt, !(ccode == 983 & year < 1991))
EconAidMelt <- subset(EconAidMelt, !(ccode == 986 & year < 1994))
EconAidMelt <- subset(EconAidMelt, !(ccode == 987 & year < 1991))
EconAidMelt <- subset(EconAidMelt, !(ccode == 990 & year < 1976))

EconAidMelt <- subset(EconAidMelt, select=c(Country.Name,ccode,year,econaid))




EconAidMelt <- EconAidMelt[order(EconAidMelt$ccode, EconAidMelt$year), ]


write.table(EconAidMelt,file="useconaid.csv",sep=",",row.names=F,na="")


##### Now, let's do the same for the military aid.


# Because USAID data has commas in the values, R will read the data as characters. Let's get rid of the commas.
col2cvt <- 3:length(MilitAid)
MilitAid[,col2cvt] <- lapply(MilitAid[,col2cvt],function(MilitAid){as.numeric(gsub(",", "", MilitAid))})

# We can get rid of Program.Name. We don't need it.
MilitAid$Program.Name <- NULL

# The US transitioned its fiscal quarters in 1976, creating a transitional quarter. 
# This will add FY1976tq to FY1976 and drop FY1976tq.
# First, we'll need to recode 1976tq to = 0 if 1976 > 0, otherwise the resulting column will be NA.
MilitAid$FY1976tq[MilitAid$FY1976 > 0 & is.na(MilitAid$FY1976tq)] <- 0
MilitAid$FY1976 <- MilitAid$FY1976 + MilitAid$FY1976tq
MilitAid$FY1976tq <- NULL



# Next, we will convert the data from wide format to long.
MilitAidMelt <- melt(MilitAid, id = "Country.Name")
summary(MilitAidMelt)

# This was a super messy "melt", creating "variable" and "value" for important units of fiscal year and economic aid total respectively.
# First, let's rename "value" as "econaid".
# Next, let's just create a "year" value for "variable" that also strips out the "FY" in the time unit.
# Afterward, we can drop "variable" and convert year to numeric, because it's driving me nuts for the meantime.
# Also, convert Country.Name from factor to character, otherwise we can't easily recode it.
MilitAidMelt <- rename(MilitAidMelt, c(value="militaid"))
MilitAidMelt$militaid <- as.numeric(MilitAidMelt$militaid)
MilitAidMelt$year <- gsub("FY","",MilitAidMelt$variable)
MilitAidMelt$variable <- NULL
MilitAidMelt$year <- as.numeric(MilitAidMelt$year)
MilitAidMelt$Country.Name <- as.character(MilitAidMelt$Country.Name)

# Let's get rid of the not-countries again.

MilitAidMelt <- MilitAidMelt[MilitAidMelt$Country.Name != "Asia (not specified)",]
MilitAidMelt <- MilitAidMelt[MilitAidMelt$Country.Name != "Caribbean (not specified)",]
MilitAidMelt <- MilitAidMelt[MilitAidMelt$Country.Name != "Central America (not specified)",]
MilitAidMelt <- MilitAidMelt[MilitAidMelt$Country.Name != "China (Hong Kong)",]
MilitAidMelt <- MilitAidMelt[MilitAidMelt$Country.Name != "East Africa (not specified)",]
MilitAidMelt <- MilitAidMelt[MilitAidMelt$Country.Name != "Eastern Europe (not specified)",]
MilitAidMelt <- MilitAidMelt[MilitAidMelt$Country.Name != "Eurasia (not specified)",]
# MilitAidMelt <- MilitAidMelt[MilitAidMelt$Country.Name != "Kosovo",]
MilitAidMelt <- MilitAidMelt[MilitAidMelt$Country.Name != "Latin America & Caribbean (not specified)",]
MilitAidMelt <- MilitAidMelt[MilitAidMelt$Country.Name != "Middle East & North Africa (not specified)",]
MilitAidMelt <- MilitAidMelt[MilitAidMelt$Country.Name != "Oceania (not specified)",]
MilitAidMelt <- MilitAidMelt[MilitAidMelt$Country.Name != "Southern Africa (not specified)",]
MilitAidMelt <- MilitAidMelt[MilitAidMelt$Country.Name != "Sub-Saharan Africa (not specified)",]
MilitAidMelt <- MilitAidMelt[MilitAidMelt$Country.Name != "West Africa (not specified)",]
MilitAidMelt <- MilitAidMelt[MilitAidMelt$Country.Name != "West Bank/Gaza",]
MilitAidMelt <- MilitAidMelt[MilitAidMelt$Country.Name != "Western Europe (not specified)",]
MilitAidMelt <- MilitAidMelt[MilitAidMelt$Country.Name != "World (not specified)",]


# Let's create some ccodes as well.
MilitAidMelt$ccode <- NA
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Afghanistan"] <- 700
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Albania"] <- 339
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Algeria"] <- 615
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Angola"] <- 540
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Antigua and Barbuda"] <- 58
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Argentina"] <- 160
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Armenia"] <- 371
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Australia"] <- 900
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Austria"] <- 305
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Azerbaijan"] <- 373
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Bahamas, The"] <- 31
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Bahrain"] <- 692
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Bangladesh"] <- 771
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Barbados"] <- 53
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Belarus"] <- 370
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Belgium"] <- 211
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Belize"] <- 80
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Benin"] <- 434
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Bhutan"] <- 760
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Bolivia"] <- 145
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Bosnia & Herzegovina"] <- 346
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Botswana"] <- 571
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Brazil"] <- 140
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Brunei"] <- 835
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Bulgaria"] <- 355
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Burkina Faso"] <- 439
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Burma (Myanmar)"] <- 775
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Burundi"] <- 516
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Cambodia"] <- 811
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Cameroon"] <- 471
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Canada"] <- 20
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Cape Verde"] <- 402
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Central African Republic"] <- 482
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Chad"] <- 483
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Chile"] <- 155
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "China (P.R.C.)"] <- 710
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "China (Taiwan)"] <- 713
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Colombia"] <- 100
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Comoros"] <- 581
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Congo (Brazzaville)"] <- 484
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Congo (Kinshasa)"] <- 490
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Costa Rica"] <- 94
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Cote d'Ivoire"] <- 437
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Croatia"] <- 344
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Cuba"] <- 40
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Cyprus"] <- 352
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Czech Republic"] <- 316
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Denmark"] <- 390
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Djibouti"] <- 522
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Dominica"] <- 54
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Dominican Republic"] <- 42
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Ecuador"] <- 130
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Egypt"] <- 651
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "El Salvador"] <- 92
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Equatorial Guinea"] <- 411
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Eritrea"] <- 531
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Estonia"] <- 366
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Ethiopia"] <- 530
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Fiji"] <- 950
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Finland"] <- 375
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "France"] <- 220
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Gabon"] <- 481
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Gambia, The"] <- 420
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Georgia"] <- 372
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Germany"] <- 260
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Ghana"] <- 452
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Greece"] <- 350
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Grenada"] <- 55
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Guatemala"] <- 90
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Guinea"] <- 438
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Guinea-Bissau"] <- 404
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Guyana"] <- 110
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Haiti"] <- 41
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Honduras"] <- 91
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Hungary"] <- 310
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Iceland"] <- 395
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "India"] <- 750
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Iceland"] <- 395
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Indonesia"] <- 850
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Iran"] <- 630
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Iraq"] <- 645
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Ireland"] <- 205
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Israel"] <- 666
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Italy"] <- 325
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Jamaica"] <- 51
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Japan"] <- 740
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Jordan"] <- 663
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Kazakhstan"] <- 705
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Kenya"] <- 501
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Kiribati"] <- 946
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Korea, North"] <- 731
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Korea, South"] <- 732
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Kosovo"] <- 347
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Kuwait"] <- 690
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Kyrgyzstan"] <- 703
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Laos"] <- 812
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Latvia"] <- 367
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Lebanon"] <- 660
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Lesotho"] <- 570
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Liberia"] <- 450
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Libya"] <- 620
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Lithuania"] <- 368
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Macedonia, Former Yugoslav Republic"] <- 343
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Madagascar"] <- 580
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Malawi"] <- 553
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Malaysia"] <- 820
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Maldives"] <- 781
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Mali"] <- 432
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Malta"] <- 338
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Marshall Islands"] <- 983
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Mauritania"] <- 435
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Mauritius"] <- 590
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Mexico"] <- 70
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Micronesia (Federated States of)"] <- 987
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Moldova"] <- 359
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Mongolia"] <- 712
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Montenegro"] <- 341
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Morocco"] <- 600
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Mozambique"] <- 541
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Namibia"] <- 565
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Nepal"] <- 790
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Netherlands"] <- 210
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "New Zealand"] <- 920
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Nicaragua"] <- 93
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Niger"] <- 436
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Nigeria"] <- 475
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Norway"] <- 385
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Oman"] <- 698
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Pakistan"] <- 770
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Palau"] <- 986
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Panama"] <- 95
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Papua New Guinea"] <- 910
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Paraguay"] <- 150
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Peru"] <- 135
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Philippines"] <- 840
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Poland"] <- 290
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Portugal"] <- 235
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Qatar"] <- 694
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Romania"] <- 360
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Russia"] <- 365
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Rwanda"] <- 517
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Samoa"] <- 990
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Sao Tome & Principe"] <- 403
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Saudi Arabia"] <- 670
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Senegal"] <- 433
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Serbia"] <- 345
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Serbia and Montenegro, Former"] <- 345
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Seychelles"] <- 591
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Sierra Leone"] <- 451
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Singapore"] <- 830
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Slovakia"] <- 317
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Slovenia"] <- 349
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Solomon Islands"] <- 940
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Somalia"] <- 520
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "South Africa"] <- 560
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "South Sudan"] <- 626
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Spain"] <- 230
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Sri Lanka"] <- 780
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "St. Lucia"] <- 56
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "St. Vincent and Grenadines"] <- 57
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "St. Kitts and Nevis"] <- 60
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Sudan"] <- 625
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Suriname"] <- 115
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Swaziland"] <- 572
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Sweden"] <- 380
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Switzerland"] <- 225
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Syria"] <- 652
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Tajikistan"] <- 702
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Tanzania"] <- 510
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Thailand"] <- 800
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Timor-Leste"] <- 860
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Togo"] <- 461
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Tonga"] <- 955
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Trinidad & Tobago"] <- 52
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Tunisia"] <- 616
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Turkey"] <- 640
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Turkmenistan"] <- 701
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Uganda"] <- 500
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Ukraine"] <- 369
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "United Arab Emirates"] <- 696
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "United Kingdom"] <- 200
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Uruguay"] <- 165
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Uzbekistan"] <- 704
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Vanuatu"] <- 935
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Venezuela"] <- 101
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Vietnam"] <- 816
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Yemen"] <- 679
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Zambia"] <- 551
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Zanzibar"] <- 511
MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Zimbabwe"] <- 552

# Let's again correct for East Germany, Czechoslovakia, United Arab Republic, the two Yemens, Vietnam, China/Taiwan, and so on.

MilitAidMelt$ccode[MilitAidMelt$Country.Name == "Vietnam" & MilitAidMelt$year < 1976] <- 817
MilitAidMelt[MilitAidMelt[, "Country.Name"] == "Vietnam" & MilitAidMelt[, "year"] < 1976 , "Country.Name"] <- "South Vietnam"

Viet5475 <- data.frame(year=seq(1954,1975),militaid=NA,Country.Name="Vietnam",ccode=816)
summary(Viet5475)
MilitAidMelt = rbind(MilitAidMelt,Viet5475)

MilitAidMelt <- subset(MilitAidMelt, !( ccode == 710 & year < 1950))
MilitAidMelt$ccode[MilitAidMelt$ccode == 713 & MilitAidMelt$year < 1950] <- 710
MilitAidMelt[MilitAidMelt[, "Country.Name"] == "China (Taiwan)" & MilitAidMelt[, "year"] < 1950 , "Country.Name"] <- "China (P.R.C.)"


MilitAidMelt$ccode[MilitAidMelt$ccode == 679 & MilitAidMelt$year < 1990] <- 678
MilitAidMelt[MilitAidMelt[, "Country.Name"] == "Yemen" & MilitAidMelt[, "year"] < 1990 , "Country.Name"] <- "Yemen Arab Republic"

YPR6789 <- data.frame(year=seq(1967,1989),militaid=NA,Country.Name="Yemen People's Republic",ccode=680)
MilitAidMelt = rbind(MilitAidMelt,YPR6789)


GDR5490 <- data.frame(year=seq(1954,1990),militaid=NA,Country.Name="East Germany",ccode=265)
MilitAidMelt = rbind(MilitAidMelt,GDR5490)

CZE4692 <- data.frame(year=seq(1946,1992),militaid=NA,Country.Name="Czechoslovakia",ccode=315)
MilitAidMelt = rbind(MilitAidMelt,CZE4692)



Serbia2006 <- data.frame(year=2006,militaid=sum(MilitAidMelt$militaid[(MilitAidMelt$year == 2006) & (MilitAidMelt$ccode == 345)]),Country.Name="Serbia 2006 (adjusted)",ccode=345)
MilitAidMelt = rbind(MilitAidMelt,Serbia2006)

MilitAidMelt <- subset(MilitAidMelt, !( Country.Name == "Serbia" & year < 2007))
MilitAidMelt <- subset(MilitAidMelt, !( Country.Name == "Serbia and Montenegro, Former" & year > 2005))



MilitAidMelt$Country.Name[MilitAidMelt$Country.Name == "Serbia and Montenegro, Former"] <- "Yugoslavia/Serbia"
MilitAidMelt$Country.Name[MilitAidMelt$Country.Name == "Serbia"] <- "Yugoslavia/Serbia"
MilitAidMelt$Country.Name[MilitAidMelt$Country.Name == "Serbia 2006 (adjusted)"] <- "Yugoslavia/Serbia"

UAR58 <- data.frame(year=1958,militaid=sum(MilitAidMelt$militaid[(MilitAidMelt$year == 1958) & (MilitAidMelt$ccode == 651 | MilitAidMelt$ccode == 652)]),Country.Name="United Arab Republic",ccode=651)
UAR59 <- data.frame(year=1959,militaid=sum(MilitAidMelt$militaid[(MilitAidMelt$year == 1959) & (MilitAidMelt$ccode == 651 | MilitAidMelt$ccode == 652)]),Country.Name="United Arab Republic",ccode=651)
UAR60 <- data.frame(year=1960,militaid=sum(MilitAidMelt$militaid[(MilitAidMelt$year == 1960) & (MilitAidMelt$ccode == 651 | MilitAidMelt$ccode == 652)]),Country.Name="United Arab Republic",ccode=651)
UAR61 <- data.frame(year=1961,militaid=sum(MilitAidMelt$militaid[(MilitAidMelt$year == 1961) & (MilitAidMelt$ccode == 651 | MilitAidMelt$ccode == 652)]),Country.Name="United Arab Republic",ccode=651)

MilitAidMelt = rbind(MilitAidMelt,UAR58,UAR59,UAR60,UAR61)

MilitAidMelt <- subset(MilitAidMelt, !( Country.Name == "Egypt" & year > 1957 & year < 1962))
MilitAidMelt <- subset(MilitAidMelt, !( Country.Name == "Syria" & year > 1957 & year < 1962))
MilitAidMelt$Country.Name[MilitAidMelt$Country.Name == "United Arab Republic"] <- "Egypt"


# Let's correct for the missingness now, like we did with the econaid data.
# RECALL: We can always add "-1" to the year in these functions if we're interested in the fiscal year BEFORE formal independence of the state.
MilitAidMelt[is.na(MilitAidMelt$militaid),"militaid"] <- 0
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 31 & year < 1973))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 51 & year < 1962))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 52 & year < 1962))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 53 & year < 1966))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 54 & year < 1978))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 55 & year < 1974))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 56 & year < 1979))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 57 & year < 1979))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 58 & year < 1981))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 60 & year < 1983))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 80 & year < 1981))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 110 & year < 1966))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 115 & year < 1975))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 221 & year < 1993))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 223 & year < 1990))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 232 & year < 1993))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 260 & year < 1955)) # This is a coding decision on West Germany/Germany
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 305 & year < 1955))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 316 & year < 1993))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 317 & year < 1993))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 331 & year < 1992))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 338 & year < 1964))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 341 & year < 2006))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 343 & year < 1993))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 344 & year < 1992))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 346 & year < 1992))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 347 & year < 2008))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 349 & year < 1992))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 352 & year < 1960))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 359 & year < 1991))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 366 & year < 1991))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 367 & year < 1991))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 368 & year < 1991))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 369 & year < 1991))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 370 & year < 1991))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 371 & year < 1991))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 372 & year < 1991))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 373 & year < 1991))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 402 & year < 1975))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 403 & year < 1975))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 404 & year < 1974))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 411 & year < 1968))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 420 & year < 1965))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 432 & year < 1960))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 433 & year < 1960))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 434 & year < 1960))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 435 & year < 1960))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 436 & year < 1960))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 437 & year < 1960))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 438 & year < 1958))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 439 & year < 1960))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 451 & year < 1961))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 452 & year < 1957))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 461 & year < 1960))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 471 & year < 1960))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 475 & year < 1960))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 481 & year < 1960))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 482 & year < 1960))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 483 & year < 1960))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 484 & year < 1960))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 490 & year < 1960))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 500 & year < 1962))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 501 & year < 1963))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 510 & year < 1961))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 511 & year < 1963))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 516 & year < 1962))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 517 & year < 1962))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 520 & year < 1960))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 522 & year < 1977))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 531 & year < 1993))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 540 & year < 1975))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 541 & year < 1975))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 551 & year < 1964))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 552 & year < 1965))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 553 & year < 1964))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 565 & year < 1990))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 570 & year < 1966))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 571 & year < 1966))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 572 & year < 1968))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 580 & year < 1960))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 581 & year < 1975))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 590 & year < 1968))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 591 & year < 1976))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 600 & year < 1956))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 615 & year < 1962))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 616 & year < 1956))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 620 & year < 1951))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 625 & year < 1956))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 626 & year < 2011))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 666 & year < 1948))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 690 & year < 1961))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 692 & year < 1971))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 694 & year < 1971))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 696 & year < 1971))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 698 & year < 1971))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 701 & year < 1991))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 702 & year < 1991))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 703 & year < 1991))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 704 & year < 1991))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 705 & year < 1991))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 731 & year < 1948))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 732 & year < 1949))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 740 & year < 1952))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 750 & year < 1947))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 760 & year < 1971))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 770 & year < 1947))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 771 & year < 1972))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 775 & year < 1948))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 780 & year < 1948))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 781 & year < 1965))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 811 & year < 1953))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 812 & year < 1953))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 817 & year < 1954))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 820 & year < 1957))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 830 & year < 1965))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 835 & year < 1984))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 850 & year < 1949))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 860 & year < 2002))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 910 & year < 1975))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 935 & year < 1981))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 940 & year < 1978))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 946 & year < 1999))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 947 & year < 2000))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 950 & year < 1970))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 955 & year < 1999))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 970 & year < 1999))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 983 & year < 1991))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 986 & year < 1994))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 987 & year < 1991))
MilitAidMelt <- subset(MilitAidMelt, !(ccode == 990 & year < 1976))

MilitAidMelt <- subset(MilitAidMelt, select=c(Country.Name,ccode,year,militaid))




MilitAidMelt <- MilitAidMelt[order(MilitAidMelt$ccode, MilitAidMelt$year), ]


write.table(MilitAidMelt,file="usmilitaid.csv",sep=",",row.names=F,na="")

# Let's create a data.frame that has both.

USAID <- merge(EconAidMelt, MilitAidMelt, by=c("Country.Name","ccode", "year"), all = TRUE)

# What this will do is add zeroes to all countries who are missing on militaid. This is because several countries that are eligible for military aid have never received it (e.g. Sweden, Switzerland).
USAID[is.na(USAID$militaid),"militaid"] <- 0


# We can take some natural logs, but need to do this in an ifelse statement. If it's negative, take the log of the absolute value, and then make that value negative.
# This procedure comes from Baccini and Urpelainen (2012).
# If it's not negative, we plus-one and log.
USAID$logeconaid <- ifelse(USAID$econaid < 0, -log(abs(USAID$econaid)), log(USAID$econaid + 1))
USAID$logmilitaid <- ifelse(USAID$militaid < 0, -log(abs(USAID$militaid)), log(USAID$militaid + 1))

# If it's negative, treat it as missing.
USAID$logeconaidna <- ifelse(USAID$econaid < 0, NA, log(USAID$econaid + 1))
USAID$logmilitaidna <- ifelse(USAID$militaid < 0, NA, log(USAID$militaid + 1))

USAID <- USAID[order(USAID$ccode, USAID$year), ]


write.table(USAID,file="ustotalaid.csv",sep=",",row.names=F,na="")







