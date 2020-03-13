#
# This file is part of the code for the BIMI Spatial Inequities Shinydashboard Clinics+ Census data web mapper.
# This should be the second of 2 R files run to prepare the data for the mapper.
# This code:
#  1. fetches health and legal clinic data for CA
#  2. reformats that data to prepare it for the mapper
#  3  loads in the census data for the nine county Bay Area that was prepared by the preprocess_census_data.R file
#  4. Joins the census and clinic data and discards clinic data not in the census geographies being mapped
#  5. Updates the mapper filters to be appropriate for the subseted clinic data 
#  6. saves the output to an Rdata file that is then read in by the file "app.R"
#
# Authors: Denys Dukhovnov (denys_dukhovnov@berkeley.edu) & Patty Frontiera (pattyf@berkeley.edu)
#

library(dplyr)
library(tidyr)
library(sf)
library(readxl)
library(plyr)

#setwd(".")
setwd("/Users/chengren/Downloads/bimi_msi/PREPROCESS_SHINY_DATA/")

# Source code to jitter any duplicate coords (locations)
source("geoR_jitter.R")
#----------------------------------------------------------------------------------------
# Health Clinics
#----------------------------------------------------------------------------------------

# Read in the data **TAB SEPARATED CSV**
# Patty note: I needed to edit the csv file manually to remove double quotes to be able to readin file in R
# Health_clinics 
#health_clinics <- read.delim("Clinic_Data/Health_Clinics_Complete_Data.csv", stringsAsFactors = F, strip.white = T)
health_clinics_ba <-read_excel("Clinic_Data/v2/Health_Clinics_Complete_Data_Language_Consolidated.xlsx", 
                            sheet="Health_Clinics_Complete_Data_v2", trim_ws = T, col_names = T)
health_clinics_cv <-read_excel("Clinic_Data/cv/Central Valley Health Clinics.xlsx", 
                               sheet="Central Valley", trim_ws = T, col_names = T)
# Standardize colnames to replace one or more non-alpha characters with a period
#names(health_clinics_ba)
#names(health_clinics_cv)
# the mistake in the old table
health_clinics_cv$`Site Web Address Supplemented` <- health_clinics_cv$`Site Web Address`
names(health_clinics_ba) <- gsub(x = names(health_clinics_ba), pattern = "\\(|\\)|\\.|\\d$|^[[:space:]]|[[:space:]]$ ", replacement = "") 
names(health_clinics_cv) <- gsub(x = names(health_clinics_cv), pattern = "\\(|\\)|\\.|\\d$|^[[:space:]]|[[:space:]]$ ", replacement = "") 
health_clinics_ba$ID <- NULL
#Columns `Health Center Site Fact Identification Number`, `Site Name`, `
#Site Name Uniform`, `Language Support Mandarin`, `Language Support Cantonese` must have a unique name
#get column index of these wrong name
t <- which(names(health_clinics_ba)%in%c("Language Support Cantonese10","Language Support Mandarin10"))
colnames(health_clinics_ba)[t] <- c("Language Support Cantonese","Language Support Mandarin")

# In order to combine by rows, we need to change the order of column
t1 <- names(health_clinics_cv)[match(names(health_clinics_ba),names(health_clinics_cv))]
#common columns
health_clinics_cv_c <- health_clinics_cv %>% select(na.omit(t1))
#rest uncommon columns
health_clinics_cv_r <- health_clinics_cv %>% select(-na.omit(t1))
#common columns
health_clinics_ba_c<- health_clinics_ba %>% select(na.omit(t1))
#rest uncommon columns
health_clinics_ba_r <- health_clinics_ba %>% select(-na.omit(t1))
table(names(health_clinics_ba_c)==names(health_clinics_cv_c))#115
#Let compare the rest name, are there any possible to fix the column name that should be same
names(health_clinics_cv_r)
names(health_clinics_ba_r)#lots of language are not avialble in central valley
#combine two dataframe
health_clinics_ba<- cbind(health_clinics_ba_c,health_clinics_ba_r)
health_clinics_cv<- cbind(health_clinics_cv_c,health_clinics_cv_r)
health_clinics <-rbind.fill(health_clinics_ba,health_clinics_cv)
#merge the rest dataset
#change format of column name
colnames(health_clinics) <- gsub("\\.\\.+|[^[:alnum:]]", ".", colnames(health_clinics))
colnames(health_clinics) <- gsub("\\.\\.", ".", colnames(health_clinics))


#
# Give clinics IDs - ignoring ID column in the file
#
health_clinics$ID <- 1:nrow(health_clinics)

### # Added # free value of 66 to ID of the missing clinic
# sort(unique(as.numeric(health_clinics$ID)), decreasing = F)  # Check which IDs exist
# health_clinics[258, 1] <- 66

#######################################################
# Jitter duplicate coordinates
## So that we get data for all points at the location
#######################################################
#Geocoding Artifact Address Primary X Coordinate
#Geocoding Artifact Address Primary Y Coordinate
health_clinics[c("Geocoding.Artifact.Address.Primary.X.Coordinate","Geocoding.Artifact.Address.Primary.Y.Coordinate")] <- jitterDupCoords(health_clinics[c("Geocoding.Artifact.Address.Primary.X.Coordinate","Geocoding.Artifact.Address.Primary.Y.Coordinate")], min=0.00001, max=0.00009)

####################################
# Reformat to create full_address
####################################
health_clinics$full_address <- paste0(health_clinics$Site.Address, ", ", 
                                      health_clinics$Site.City, ", ",
                                      health_clinics$Site.State.Abbreviation, " ", 
                                      health_clinics$Site.Postal.Code)

####################################
# Reformat to create website URL
####################################

health_clinics$website <- health_clinics$Clinic.Site.Web.Address

health_clinics  <- health_clinics %>%
  mutate(website = if_else(is.na(website) == TRUE, health_clinics$Site.Web.Address.Supplemented, website)) %>%
  mutate(website = if_else(is.na(website) == TRUE & is.na(Site.Web.Address.Supplemented) == TRUE, 
                           health_clinics$Site.Web.Address, website))

health_clinics$website <- ifelse(substring(health_clinics$website, 1, 4) == "http", health_clinics$website, paste0("http://", health_clinics$website))



#####################################################################################
# CREATE STANDARDIZED LANGUAGE FILTER COLUMN
#####################################################################################
# We will only use the columns that begin "Language.Support.<language_name>"
# First rename some columns to exclude ones we won't use in the filters
# Or to reformate the column names to automate extraction
# First remove the ones we won't be useinng by renaming them
detach(package:plyr,unload=TRUE)
health_clinics <- rename(health_clinics, 
                        Language.Form.Notes=Language.Support.Form,
                        Language.Filter.List= Language.Support.Filter, 
                        Language.Screenshot=Language.Support.Screenshot,
                        #Language.SupportOriginal.Dari=Language.Support.Dari,
                        Language.SupportOriginal.Persian=Language.Support.Persian,
                        Language.SupportOriginal.Mandarin=Language.Support.Mandarin,
                        Language.SupportOriginal.Chinese=Language.Support.Chinese,
                        Language.SupportOriginal.Taishanese=Language.Support.Toishan,
                        Language.SupportOriginal.Cantonese=Language.Support.Cantonese
                        #Language.SupportOriginal.Tagalog=Language.Support.Tagalog #,
                        #Language.SupportOriginal.Filipino=Language.Support.Filipino
                        )

# Then rename the ones we will use - as needed
health_clinics <- rename(health_clinics, 
                         Language.Support.Tagalog.Filipino=Language.Support.Tagalog,
                         #Language.Support.Persian.Dari.Farsi=Language.Support.Farsi,
                         Language.Support.Mandarin.Chinese=Language.SupportOriginal.Mandarin,
                         Language.Support.Cantonese=Language.SupportOriginal.Cantonese
)

# Extract the names of all columns with the languages we will filter on
# and create a list of the labels we will use in the web app
all_health_language_columns <- names(health_clinics)[grep("Language.Support\\.",names(health_clinics))]
all_health_language_labels <- trimws(gsub("\\.", "/", gsub("Language.Support.", "", all_health_language_columns)))

# Create a language lookup list mapping column names to filter labels
language_lut<- list()
for (i in 1:length(all_health_language_columns)) {
  print(paste0("label ", all_health_language_columns[i], " to ", all_health_language_labels[i]))
  language_lut[[all_health_language_columns[i]]] <- all_health_language_labels[i]
}

# Replace NAs with empty strings
# IFF the column contains characters
for (i in sort(all_health_language_columns)) {
  if (nrow(health_clinics[is.na(health_clinics[i]),]) > 0) {
    # Recode NAs to empty strings ("")
    print(i)
    health_clinics[[i]] <- ifelse(is.na(health_clinics[[i]]), " ", health_clinics[[i]])
    
  }
}

# Apply the language lookup to populate the language filter columns with the language label
for (i in all_health_language_columns) {
  for (r in 1:nrow(health_clinics)) {
    if (health_clinics[r,i] == "Yes" || health_clinics[r, i] == "1") {        # All rows that are marked "Yes" or "1", else NA
        health_clinics[r,i] <- language_lut[[i]]
    } else {
      health_clinics[r,i] <- ""
    }
  }
}

# Now concatenate the language filters into one long string
health_clinics$language_filter <- gsub(" +", ", ", trimws(do.call(paste, c(health_clinics[all_health_language_columns], sep = " "))))


#####################################################################################
# older code
#
# Create a language filter string based on standardized column names with binary values
# all_health_languages <- trimws(gsub("\\.", "/", gsub("Language.Support.", "", names(health_clinics)[92:120])))
# 
# for (i in 92:120) {
#   for (r in 1:nrow(health_clinics)) {
#     if (health_clinics[r,i] == "Yes" | health_clinics[r, i] == "1") {    # All rows that are marked "Yes" or "1", else empty cell
#       health_clinics[r,i] <- all_health_languages[i-91]} 
#     else {health_clinics[r,i] <- ""}
#   }
# }
# 
# health_clinics$language_filter <- gsub(" +", ", ", trimws(do.call(paste, c(health_clinics[92:120], sep = " "))))
#####################################################################################

####################################
# Health Services Filter
####################################
# Create a health services filter string based on standardized column names with binary values
health_clinics$Require.Payment <- with(health_clinics, 
                                       ifelse(Require.Payment == "Sliding scale", "Sliding scale payment system", 
                                              #ifelse(Require.Payment == "Yes" | Require.Payment == "1", "Payment required", 
                                                     ifelse(Require.Payment == "Free of charge" | Require.Payment == "2", "Free services", "")))#)

health_clinics$Sameday.Appointment.or.Walk.in <- with(health_clinics, 
                                       ifelse(Sameday.Appointment.or.Walk.in == "Walk-in" | 
                                                Sameday.Appointment.or.Walk.in == "3", "Walk-ins", 
                                              ifelse(Sameday.Appointment.or.Walk.in == "Same-day" | 
                                                       Sameday.Appointment.or.Walk.in == "1", "Same-day appointments", 
                                                     ifelse(Sameday.Appointment.or.Walk.in == "Both" |
                                                              Sameday.Appointment.or.Walk.in == "4", "Same-day appointments & walk-ins", ""))))
                                                           # ifelse(Sameday.Appointment.or.Walk.in == "Neither" |
                                                           #          Sameday.Appointment.or.Walk.in == "2", "No same-day appt. or walk-ins", "")))))
                                                    
health_clinics$Late.Hours.or.Weekend.Appointment <- with(health_clinics,
                                       ifelse(Late.Hours.or.Weekend.Appointment == "After 6 pm" | 
                                                Late.Hours.or.Weekend.Appointment == "1", "Open after 6 pm",
                                              ifelse(Late.Hours.or.Weekend.Appointment == "Weekend" |
                                                       Late.Hours.or.Weekend.Appointment == "2", "Open weekends",
                                                     ifelse(Late.Hours.or.Weekend.Appointment == "Both" | Late.Hours.or.Weekend.Appointment == "Yes" |
                                                              Late.Hours.or.Weekend.Appointment == "3", "Open after 6 pm & open weekends", ""))))
                                                           # ifelse(Late.Hours.or.Weekend.Appointment == "Neither" |
                                                           #        Late.Hours.or.Weekend.Appointment == "4", "Regular weekday hours only", "")))))
                                            

health_clinics$Enrollment.Assistance <- with(health_clinics,
                                             ifelse(Enrollment.Assistance == "Yes" | Enrollment.Assistance == "1", "Enrollment assistance", ""))
                                                   # ifelse(Enrollment.Assistance == "No" | Enrollment.Assistance == "2", "No enrollment assistance", "")))

###################################################
# Health Clinics service_access
## Create by concatening four columns
###################################################

service_access_columns <- c("Enrollment.Assistance","Require.Payment","Sameday.Appointment.or.Walk.in", "Late.Hours.or.Weekend.Appointment")
health_clinics$service_access <- gsub(",", ", ", 
                                     gsub("^,*|(?<=,),|,*$", "", 
                                          gsub(",,,|,,", ",",
                                              #apply(do.call(cbind, health_clinics[, c(74,85,126,128)]), 
                                              apply(do.call(cbind, health_clinics[, service_access_columns]), 
                                                    function(x) {do.call(paste, c(as.list(x), sep = ","))}, MARGIN = 1)), perl = T))



###################################################
# Health Clinics health_services
###################################################
# Create a services filter string based on standardized column names with binary values
## First, split the multi-service columns and rename them for shorter tags
health_clinics <- health_clinics %>% 
  mutate(Case.Management = if_else(Chronic.Illness.Management.Case.Management == 1, 1, 0)) %>%  # I assume this includes two items, hence split the name
  rename(Chronic.Illness = Chronic.Illness.Management.Case.Management) %>%
  rename(Behavioral.Mental.Health = Behavioral.Mental.Health.including.substance.abuse) %>% 
  rename(Primary.Care = Primary.Care.including.pediatrics.and.adult.medicine) %>% 
  rename(Womens.Health.OB.GYN = Women.s.health.OB.GYN.pap.smears.etc)


# identify columns with services
# We got this list in draft from using:
## x<-paste(shQuote(names(health_clinics[c(133:148)])), collapse=", ")
## plus adding new cols: Case.Management
health_service_cols <- c('Behavioral.Mental.Health', 'Cancer.Therapy', 'Dental.Care', 
                          'Chronic.Illness', 'Emergency.Urgent.Care', 'Holistic.Services', 
                          'Immunizations', 'Prevention.Wellness', 'Primary.Care', 'Physicals', 'Pharmacy', 
                          'Reproductive.Sexual.Health', 'Social.Services.Resource.Referrals', 
                          'Womens.Health.OB.GYN', 'Specialty.Care', 'Domestic.violence', 'Case.Management')

health_service_cols <- trimws(health_service_cols)
#all_health_services <- trimws(names(health_clinics)[c(133:149)])
#all_health_services <- trimws(names(health_clinics)[service_cols])
#for (i in 133:149) {
for (r in health_service_cols) {
  ls <- health_clinics[,r]
  ls[is.na(ls)] <- 0
  health_clinics[,r]<-ls
}
for (i in health_service_cols) {
  print(i)
  for (r in 1:nrow(health_clinics)) {
    if (health_clinics[r,i] == 1) { # All rows that are marked "1", else empty cell
      health_clinics[r,i] <- i #all_health_services[i-132]
    } else {
      health_clinics[r,i] <- ""
    }
  }
}

#health_clinics$health_services <- gsub("\\.", " ", gsub(" +", ", ", trimws(do.call(paste, c(health_clinics[133:149], sep = " ")))))
health_clinics$health_services <- gsub("\\.", " ", gsub(" +", ", ", trimws(do.call(paste, c(health_clinics[health_service_cols], sep = " ")))))

# ### Added to the spreadsheet 
# health_clinics[c(233,234,262,263), "Geocoding.Artifact.Address.Primary.X.Coordinate"] <- c(-122.231675, -122.231675, -121.976641, -122.067731)
# health_clinics[c(233,234,262,263), "Geocoding.Artifact.Address.Primary.Y.Coordinate"] <- c(37.798651, 37.798651, 38.360680, 38.236525)

# Recode HSRA
health_clinics$HRSA_text <- "unknown"
health_clinics[["Migrant.Health.Centers.HRSA.Grant.Subprogram.Indicator"]] <- ifelse(is.na(health_clinics[["Migrant.Health.Centers.HRSA.Grant.Subprogram.Indicator"]]), "unknown", health_clinics[["Migrant.Health.Centers.HRSA.Grant.Subprogram.Indicator"]])
health_clinics[health_clinics$Migrant.Health.Centers.HRSA.Grant.Subprogram.Indicator == "Y",]$HRSA_text <- "Yes"
health_clinics[health_clinics$Migrant.Health.Centers.HRSA.Grant.Subprogram.Indicator == "N",]$HRSA_text <- "No"

# Set uninsured
health_clinics$uninsured_text <- "unknown"

health_clinics  <- health_clinics %>%
  mutate(uninsured_text = if_else(Serving.Uninsured == 1,  "Yes", uninsured_text)) %>%
  mutate(uninsured_text = if_else(Serving.Uninsured == 3,  "Yes - no payment required", uninsured_text)) %>%
  mutate(uninsured_text = if_else(Serving.Uninsured == 77,  "Information unavailable", uninsured_text)) %>%
  mutate(uninsured_text = if_else(is.na(uninsured_text),  "unknown", uninsured_text))

# Recode the missing codes in Mission Statment to empty string
#health_clinics[health_clinics$Mission.Statement == "77", "Mission.Statement"] <- " "
health_clinics[["Mission.Statement"]] <- ifelse(is.na(health_clinics[["Mission.Statement"]]), "", health_clinics[["Mission.Statement"]])
health_clinics[["Mission.Statement"]] <- ifelse(health_clinics[["Mission.Statement"]]=="77", "", health_clinics[["Mission.Statement"]])

# Format Zip Codes
health_clinics$zipcode <- substr(health_clinics$Site.Postal.Code, 1, 5)

# Keep sites that provide services - Are we no longer doing this filter?
# health_clinics <- health_clinics[grepl("Service Delivery Site", health_clinics$Health.Center.Type.Description, fixed = T),]

# Exclude sites with a value in the column Exclude.from.Interactive.Map
health_clinics<- health_clinics[health_clinics$Exclude.from.Interactive.Map=="" | is.na(health_clinics$Exclude.from.Interactive.Map) ,]

# subset COLS - try to keep the same order as for legal 
health_keep_cols <- c(
  "ID",                      # need a unique id
  "Site.Name",
  "full_address",
  "Site.Telephone.Number",
  "Federally.Qualified.Health.Center.FQHC.Look.Alike.Organization.Site.Administrator.Contact.Email.Address",
  "website",
  "Mission.Statement",
  "Geocoding.Artifact.Address.Primary.X.Coordinate",
  "Geocoding.Artifact.Address.Primary.Y.Coordinate",
  "health_services",
  "language_filter",
  "service_access",
  #"Site.City",
  "zipcode"#,
  # "Migrant.Health.Centers.HRSA.Grant.Subprogram.Indicator",
  # "Serving.Uninsured",
  # "Capacity.Hours.of.Operation.per.Week"
) 

health_clinics <- health_clinics[c(health_keep_cols)]

# Rename the columns
colnames(health_clinics) <- c("ID", "org", "address", "phone", "email", "website", "mission",
                              "lon", "lat", "services", "language_support", "service_access", #"city", 
                              "zipcode"#,
                              #"HRSA", "uninsured", "capacity_hours"
                              )

#calculate missing rate
health_clinics$service_access <- gsub(health_clinics$service_access,pattern = "NA, NA, NA, NA", replacement = "") 
round(colSums(is.na(health_clinics))/nrow(health_clinics)*100,2)
#----------------------------------------------------------------------------------------
# Legal Aid Clinics
#----------------------------------------------------------------------------------------

# Read in the data
# Legal Aid clinics 
#legal_clinics <- read.csv("Clinic_Data/Legal_Aid_Clinics_Complete_Data.csv", stringsAsFactors = F, strip.white = T)
legal_clinics <-read_excel("Clinic_Data/v2/Legal_Aid_Clinics_Complete_Data_Language_Consolidated.xlsx", 
                           sheet="Legal_Aid_Clinics_Complete_v2", trim_ws = T, col_names = T)

# Standardize colnames to replace one or more non-alpha characters with a period
names(legal_clinics ) <- gsub(x = names(legal_clinics), pattern = "[^[:alnum:]]", replacement = ".") 
colnames(legal_clinics) <- gsub("\\.\\.+", ".", colnames(legal_clinics))

# Give clinics IDs
legal_clinics$ID <- 1:nrow(legal_clinics)

#######################################################
# Jitter duplicate coordinates
## So that we get data for all points at the location
#######################################################
# options(digits=9)
# legal_clinics$lon <- round(legal_clinics$lon,4) 
# legal_clinics$lat <- round(legal_clinics$lat,6)
# legal_clinics$lon_lat <- paste0(legal_clinics$lon, "_", legal_clinics$lat)
# num_occ <- as.data.frame(table(legal_clinics$lon_lat), stringsAsFactors = F)
# num_occ[num_occ$Freq > 1,]$Var1
# legal_clinics[legal_clinics$lon_lat %in% num_occ[num_occ$Freq > 1,]$Var1, ]$Site.Name
# dup_locs<- legal_clinics[legal_clinics$lon_lat %in% num_occ[num_occ$Freq > 1,]$Var1, ]


legal_clinics[c("lon","lat")]<- jitterDupCoords(legal_clinics[c("lon","lat")], min=0.00001, max=0.00009)

# Now retest for dup coords
# legal_clinics$lon_lat <- paste0(legal_clinics$lon, "_", legal_clinics$lat)
# num_occ <- as.data.frame(table(legal_clinics$lon_lat), stringsAsFactors = F)
# num_occ[num_occ$Freq > 1,]$Var1 
# legal_clinics[legal_clinics$lon_lat %in% num_occ[num_occ$Freq > 1,]$Var1, ]$Site.Name
# dup_locs<- legal_clinics[legal_clinics$lon_lat %in% num_occ[num_occ$Freq > 1,]$Var1, ]
######################################################################################
# Reformat to create website URL
legal_clinics$website <- legal_clinics$Site.Web.Address

legal_clinics  <- legal_clinics %>%
  mutate(website = if_else(website == "", legal_clinics$Site.Web.Address.SPECIFIC, website)) %>%
  mutate(website = if_else(website == "", legal_clinics$Site.Web.Address, website))

legal_clinics$website <- ifelse(substring(legal_clinics$website, 1, 4) == "http", health_clinics$website, paste0("http://", legal_clinics$website))

#####################################################################################
# CREATE STANDARDIZED LANGUAGE FILTER COLUMN
#####################################################################################
# Create a language filter string based on standardized column names with binary values
#all_legal_languages <- trimws(gsub("\\.", "/", gsub("Language.Support.", "", names(legal_clinics)[35:51])))

# First rename some columns to exclude ones we won't use in the filters
# Or to reformate the column names to automate extraction
legal_clinics <- rename(legal_clinics, 
                         Language.Form.Notes=Language.Support.Form,
                         Language.Filter.List= Language.Support.Filter, 
                         Language.Screenshot=Language.Support.Screenshot,
                         Language.SupportOriginal.Dari=Language.Support.Dari,
                         Language.SupportOriginal.Persian=Language.Support.Persian,
                         Language.SupportOriginal.Tagalog=Language.Support.Tagalog.38,
                         #Language.SupportOriginal.Taishanese=Language.Support.Toishan,
                         Language.SupportOriginal.Filipino=Language.Support.Filipino
                         )
# new language labels
legal_clinics <- rename(legal_clinics, 
                        Language.Support.Tagalog.Filipino=Language.SupportOriginal.Tagalog,
                        Language.Support.Persian.Dari.Farsi=Language.Support.Farsi
                        #Language.Support.Mandarin.Chinese=Language.Support.Mandarin,
                        #Language.Support.Cantonese=Language.Support.Cantonese.1
                        )

#all_legal_languages <- trimws(gsub("\\.", "/", gsub("Language.Support.", "", names(legal_clinics2)[grep("Language.Support.",names(legal_clinics2))])))

# Extract the names of all columns with the languages we will filter on
# and create a list of the labels we will use in the web app
all_legal_language_columns <- names(legal_clinics)[grep("Language.Support\\.",names(legal_clinics))]
all_legal_language_labels <- trimws(gsub("\\.", "/", gsub("Language.Support.", "", all_legal_language_columns)))

# Create a language lookup list mapping column names to filter labels
language_lut<- list()
for (i in 1:length(all_legal_language_columns)) {
  print(paste0("label ", all_legal_language_columns[i], " to ", all_legal_language_labels[i]))
  language_lut[[all_legal_language_columns[i]]] <- all_legal_language_labels[i]
}

# Replace NAs with empty strings
# IFF the column contains characters
for (i in sort(all_legal_language_columns)) {
  if (nrow(legal_clinics[is.na(legal_clinics[i]),]) > 0) {
    # Recode NAs to empty strings ("")
    print(i)
    legal_clinics[[i]] <- ifelse(is.na(legal_clinics[[i]]), "", legal_clinics[[i]])
    
  }
}

# Apply the language lookup to populate the language filter columns with the language label
for (i in all_legal_language_columns) {
  for (r in 1:nrow(legal_clinics)) {
    if (legal_clinics[r,i] == "Yes" || legal_clinics[r, i] == "1") {        # All rows that are marked "Yes" or "1", else NA
      legal_clinics[r,i] <- language_lut[[i]]  
    } else {
      legal_clinics[r,i] <- ""
    }
  }
}

# for (i in grep("Language.Support.",names(legal_clinics)) ) {
#   for (r in 1:nrow(legal_clinics)) {
#     if (legal_clinics[r,i] == "Yes" || legal_clinics[r, i] == "1") {        # All rows that are marked "Yes" or "1", else NA
#       legal_clinics[r,i] <- all_legal_languages[i-34]} 
#     else {legal_clinics[r,i] <- ""}
#   }
# }

#legal_clinics$language_filter <- gsub(" +", ", ", trimws(do.call(paste, c(legal_clinics[35:51], sep = " "))))

# Now concatenate the language filters into one long string
legal_clinics$language_filter <- gsub(" +", ", ", trimws(do.call(paste, c(legal_clinics[all_legal_language_columns], sep = " "))))


# Create service access filter string based on column values
legal_clinics$Require.Payment <- with(legal_clinics, 
                                          #ifelse(Require.Payment == "Yes" | Require.Payment == "1", "Payment required",
                                                 ifelse(Require.Payment == "No" | Require.Payment == "2", "No payment required", ""))
                                                        #ifelse(Require.Payment == "" | Require.Payment == "No information", "", "Free or low cost"))))

legal_clinics$Targeting.Low.Income <- with(legal_clinics,
                                           ifelse(grepl("Yes", Targeting.Low.Income, ignore.case = T) == T | Targeting.Low.Income == "1", "Targeting low income", ""))
                                                #ifelse(Targeting.Low.Income == "No" | Targeting.Low.Income == "2", "Not targeting low income", "")))

#### NEED TO CLARIFY: WHAT IS "6"/"10" AND WHY NONE HAVE "YES" 
# legal_clinics$Late.Hours.or.Weekend.Appointment <- with(legal_clinics,
#                                                         ifelse(grepl("No information", Late.Hours.or.Weekend.Appointment, ignore.case = T) == T | 
#                                                                  Late.Hours.or.Weekend.Appointment == "", "", "Late hours"))

legal_clinics$Walk.in <- with(legal_clinics,
                                 ifelse(Walk.in == "Yes" | Walk.in == "1", "Walk-ins", ""))
                                       #ifelse(Walk.in == "No" | Walk.in == "2", "Walk-ins not offered", "")))


legal_clinics$service_access <- gsub("  +", ", ", trimws(gsub("^,*|, ,|,*$", " ", 
                                     trimws(do.call(paste, c(legal_clinics[c("Require.Payment", "Targeting.Low.Income", "Walk.in")], 
                                                             sep = ", "))))))

# Grab first 5 digits of zipcode
legal_clinics$zipcode <- substr(legal_clinics$Zipcode, 1, 5)


# Exclude sites with a value in Exclude.from.interactive.map column
#legal_clinics <- legal_clinics[legal_clinics$Exclude.from.interactive.map == "Include in map",]
legal_clinics <- legal_clinics[legal_clinics$Exclude.from.interactive.map=="" | is.na(legal_clinics$Exclude.from.interactive.map) ,]

# Subset COLS
legal_keep_cols <- c(
  "ID",                     # need a unique id
  "Site.Name",
  "Address.Clean",
  "Site.Telephone.Number",
  "Email",
  "website",
  "Mission.Statement",
  "lon",
  "lat",
  "Legal.Services.Clean",
  "language_filter",
  "service_access",
  "zipcode"#,
  #"Capacity.Hours.of.Operation.per.Week"
) 

legal_clinics <- legal_clinics[c(legal_keep_cols)]

# Rename the columns
colnames(legal_clinics) <- c("ID", "org", "address", "phone", "email", "website", "mission", "lon", "lat",
                             "services", "language_support", "service_access", "zipcode"#, 
                             #"capacity_hours",
                              )




####################################################
# CLEAN UP - a bit not all
####################################################
# Clean the environment by keeping only the relevant inputs for the interactive app
rm(list = setdiff(ls(), c("health_clinics", "legal_clinics")))


####################################################
## Preload full filter menu lists
####################################################

# Capitalize the first letter in the drop down menu lists
capitalize <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  return(x)
}

#health_language_support <- capitalize(unique(unlist(strsplit(trimws(health_clinics$language_support), split = "\\, | & |/")))) ## OK
#legal_language_support <-  capitalize(unique(unlist(strsplit(trimws(legal_clinics$language_support), split = "\\, | & |/"))))  ## OK
health_language_support <- capitalize(unique(unlist(strsplit(trimws(health_clinics$language_support), split = "\\, | & ")))) ## Revised to include , "/" delimited compound values
legal_language_support <-  capitalize(unique(unlist(strsplit(trimws(legal_clinics$language_support), split = "\\, | & "))))  ## Revised to include , "/" delimited compound values

health_services <- capitalize(unique(unlist(strsplit(health_clinics$services, split = "\\, | & "))))                      ## OK
legal_services <- capitalize(trimws(unique(unlist(strsplit(trimws(legal_clinics$services), split = "\\, | & ")))))        ## OK

health_service_access <- capitalize(unique(unlist(strsplit(trimws(health_clinics$service_access), split = "\\, | & "))))   ## OK
legal_service_access <- capitalize(unique(unlist(strsplit(trimws(legal_clinics$service_access), split = "\\, | & "))))     ## OK


health_clinics$type <- "health"
legal_clinics$type <- "legal"

clinics_preload <- rbind(health_clinics, legal_clinics)

## NOTE: This extra bit gets rid of parentheses in the filter choices b/c filters return no results when a parenthesis is encountered
clinics_preload$services <- gsub("\\(|\\)", "", clinics_preload$services)
health_services <- gsub("\\(|\\)", "", health_services)
legal_services <- gsub("\\(|\\)", "", legal_services)

####################################################
# Give all clinics a UNIQUE ID
####################################################
clinics_preload$ID <- 1:nrow(clinics_preload)

### NOT NECESSARY ANYMORE!!!
# # Function to chop the clean comma separated strings of services/languages/access features into a vector of sorted elements
# filter_prep_fun <- function(sel.clinics, sel.column) {
#   x <- sort(
#     unique(
#       unlist(
#         strsplit(
#           get(paste0(sel.clinics, "_clinics"))[
#             unique(
#               unlist(
#                 strsplit(
#                   trimws(
#                     get(paste0(sel.clinics, "_clinics"))[, sel.column]
#                   ),
#                   split = "\\, "
#                 )
#               )
#             )
#             %in% get(paste0(sel.clinics, "_", sel.column)), sel.column
#             ], 
#           split = "\\, "
#         )
#       )
#     ), decreasing = F
#   )
#   
#   return(x)
# }


#############################################################
##### ADDING ADDITIONAL SHAPES TO BE PLOTTED ON THE MAP #####
#############################################################

# On hold per Jasmijn April 2019
# library(sf)
# # Read in county and place shapes
# CA.counties <- st_read(dsn = "CA_counties/CA_counties.shp", layer = "CA_counties", stringsAsFactors = F)
# CA.places <- st_read(dsn = "CA_places/tl_2018_06_place.shp", layer = "tl_2018_06_place", stringsAsFactors = F)
# 
# # Transform shape projection to UTM zone 10
# CA.counties <- st_transform(CA.counties, crs = "+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
# CA.places <- st_transform(CA.places, crs = "+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
# 
# # Simplify polygons for lighter map load time
# CA.counties <- st_simplify(CA.counties, preserveTopology = T, dTolerance = 100)
# CA.places <- st_simplify(CA.places, preserveTopology = T, dTolerance = 100)
# 
# # Transform shape projection to UTM zone 10
# CA.counties <- st_transform(CA.counties, crs = "+proj=longlat +datum=WGS84")
# CA.places <- st_transform(CA.places, crs = "+proj=longlat +datum=WGS84")
# 

####################################
# SAVE DATA for SERVICE Locator APP
####################################
# Save the environment to be read when app starts
save.image("../BIMI_MSI_SHINY/service_locator_data_processed.Rdata")
    
####################################
##### ADDING CENSUS TRACT DATA #####
####################################
     
# Read in the census data
#census_data <- read_sf("./preprocess_census_data/msi_tractdata_acs2017.shp")
load("../BIMI_MSI_SHINY/service_locator_data_processed.Rdata")
load("./preprocess_census_data/census_tract_data_processed.Rdata")

# # Simplify the tract polygons, while preserving the topology for close-up view 
##census_data <- st_transform(census_data, crs = "+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
# census_data <- st_simplify(census_data, preserveTopology = T, dTolerance = 100)
##census_data <- st_transform(census_data, crs = "+proj=longlat +datum=WGS84")

# Make clinics an sf object with same crs as census data
clinics_preload <- clinics_preload %>% 
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(census_data), remove = F)

st_crs(census_data) == st_crs(clinics_preload)

# get the census vars for each clinic and join it to the clinics data
clinics_preload <- st_join(clinics_preload, census_data)

# Remove any clinics with no GEOID - that means it is not in our counties of interest
clinics_preload <- clinics_preload[!is.na(clinics_preload$GEOID),]

# Set any NAs in the clinics data to an empty string so it displays nice
clinics_preload <- clinics_preload %>% mutate_if(is.character, ~replace_na(., ""))

# Update filter lists since some clinics may have been filtered out
## NOTE: we should just copy the above section to here and run once!
health_language_support <- capitalize(unique(unlist(strsplit(trimws(clinics_preload[clinics_preload$type=='health',]$language_support), split = "\\, | & ")))) ## Revised to include , "/" delimited compound values
legal_language_support <-  capitalize(unique(unlist(strsplit(trimws(clinics_preload[clinics_preload$type=='legal',]$language_support), split = "\\, | & "))))  ## Revised to include , "/" delimited compound values

health_services <- capitalize(unique(unlist(strsplit(clinics_preload[clinics_preload$type=='health',]$services, split = "\\, | & "))))                      ## OK
legal_services <- capitalize(trimws(unique(unlist(strsplit(trimws(clinics_preload[clinics_preload$type=='legal',]$services), split = "\\, | & ")))))        ## OK

health_service_access <- capitalize(unique(unlist(strsplit(trimws(clinics_preload[clinics_preload$type=='health',]$service_access), split = "\\, | & "))))   ## OK
legal_service_access <- capitalize(unique(unlist(strsplit(trimws(clinics_preload[clinics_preload$type=='legal',]$service_access), split = "\\, | & "))))     ## OK

#######################################################
# CLEAN UP - all but clinics_preload and select lists
#######################################################
# Clean the environment by keeping only the relevant inputs for the interactive app

keep_objects <- c("census_data","clinics_preload", 
                   "health_language_support","health_service_access", "health_services", 
                   "legal_language_support", "legal_service_access","legal_services")

rm(list = setdiff(ls(), keep_objects))
####################################
# SAVE DATA FOR CENSUS APP
####################################
# Save the environment to be read when app starts
save.image("../CENSUS/census_app_data_processed.Rdata")

# Remove all objects
rm(list = ls())
