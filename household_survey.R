library(stringr)
library(stringdist)

#################################################################################
#                           HOUSEHOLD DATA
#################################################################################

household_survey1 <- read.csv("/home/tumaini/Desktop/IHI-Projects/ACOBISREM/Exported ODK Data/Household Survey.csv",stringsAsFactors = FALSE)
household_survey2 <- read.csv("/home/tumaini/Desktop/IHI-Projects/ACOBISREM/Exported ODK Data/Household Survey Version 2.csv", stringsAsFactors = FALSE)
household_survey3 <- read.csv("/home/tumaini/Desktop/IHI-Projects/ACOBISREM/Exported ODK Data/Household Survey Version 3.csv", stringsAsFactors = FALSE)

household_survey1$householdInfo.houseHead <- NULL

household_survey_list1 <- read.csv("/home/tumaini/Desktop/IHI-Projects/ACOBISREM/Exported ODK Data/Household Survey_hhListingGrp_memberRpt.csv",stringsAsFactors = FALSE)
household_survey_list2 <- read.csv("/home/tumaini/Desktop/IHI-Projects/ACOBISREM/Exported ODK Data/Household Survey Version 2_hhListingGrp_memberRpt.csv",stringsAsFactors = FALSE)
household_survey_list3 <- read.csv("/home/tumaini/Desktop/IHI-Projects/ACOBISREM/Exported ODK Data/Household Survey Version 3_hhListingGrp_memberRpt.csv",stringsAsFactors = FALSE)

household_survey_list1$member_no <- ifelse(str_length(household_survey_list1$member) > 10,
                                           household_survey_list1$member,
                                           household_survey_list1$memberIdManual)

household_survey_list2$member_no <- household_survey_list2$memberIdManual
household_survey_list3$member_no <- household_survey_list3$memberIdManual



household_survey_list1$member <- NULL
household_survey_list1$memberIdDisplay <- NULL

household <- rbind(household_survey1,household_survey2,household_survey3)
household$household_id <- ifelse(household$householdInfo.hhId == "99",household$householdInfo.hhIdManual,
                                 household$householdInfo.hhId)
    
household_list <- rbind(household_survey_list1,household_survey_list2,household_survey_list3)



write.csv(household,file = 
              "/home/tumaini/Desktop/IHI-Projects/ACOBISREM/ACOBIS/household.csv",
          row.names = FALSE)
write.csv(household_list,file = 
              "/home/tumaini/Desktop/IHI-Projects/ACOBISREM/ACOBIS/participants.csv",
          row.names = FALSE)
# browser()
# household_duplicates <- household[duplicated(household$household_id) & household$household_id != "",]
# members_duplicates <- household_list[duplicated(household_list$member_no) & household_list$member_no != "",]

household_denominator <- nrow(household)
individual_denominator <- nrow(household_list)

# Remove unecessary variables from workspace
rm(household_survey1,household_survey2,household_survey3,household_survey_list1,
   household_survey_list2, household_survey_list3)

# Merge ohousehold data with member data 
household_dataset <- merge(household,household_list,by.x="KEY",by.y="PARENT_KEY")
household_dataset <- as.data.frame(sapply(household_dataset, toupper))

#####################################################################################
#                            LAB DATA
#####################################################################################

beatha_lab_forms <- read.csv("/home/tumaini/Desktop/IHI-Projects/ACOBISREM/ACOBIS/ACOBIS_Beatha Data/Lab forms ACOBIS/Acobisrem Lab form.txt", sep = ";", stringsAsFactors = FALSE)
zay_lab_forms <- read.csv("/home/tumaini/Desktop/IHI-Projects/ACOBISREM/ACOBIS/ACOBIS_Zay Data/Acobisrem Lab form.txt", sep = ";")

write.csv(zay_lab_forms,file = 
              "/home/tumaini/Desktop/IHI-Projects/ACOBISREM/ACOBIS/beatha_lab_forms.csv",
          row.names = FALSE)
write.csv(beatha_lab_forms,file = 
              "/home/tumaini/Desktop/IHI-Projects/ACOBISREM/ACOBIS/beatha_lab_forms.csv",
          row.names = FALSE)

#names(beatha_lab_forms)
new.names<- c("form_no","hamlet","hamlet_suffix","household_no","participant","sample_taken","interviewer",
              "sample_read","technician","pfalci_asex","pfalci_gamet","pmal_asex","pmal_gamet","other_asex",
              "other_gamet","parasite_asex","parasite_gamete","haem_integer","haem_decimal",
              "interviewer_comments","lab_comments")

# create a copy with the new variable names
beatha_lab_final <- beatha_lab_forms
names(beatha_lab_final) <- new.names
zay_lab_final <- zay_lab_forms
names(zay_lab_final) <- new.names

# Clean hamlet_suffix variable which ocasionally has 2 characters rather than 1
zay_lab_final$hamlet_suffix_fixed <- substr(zay_lab_final$hamlet_suffix,1,1)
beatha_lab_final$hamlet_suffix_fixed <- substr(beatha_lab_final$hamlet_suffix,1,1)

# Change household_no to character so it can accept leading zeros
zay_lab_final$household_no <- as.character(zay_lab_final$household_no)
beatha_lab_final$household_no <- as.character(beatha_lab_final$household_no)

zay_lab_final$household_no_fixed <- str_pad(zay_lab_final$household_no, side = "left", pad = "0", width = 9)
beatha_lab_final$household_no_fixed <- str_pad(beatha_lab_final$household_no, side = "left", pad = "0", width = 9)


# Add a 'individual_id' column
beatha_lab_final$individual_id <- paste(beatha_lab_final$hamlet,beatha_lab_final$hamlet_suffix_fixed,
                                        beatha_lab_final$household_no_fixed,sep ='')
zay_lab_final$individual_id <- paste(zay_lab_final$hamlet,zay_lab_final$hamlet_suffix_fixed,
                                     zay_lab_final$household_no_fixed,sep ='')

# Append 2 separate data sets to form one
lab_dataset <- rbind(zay_lab_final,beatha_lab_final)


# Convert characters to upper case
lab_dataset <- as.data.frame(sapply(lab_dataset, toupper))

write.csv(lab_dataset,file = 
              "/home/tumaini/Desktop/IHI-Projects/ACOBISREM/ACOBIS/lab_data.csv",
          row.names = FALSE)


# Remove unecessary variables from workspace
rm(zay_lab_final, zay_lab_forms,beatha_lab_final,beatha_lab_forms)

#######################################################################################
#                           MERGE LAB WITH HOUSEHOLD
#######################################################################################

household_with_lab <- merge(household_dataset,lab_dataset,by.x="member_no",by.y="individual_id")
household_with_lab$dob <- as.Date(household_with_lab$dob, format = "%d %b %Y")
household_with_lab$formInfo.interviewDate <- as.Date(household_with_lab$formInfo.interviewDate,
                                                     format = "%d %b %Y")

write.csv(household_with_lab,file = 
              "/home/tumaini/Desktop/IHI-Projects/ACOBISREM/ACOBIS/analytical_data2.csv",
          row.names = FALSE)


#######################################################################################
#                   MORE DATA CLEANING AND TRANSCRIPTION ERROR RECOVERY
#######################################################################################
# Create dataset of unlinked rows for data cleaning
unlinked_data <- subset(lab_dataset,!(lab_dataset$individual_id %in% household_with_lab$member_no))

# Minimise dataset keeping only necessary columns for ease of manipulation
unlinked_data <- unlinked_data[c("form_no","participant","individual_id","sample_taken")]
unlinked_data$individual_id <- as.character(unlinked_data$individual_id)
# Standardize date format
unlinked_data$sample_taken <- as.Date(unlinked_data$sample_taken, format = "%d/%m/%Y")
# Minimised members dataset keeping only necessary  columns for ease of manipulation
all_members <- household_list[c("PARENT_KEY","member_no","memberFName","memberMName","memberLName")]


fuzzy_matches <<- NULL

for(row_no in 1:nrow(unlinked_data)){
    
    row_data <- unlinked_data[row_no,]
    row_data$form_no <- as.character(row_data$form_no)
    row_data$participant <- as.character(row_data$participant)
    #row_data$sample_taken <- as.Date.character(row_data$sample_taken, format = "d/m/Y" )
    row_data$individual_id <- as.character(row_data$individual_id)
    
    # Blocking to reduce search space
    block <- subset(all_members, substr((row_data$individual_id),1,3) == 
                        substr(all_members$member_no,1,3))
    
    if(nrow(block) > 0){
        block$interview_date = ""
        for(item in 1:nrow(block)){
            parent <- as.character(block[item,]["PARENT_KEY"])
            block[item,]$interview_date <- subset(household$formInfo.interviewDate,household$KEY == parent)
        }
        
        
        possible_matches <- subset(block,stringdist(block$member_no, row_data$individual_id) < 3)
        
        target <- row_data$participant
        target_id <- row_data$individual_id
        lab_sample_taken <- row_data$sample_taken
        field_sample_taken <- 
            
            if(nrow(possible_matches) > 0){
                possible_matches <- cbind(possible_matches,target, target_id, lab_sample_taken)
                # Flag used later during clerical review to mark whether a row matches or not (1/0)
                possible_matches$is_match <- 0
                
                fuzzy_matches <<- rbind(fuzzy_matches,possible_matches)
            }
    }    
    
}

fuzzy_matches$interview_date <- as.Date(fuzzy_matches$interview_date, format = "%d %b %Y")

# Filter even further to get only those possible matches where lab sample taken equals interview date
fuzzy_matches_minimized <- subset(fuzzy_matches,fuzzy_matches$interview_date == 
                                      fuzzy_matches$lab_sample_taken)

write.csv(fuzzy_matches_minimized,file = 
              "/home/tumaini/Desktop/IHI-Projects/ACOBISREM/ACOBIS/fuzzy_matches.csv",
          row.names = FALSE)

####################################################################################
# PAUSE HERE FOR MANUAL CLERICAL REVIEW OF fuzzy_matches.csv. IN THE FILE, WHEN A 
# MATCH IS OBSERVED, SET THE FIELD "is_match" TO 1.
####################################################################################

browser()

######################################################################################
# CONTINUE
######################################################################################
corrected <- read.csv("/home/tumaini/Desktop/IHI-Projects/ACOBISREM/ACOBIS/fuzzy_matches3.csv",
                      stringsAsFactors = FALSE)
corrected <- subset(corrected, corrected$is_match == 1)

lab_dataset$individual_id <- as.character(lab_dataset$individual_id)

for(corrected_row in 1:nrow(corrected)){
    
    #What it did use
    old_id <- corrected[corrected_row,]$target_id
    # What it should have used
    new_id <- corrected[corrected_row,]$member_no
    
    tryCatch({
        lab_dataset[lab_dataset$individual_id == old_id,]$individual_id <- new_id
        
    },
    error = function(err) {   
        print(paste("MY_ERROR:  ",err))
        # Choose a return value in case of error
    })
}

household_with_lab <- merge(household_dataset,lab_dataset,by.x="member_no",by.y="individual_id")
household_with_lab$dob <- as.Date(household_with_lab$dob, format = "%d %b %Y")
household_with_lab$formInfo.interviewDate <- as.Date(household_with_lab$formInfo.interviewDate,
                                                     format = "%d %b %Y")

household_with_lab$age <- as.integer(household_with_lab$age)
household_with_lab$ageApprox <- as.integer(household_with_lab$ageApprox)
household_with_lab$analysis_age <- ifelse(household_with_lab$knowDob == 1,
                                         floor(as.numeric(household_with_lab$formInfo.interviewDate -
                                                         household_with_lab$dob)/365.25),
                                         ifelse(household_with_lab$knowAge == 1, household_with_lab$age, household_with_lab$ageApprox))

household_with_lab$age_grp <- ifelse(household_with_lab$analysis_age < 5,"0-4",
                                     ifelse(4 < household_with_lab$analysis_age & 
                                                household_with_lab$analysis_age < 15,"5-14",
                                            ifelse(14 < household_with_lab$analysis_age & 
                                                       household_with_lab$analysis_age < 60,"15-59","60+")
                                            )
                                     )

household_with_lab$parasite_asex <- as.integer(as.character(household_with_lab$parasite_asex))
household_with_lab$parasite_asex2 <- ifelse(household_with_lab$parasite_asex > 0, 1 , 0)

############################################
#  PAUSE
browser()
############################################

household_with_lab$studyInfo.ward <- as.character(household_with_lab$studyInfo.ward)
household_with_lab$studyInfo.ward <- ifelse(household_with_lab$formInfo.hamletLeaderLName %in% 
       (subset(household_with_lab, household_with_lab$studyInfo.ward == "WARD")[,"formInfo.hamletLeaderLName"]),
       "CHUMBI",household_with_lab$studyInfo.ward)

household_with_lab$cohort <- ifelse(household_with_lab$studyInfo.ward %in% 
                                    toupper(c("chumbi","Ikwiriri","Mgomba","Umwe")),"intervention",
                                    ifelse(household_with_lab$studyInfo.ward %in% toupper(c("Kibiti","Bungu")),"control","None"))

# Dealing with village values of "99"
household_with_lab$studyInfo.village <- as.character(household_with_lab$studyInfo.village)
household_with_lab$studyInfo.villageManual <- as.character(household_with_lab$studyInfo.villageManual)

household_with_lab$studyInfo.villageManual <- ifelse(household_with_lab$studyInfo.villageManual == "CHUMBI.A","CHUMBI A",
                                                     ifelse(household_with_lab$studyInfo.villageManual =="KIBITI.A","KIBITI A",
                                                            ifelse(household_with_lab$studyInfo.villageManual == "KIBITI,A", "KIBITI A",
                                                                   ifelse(household_with_lab$studyInfo.villageManual == "KIWANGA MJINI","KIWANGA",
                                                                          ifelse(household_with_lab$studyInfo.villageManual == "KIBITI B", "KIBITI KATI",
                                                                                 ifelse(household_with_lab$studyInfo.villageManual =="KIBITI A","KIBITI KUSINI",
                                                                                        household_with_lab$studyInfo.villageManual))
                                                                          ))))
browser()
household_with_lab$studyInfo.village <- ifelse(household_with_lab$studyInfo.village == 99,household_with_lab$studyInfo.villageManual,
                                               household_with_lab$studyInfo.village)

household_with_lab$study_site <- ifelse(household_with_lab$studyInfo.ward == "UMWE","IKWIRIRI",
                                        ifelse(household_with_lab$studyInfo.ward == "MGOMBA","IKWIRIRI",
                                               household_with_lab$studyInfo.ward))


write.csv(household_with_lab,file = 
              "/home/tumaini/Desktop/IHI-Projects/ACOBISREM/ACOBIS/analytical_data3.csv",
          row.names = FALSE)