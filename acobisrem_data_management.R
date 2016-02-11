beatha_lab_forms <- read.csv("/home/tumaini/Desktop/IHI-Projects/ACOBISREM/ACOBIS/ACOBIS_Beatha Data/Lab forms ACOBIS/Acobisrem Lab form.txt", sep = ";", stringsAsFactors = FALSE)
zay_lab_forms <- read.csv("/home/tumaini/Desktop/IHI-Projects/ACOBISREM/ACOBIS/ACOBIS_Zay Data/Acobisrem Lab form.txt", sep = ";")
    
    
names(beatha_lab_forms)
new.names<- c("form_no","hamlet","hamlet_suffix","household_no","participant","sample_taken","interviewer",
              "sample_read","technician","pfalci_asex","pfalci_gamet","pmal_asex","pmal_gamet","other_asex",
              "other_gamet","parasite_asex","parasite_gamete","haem_integer","haem_decimal",
              "interviewer_comments","lab_comments")

# create a copy with the new variable names
beatha_lab_final <- beatha_lab_forms
names(beatha_lab_final) <- new.names
zay_lab_final <- zay_lab_forms
names(zay_lab_final) <- new.names

# Add a 'individual_id' column
beatha_lab_final$individual_id <- paste(beatha_lab_final$hamlet,beatha_lab_final$hamlet_suffix,
                                          beatha_lab_final$household_no,sep ='')
zay_lab_final$individual_id <- paste(zay_lab_final$hamlet,zay_lab_final$hamlet_suffix,
                                     zay_lab_final$household_no,sep ='')

# Append 2 separate data sets to form one
lab_data <- rbind(zay_lab_final,beatha_lab_final)

