#this is for a client that I frequently get lists for and then need to upload into a CRM. 
#the lists are never in consistent formats and the CRM is very specific in appropriate formats for uploading. 
#this is the code that I use to format those lists so I can more quickly get them uploaded into the CRM. 

name <- read.csv("Desktop/name.csv")

colnames(name)

name_filt <- name %>% select(PERM_INDIVIDUAL_ID, FNAME, MIDDLE_INI, LNAME, MOBILE_NUM, 
                         PHONE, PHONE_NUM, Appended.Phone, ETHNIC_INFER, ADDRESS, 
                         MCITY, STATE, ZIP5, VOTER_ID, BIRTHDATE, SEX) 

name_filt <- name_filt %>% rename(first_name = FNAME, middle_name = MIDDLE_INI, 
                              last_name = LNAME, cell_phone = MOBILE_NUM, 
                              phone_number = PHONE, other_phone = PHONE_NUM, 
                              work_phone = Appended.Phone, dob = BIRTHDATE, 
                              address = ADDRESS, city = MCITY, zip = ZIP5) %>% 
           replace(is.na(name_filt), "")
unique(name_filt$ETHNIC_INFER)  
#when ETHNIC_INFER = c("B", "H")
name_black_voter <- name_filt %>% filter(ETHNIC_INFER == "B" & VOTER_ID != "")
name_black_nv <- name_filt %>% filter(ETHNIC_INFER == "B" & VOTER_ID == "")
name_hisp_voter <- name_filt %>% filter(ETHNIC_INFER == "H" & VOTER_ID != "")
name_hisp_nv <- name_filt %>% filter(ETHNIC_INFER == "H" & VOTER_ID == "")

data_list <- list(name_black_voter, name_black_nv, name_hisp_voter, name_hisp_nv)
file_names <- paste0("Desktop/name_", c("black_voter", "black_nv", "hisp_voter", "hisp_nv"), ".csv")

for (i in seq_along(data_list)) {
  write.csv(data_list[[i]], file_names[i], row.names = FALSE)
}
