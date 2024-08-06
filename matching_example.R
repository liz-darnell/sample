# INDIVIDUAL HEADER FILE (from FEC)
indiv_header_file <- read_csv("/indiv_header_file.csv")

# LIST OF FILES IN FOLDER 
files <- list.files("/indiv24 (2)/by_date", 
                    pattern = "\\.txt$", all.files = TRUE, full.names = TRUE)

#EMPTY CONTAINER FOR THE PROCESSED DATA
data_container <- list()

#READ ALL FILES IN FOLDER
for (i in seq_along(files)) {
  data_container[[i]] <- read_delim(files[i], "|", escape_double = FALSE, 
                                    col_names = FALSE, trim_ws = TRUE)
}

#ADD HEADERS
for (i in seq_along(data_container)) {
  colnames(data_container[[i]]) <- colnames(indiv_header_file)
}

#CHECK COLUMN TYPES
col_types_list <- list()

for (i in seq_along(data_container)) {
  col_types <- sapply(data_container[[i]], typeof)
  col_types_list[[i]] <- col_types
}

for (i in 15) {
  print(col_types_list[[i]])
}

#CREATING DF FOR EACH LIST

a <- data.frame(data_container[1])
b <- data.frame(data_container[2])
c <- data.frame(data_container[3])
d <- data.frame(data_container[4])
e <- data.frame(data_container[5])
f <- data.frame(data_container[6])
g <- data.frame(data_container[7])
h <- data.frame(data_container[8])
i <- data.frame(data_container[9])
j <- data.frame(data_container[10])
k <- data.frame(data_container[11])
l <- data.frame(data_container[12])
m <- data.frame(data_container[13])
n <- data.frame(data_container[14])
o <- data.frame(data_container[15])

#FIXING COL TYPE ISSUES

g$TRANSACTION_DT <- as.character(g$TRANSACTION_DT)
h$TRANSACTION_DT <- as.character(h$TRANSACTION_DT)
i$TRANSACTION_DT <- as.character(i$TRANSACTION_DT)
o$TRANSACTION_DT <- as.character(o$TRANSACTION_DT)

o$TRANSACTION_TP <- as.character(o$TRANSACTION_TP)

o$ZIP_CODE <- as.character(o$ZIP_CODE)

#SEPERATING NAMES
a <- separate(a, "NAME", into = c("Last Name", "First Name"), sep = ", ")
b <- separate(b, "NAME", into = c("Last Name", "First Name"), sep = ", ")
c <- separate(c, "NAME", into = c("Last Name", "First Name"), sep = ", ")
d <- separate(d, "NAME", into = c("Last Name", "First Name"), sep = ", ")
e <- separate(e, "NAME", into = c("Last Name", "First Name"), sep = ", ")
f <- separate(f, "NAME", into = c("Last Name", "First Name"), sep = ", ")
g <- separate(g, "NAME", into = c("Last Name", "First Name"), sep = ", ")
h <- separate(h, "NAME", into = c("Last Name", "First Name"), sep = ", ")
i <- separate(i, "NAME", into = c("Last Name", "First Name"), sep = ", ")
j <- separate(j, "NAME", into = c("Last Name", "First Name"), sep = ", ")
k <- separate(k, "NAME", into = c("Last Name", "First Name"), sep = ", ")
l <- separate(l, "NAME", into = c("Last Name", "First Name"), sep = ", ")
m <- separate(m, "NAME", into = c("Last Name", "First Name"), sep = ", ")
n <- separate(n, "NAME", into = c("Last Name", "First Name"), sep = ", ")
o <- separate(o, "NAME", into = c("Last Name", "First Name"), sep = ", ")

#adding full name 
a <- a %>% mutate(full_name = paste(`First Name`, `Last Name`, sep = " "))
b <- b %>% mutate(full_name = paste(`First Name`, `Last Name`, sep = " "))
c <- c %>% mutate(full_name = paste(`First Name`, `Last Name`, sep = " "))
d <- d %>% mutate(full_name = paste(`First Name`, `Last Name`, sep = " "))
e <- e %>% mutate(full_name = paste(`First Name`, `Last Name`, sep = " "))
f <- f %>% mutate(full_name = paste(`First Name`, `Last Name`, sep = " "))
g <- g %>% mutate(full_name = paste(`First Name`, `Last Name`, sep = " "))
h <- h %>% mutate(full_name = paste(`First Name`, `Last Name`, sep = " "))
i <- i %>% mutate(full_name = paste(`First Name`, `Last Name`, sep = " "))
j <- j %>% mutate(full_name = paste(`First Name`, `Last Name`, sep = " "))
k <- k %>% mutate(full_name = paste(`First Name`, `Last Name`, sep = " "))
l <- l %>% mutate(full_name = paste(`First Name`, `Last Name`, sep = " "))
m <- m %>% mutate(full_name = paste(`First Name`, `Last Name`, sep = " "))
n <- n %>% mutate(full_name = paste(`First Name`, `Last Name`, sep = " "))
o <- o %>% mutate(full_name = paste(`First Name`, `Last Name`, sep = " "))

#CANDIDATE TO COMMITTEE LIST 
#candidate committee linkage
ccl_header_file <- read_csv("/Users/lizdarnell/Desktop/FEC Pull July 2024/ccl_header_file.csv")

ccl <- read_delim("/Users/lizdarnell/Desktop/FEC Pull July 2024/ccl.txt", 
                  "|", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

colnames(ccl) <- colnames(ccl_header_file)

#candidate file
cn_header_file <- read_csv("/Users/lizdarnell/Desktop/FEC Pull July 2024/cn_header_file.csv")

cn <- read_delim("/Users/lizdarnell/Desktop/FEC Pull July 2024/cn.txt", 
                 "|", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

colnames(cn) <- colnames(cn_header_file)

#creating link file 
link <- left_join(cn, ccl, by = "CAND_ID") 

link <- link %>% select(CAND_ID, CAND_NAME, CAND_PTY_AFFILIATION, 
                        CAND_ELECTION_YR.x, CAND_OFFICE, CAND_ST, CMTE_ID,
                        CMTE_TP, FEC_ELECTION_YR, LINKAGE_ID)

#UPLOAD MEMBERSHIP LIST 
member_edit <- read_csv("/match_edit.csv")
member <- read_csv("/match.csv")

#minor data cleaning 
member_edit <- member_edit %>% 
               mutate(full_name = paste(`First Name`, `Last Name`, sep = " "))

member <- member %>% 
          mutate(full_name = paste(`First Name`, `Last Name`, sep = " "))

member$`First Name` <- member$`First Name`%>% toupper()
member$`Last Name` <- member$`Last Name`%>% toupper()
member$full_name <- member$full_name %>% toupper()
################################################################################
full_frame <- bind_rows(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

tight_match <- inner_join(full_frame, member, by = "full_name", relationship = "many-to-many")

tight_match_reorder <- tight_match %>% select(full_name, `First Name.x`, `First Name.y`,
                                              `Last Name.x`, `Last Name.y`, CITY, 
                                              City, STATE, State, everything())


tight_match_state <- tight_match_reorder %>% filter(STATE == State) 

tight_match_state$TRANSACTION_DT <- as.Date(tight_match_state$TRANSACTION_DT, format = "%m%d%Y") 

tight_match_state <- tight_match_state %>% filter(TRANSACTION_DT >= '2024-04-01')

tight_match_sum <- tight_match_state %>% group_by(full_name) %>% summarise(total_amt = sum(TRANSACTION_AMT))

tight_match_cmte <- left_join(tight_match_state, link, by = "CMTE_ID", 
                              relationship = "many-to-many") 
################################################################################
tight_match_cmte <- tight_match_cmte %>% select(full_name, CITY, STATE, ZIP_CODE,
                                                EMPLOYER, OCCUPATION, 
                                                `Professional Affiliation`, CMTE_ID,
                                                CAND_ID, CAND_NAME, CAND_PTY_AFFILIATION,
                                                CAND_ELECTION_YR.x, CAND_OFFICE, TRANSACTION_DT, 
                                                TRANSACTION_AMT, CMTE_TP, TRAN_ID)
################################################################################
write.csv(tight_match_cmte, "tight_match_cmte_a.csv")
write.csv(tight_match_sum, "tight_match_sum_a.csv")
