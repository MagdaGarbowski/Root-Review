# Code to sift through Web of Science results for Root Review 
# October 21, 2019 
# Goals
# (1) Remove obvious "drops" based on keywords
# (2) Score remaining papers based on keywords in Titles, Abstracts, and keywords 

setwd("~/RootTraits_Review/")
filedirectory = "WoS_downloads/"
file_list = list.files(path=filedirectory, pattern="\\.csv", full.names = TRUE)

ll = lapply(file_list, read.csv, stringsAsFactor = FALSE)
ll = do.call(rbind, ll)

Lit_data<-ll
write.csv(Lit_data, "Lit_data.csv")

# MATT - START HERE 

# Lit_data<-read.csv(file.choose())

# Agriculture drop words 
Drop_keywords<-"agric.*|crop.*|agron.*|bean.*|rice| pea | fruit | corn | maize | zea | alfalfa | tomato | strawberr.*| lettuce | sorghum | salvia | oat |.*nut | nut | bananna | wheat | cereal.* | grain.* | citrus | learning | viti.*| grape.*|
phytorem.*|toxic.*|mutant|QTL|mutant|plantation|orchard|climax| old growth |pathogen|infection| waste water | simulation | aquatic | pond | OTU "

## Matt's function 
#grepl_litsearch_function_ME<-function(terms, data,
#                                   cols = c("TI", "AB", "ID", "SO"))
#{
#  ans = lapply(data[,cols], function(x) grepl(terms, x, ignore.case = TRUE))
#  Reduce(`|`, ans)
#}

## Magda's function 
grepl_litsearch_function<-function(terms, data){
  grepl(terms, data$TI, ignore.case = TRUE) | grepl(terms, data$AB, ignore.case = TRUE) | grepl(terms, data$ID, ignore.case = TRUE) | grepl(terms, data$SO, ignore.case = TRUE)
}

Lit_data$Dropped<-grepl_litsearch_function(Drop_keywords, Lit_data[,c("TI","AB","ID","SO")])

Lit_data_keep<-Lit_data[(Lit_data$Dropped%in%"FALSE"),]
Lit_data_drop<-Lit_data[(Lit_data$Dropped%in%"TRUE"),]

### MATT - here is where I am doing some checking for references 
# e.g. 
#grep("genomics", Lit_data_drop$AB) # Why is this filtered out? None of the Drop_keywords are in the Title, Abstract, ID or source (SO)

#
#
#
#
#
#
#
#
#
#

# On Lit_data_keep find words related to restoration, ecology, and select topics (drought, erosion, carbon, invasion/competition)
## Add root key words from abstract 


# Restoraiton and ecology KEEP words 
Keep_words_restoration<-"restor.* |reveg.* |re-veg.* |reclaim.* |rehab.*|seedling"
Keep_words_ecology<-"ecol.*|ecosystem process.*|evolution|adapt.*"

# Topics KEEP words 
Keep_words_topics_drought<-"drought|precip.*|rain.*"
Keep_words_topics_erosion<-"erosi.*|soil stabil.* |bank stabil.*"
Keep_words_topics_carbon<-"carbon sequest* |sequest.* |aggregat.*|carbon storage"
Keep_words_topics_invasion_comp<-"invasi.* | compet.*"
Keep_words_topics_mic_associations<-"mycorrhiz|rhizob"

# Create columns for all KEEP categories 
Lit_data_keep$Keep_words_restoration<-grepl_litsearch_function(Keep_words_restoration, Lit_data_keep) * 3
Lit_data_keep$Keep_words_ecology<-grepl_litsearch_function(Keep_words_ecology, Lit_data_keep) * 1
Lit_data_keep$Keep_words_topics_drought<-grepl_litsearch_function(Keep_words_topics_drought, Lit_data_keep) * 1 
Lit_data_keep$Keep_words_topics_erosion<-grepl_litsearch_function(Keep_words_topics_erosion, Lit_data_keep) * 1
Lit_data_keep$Keep_words_topics_carbon<-grepl_litsearch_function(Keep_words_topics_carbon, Lit_data_keep) * 1
Lit_data_keep$Keep_words_topics_carbon<-grepl_litsearch_function(Keep_words_topics_carbon, Lit_data_keep) * 1
Lit_data_keep$Keep_words_topics_invasion_comp<-grepl_litsearch_function(Keep_words_topics_invasion_comp, Lit_data_keep) * 1
Lit_data_keep$Keep_words_topics_mic_associations<-grepl_litsearch_function(Keep_words_topics_mic_associations, Lit_data_keep) *1

Lit_data_keep$KeepScore<- rowSums(Lit_data_keep[,grep("Keep",colnames(Lit_data_keep))])

write.csv(Lit_data_keep, "Lit_Data_keep_10232019.csv")
write.csv(Lit_data_drop, "Lit_data_drop_10232019.csv")


### Search Lit_data for authors 
View(grep("Leger", Lit_data_keep$AU, value = TRUE))
View(grep("Fitter",Lit_data_keep$AU, value = TRUE))
View(grep("Brown",Lit_data_keep$AU, value = TRUE))
