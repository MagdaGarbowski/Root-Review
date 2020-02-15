### Attempt at summary statistics with root review data for Drought 
# Goals: 
# How many unique studies? 
# (1) Figure with which traits are measured most (all references). Traits on X axis, # of references on Y axis 
# (1) Of those traits on opposing axis, which traits have been linked to survival, establishment or biomass and 
# How many studies link traits to other response variable...survival/aboveground bimass? 
setwd("/Users/MagdaGarbowski/RootTraits_Review/")
library(ggplot2)
library(plyr)

Root_data<-read.csv("DataforSummaries/RootReview_Drought_DataforSummaries.csv", header = TRUE, na.strings = c("","NA"))
Root_data_drought<-Root_data
Root_data_drought$Growth.Form.Sum<-paste(Root_data_drought$Growth.Form1,
                                         Root_data_drought$Growth.Form.2, 
                                         Root_data_drought$Growth.Form.3, sep = ",")
Root_data_drought$Growth.Form.Sum<-gsub(",NA,NA", "", Root_data_drought$Growth.Form.Sum)
Root_data_drought$Growth.Form.Sum<-gsub(",NA", "", Root_data_drought$Growth.Form.Sum)

unique_refs<-Root_data_drought[!duplicated(Root_data_drought$AU), ]

#View(table(unique_refs$Growth.Form.Sum))
#View(Root_data_drought)

Root_data_drought_min<-Root_data_drought[,c("AU","PY","TI","Restoration.Goal.1","Restoration.Goal.2","Response.category","Is.the.root.trait.s..the.independent.or.dependent.variable.","Correlated.response.variable.",
                                            "Variable","CR_Direction_1","Positively_correlated","Positively_correlated.1","Negatively_correlated",
                                            "Correlated.response.variable..1","Variable.1","Positively_correlated.2","Positively_correlated.3", "Negatively_correlated.1","Negatively_correlated.2","Negatively_correlated.3",
                                            "X","Other.correlated.traits","Aboveground_traits", "Growth.Form1","Growth.Form.2","Growth.Form.3","Growth.Form.Sum",
                                            "RootTrait_1","Trait1_Direction","RootTrait_2","Trait2_Direction","RootTrait_3","Trait3_Direction","RootTrait_4","Trait4_Direction","RootTrait_5","Trait5_Direction",
                                            "RootTrait_6","Trait6_Direction","RootTrait_7","Trait7_Direction","RootTrait_8","Trait8_Direction","RootTrait_9","Trait9_Direction","RootTrait_10","Trait10_Direction", 
                                            "AbovegroundTraits")]

Root_columns<-c("RootTrait_1","RootTrait_2","RootTrait_3","RootTrait_4","RootTrait_5","RootTrait_6","RootTrait_7","RootTrait_8","RootTrait_9","RootTrait_10")
Root_direction_columns<-c("Trait1_Direction", "Trait2_Direction", "Trait3_Direction","Trait4_Direction", "Trait5_Direction", "Trait6_Direction","Trait7_Direction", "Trait8_Direction", "Trait9_Direction", "Trait10_Direction")
Root_and_Direction_columns<-c("RootTrait_1","Trait1_Direction","RootTrait_2","Trait2_Direction","RootTrait_3","Trait3_Direction","RootTrait_4","Trait4_Direction","RootTrait_5","Trait5_Direction",
                              "RootTrait_6","Trait6_Direction","RootTrait_7","Trait7_Direction","RootTrait_8","Trait8_Direction","RootTrait_9","Trait9_Direction","RootTrait_10","Trait10_Direction")


Root_data_drought_min[Root_columns]<-as.data.frame(lapply(Root_data_drought_min[Root_columns], gsub, pattern = "Fine root to root biomass", replacement = "Fine root ratio"))
Root_data_drought_min[Root_columns]<-as.data.frame(lapply(Root_data_drought_min[Root_columns], gsub, pattern = "Root mass fraction|Root to shoot ratio", replacement = "RSR or RMF"))

Root_data_drought_min[Root_and_Direction_columns]<-sapply(Root_data_drought_min[Root_and_Direction_columns], as.character)

# Paste together root trait with response 
# Will then reshape based on this column and split to get seperate traits & responses 

Root_data_drought_min$RootT1wResponse<-paste(Root_data_drought_min$RootTrait_1, Root_data_drought_min$Trait1_Direction, sep = "_")
Root_data_drought_min$RootT2wResponse<-paste(Root_data_drought_min$RootTrait_2, Root_data_drought_min$Trait2_Direction, sep = "_")
Root_data_drought_min$RootT3wResponse<-paste(Root_data_drought_min$RootTrait_3, Root_data_drought_min$Trait3_Direction, sep = "_")
Root_data_drought_min$RootT4wResponse<-paste(Root_data_drought_min$RootTrait_4, Root_data_drought_min$Trait4_Direction, sep = "_")
Root_data_drought_min$RootT5wResponse<-paste(Root_data_drought_min$RootTrait_5, Root_data_drought_min$Trait5_Direction, sep = "_")
Root_data_drought_min$RootT6wResponse<-paste(Root_data_drought_min$RootTrait_6, Root_data_drought_min$Trait6_Direction, sep = "_")
Root_data_drought_min$RootT7wResponse<-paste(Root_data_drought_min$RootTrait_7, Root_data_drought_min$Trait7_Direction, sep = "_")
Root_data_drought_min$RootT8wResponse<-paste(Root_data_drought_min$RootTrait_8, Root_data_drought_min$Trait8_Direction, sep = "_")
Root_data_drought_min$RootT9wResponse<-paste(Root_data_drought_min$RootTrait_9, Root_data_drought_min$Trait9_Direction, sep = "_")
Root_data_drought_min$RootT10wResponse<-paste(Root_data_drought_min$RootTrait_10, Root_data_drought_min$Trait10_Direction, sep = "_")

Root_merged_Direction<-c("RootT1wResponse","RootT2wResponse","RootT3wResponse","RootT4wResponse","RootT5wResponse","RootT6wResponse","RootT7wResponse","RootT8wResponse","RootT9wResponse","RootT10wResponse")

Drought_unique_references = Root_data_drought_min[!duplicated(Root_data_drought_min$AU),]
str(Drought_unique_references)

d<-reshape(Drought_unique_references, 
           varying = Root_merged_Direction, 
           v.names = "Trait", 
           timevar = "Org.position",
           times = Root_merged_Direction,
           direction = "long")

# Split d to get trait and response next to one another 
dd<-do.call(rbind, strsplit(d$Trait, '_'))
colnames(dd)<-c("Trait","Response_Dir")
Drought_unique_references_dd<-cbind(Drought_unique_references,dd)

levels(Drought_unique_references_dd$Trait)[levels(Drought_unique_references_dd$Trait)=='NA'] <- NA
Drought_unique_references_dd<-Drought_unique_references_dd[!is.na(Drought_unique_references_dd$Trait),]

dat<-as.data.frame(count(Drought_unique_references_dd, "Trait"))
dat_2<-dat[!(dat$freq < 4),]


##### Correlated responses (Survival, Establishment, Growth, Biomass)
Root_data_drought_correlated<-Root_data_drought[, c("AU","Variable","Positively_correlated",
                                                    "Positively_correlated.1","Negatively_correlated", 
                                                    "Variable.1","Positively_correlated.2","Positively_correlated.3", "Negatively_correlated.1")]
# Stack Variable and Variable 

Root_data_drought_correlated_survival<-Root_data_drought_correlated[! (is.na(Root_data_drought_correlated$Variable) & is.na(Root_data_drought_correlated$Variable.1)) ,]

Root_data_drought_correlated_survival$Resp_positive_trait.1<-paste(Root_data_drought_correlated_survival$Variable, Root_data_drought_correlated_survival$Positively_correlated, sep = "_")
Root_data_drought_correlated_survival$Resp_positive_trait.2<-paste(Root_data_drought_correlated_survival$Variable, Root_data_drought_correlated_survival$Positively_correlated.1, sep = "_")
Root_data_drought_correlated_survival$Resp_positive_trait.3<-paste(Root_data_drought_correlated_survival$Variable.1, Root_data_drought_correlated_survival$Positively_correlated.2, sep = "_")
Root_data_drought_correlated_survival$Resp_positive_trait.4<-paste(Root_data_drought_correlated_survival$Variable.1, Root_data_drought_correlated_survival$Positively_correlated.3, sep = "_")
Root_data_drought_correlated_survival$Resp_neg_trait.1<-paste(Root_data_drought_correlated_survival$Variable, Root_data_drought_correlated_survival$Negatively_correlated, sep = "_")
Root_data_drought_correlated_survival$Resp_neg_trait.2<-paste(Root_data_drought_correlated_survival$Variable.1, Root_data_drought_correlated_survival$Negatively_correlated.1, sep = "_")


ds<-reshape(Root_data_drought_correlated_survival, 
           varying = c("Resp_positive_trait.1","Resp_positive_trait.2","Resp_positive_trait.3","Resp_positive_trait.4","Resp_neg_trait.1","Resp_neg_trait.2"), 
           v.names = "Response", 
           timevar = "X",
           times = c("Resp_positive_trait.1","Resp_positive_trait.2","Resp_positive_trait.3","Resp_positive_trait.4","Resp_neg_trait.1","Resp_neg_trait.2"),
           direction = "long")

# Remove rows with "NA" in "them "Response" column to end up with just rows that have have trait_response
dss<-ds[!grepl("NA", ds$Response),]

# Split "trait vs. response column "Response" to be "Trait" and "response" - only four negative correlations so can exclude those for now. 

dsss<-do.call(rbind, strsplit(dss$Response, '_'))
colnames(dsss)<-c("Measure","Trait")
Corr_data<-cbind(dss, dsss)
Corr_data<-Corr_data[!grepl("neg", Corr_data$X),]
Corr_data_key_response<-Corr_data[grepl("Biomass.*|Survival|Establishment|Growth.*", Corr_data$Measure),]

data_corr<-as.data.frame(count(Corr_data_key_response, c("Trait","Measure")))
head(data_corr,30)
data_corr<-data_corr[grepl("Root to shoot ratio|Root biomass|Rooting depth|Specific root length|Diameter|Root elongation rate", data_corr$Trait),]

data_corr[c("Trait")]<-as.data.frame(lapply(data_corr[c("Trait")], gsub, pattern = "Root to shoot ratio", replacement = "RSR or RMF"))
data_corr[c("Trait")]<-as.data.frame(lapply(data_corr[c("Trait")], gsub, pattern = "Root biomass ", replacement = "Root biomass"))
data_corr[c("Trait")]<-as.data.frame(lapply(data_corr[c("Trait")], gsub, pattern = "Rooting depth to leaf area ratio", replacement = "Rooting depth"))
data_corr[c("Trait")]<-as.data.frame(lapply(data_corr[c("Trait")], gsub, pattern = "Specific root length", replacement = "SRL"))
data_corr[c("Trait")]<-as.data.frame(lapply(data_corr[c("Trait")], gsub, pattern = "Root elongation rate", replacement = "RER"))
data_corr[c("Measure")]<-as.data.frame(lapply(data_corr[c("Measure")], gsub, pattern = "total", replacement = "Total"))



data_corr$Trait<-factor(data_corr$Trait, levels = c("RSR or RMF", "Root biomass","SRL", "Rooting depth","Diameter","RER"))
data_corr$Measure<-factor(data_corr$Measure, levels = c("Biomass (AG)", "Biomass (Total)", "Biomass (Recovery)","Growth rate (AG)","Growth (potential)","Establishment","Survival"))

#### - Plots
plot_all_refs<-ggplot(dat_2, aes(x = reorder(Trait, -freq), y = freq)) + 
  geom_bar (stat = "identity") + 
  labs(y = "Times measured") + 
  theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_blank(),
        panel.grid = element_blank())

plot_AG_response<-ggplot(data_corr, aes(x = Trait, y = freq)) + 
  geom_bar (aes(fill = Measure), stat = "identity") + 
  labs(y = "Times related to performance") + 
  scale_fill_manual(values = c("steelblue3","steelblue4","blue","palegreen3","palegreen4","grey40","grey30"))+theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.position = c(.8,.8),
        legend.text = element_text(size = 8),
        legend.key.width = unit(.3, "cm"),
        legend.key.height = unit(.3, "cm"),
        panel.grid = element_blank())


Fig_combined<-plot_all_refs + annotation_custom(ggplotGrob(plot_AG_response), 
                                                xmin = 5.2, xmax = 12, 
                                                ymin = 13, ymax = 48)

