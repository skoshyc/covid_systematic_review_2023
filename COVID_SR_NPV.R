
setwd() #set the working directory which contains the data file. 

library(readxl) #to read excel files

library(meta) #for the meta analysis
full_data_set=read_excel("Master File of Data extraction_new.xlsx",sheet="Master - Study Data")
#full_data_set$Identifier=paste0(full_data_set$`Article #`,"_",full_data_set$`Article First Author`," et al.")
number_of_papers=length(unique(full_data_set$Identifier))
#Finding the overall category
overall_analysis=full_data_set[grep("Overall*",full_data_set$`Subgroups (if any; include Overall)`),]
number_papers_overall=length(unique(overall_analysis$Identifier))
#filtering out the rows which have not reported TP, FP, TN and FN
overall_analysis1=overall_analysis[which(is.na(overall_analysis$`True Positive (TP)`)!=TRUE),]
overall_analysis1=overall_analysis1[which(is.na(overall_analysis1$`False Positive (FP)`)!=TRUE),]

number_papers_overall1=length(unique(overall_analysis1$Identifier))


#Test subgroups analysis
subset_for_analysis=data.frame("Identifier"=overall_analysis1$Identifier,
                               'TP'=overall_analysis1$`True Positive (TP)`,
                               'FP'=overall_analysis1$`False Positive (FP)`,
                               'TN'=overall_analysis1$`True Negative (TN)`,
                               'FN'=overall_analysis1$`False Negative (FN)`,
                               'test_name_common'=overall_analysis1$`Index test description - Use one line per test and subgroup. For example, If Standard Q and BinaxNOW were both used in asymptomatic and symptomatic subjects, there would be 4 lines: (1) Standard Q Asymptomatic; (2) Standard Q Symptomatic; (3) BinaxNOW asymptomatic; and (4) BinaxNOW symptomatic`,
                               'test_name_other'=overall_analysis1$`Index test if Other`)

subset_for_analysis$test_name=subset_for_analysis$test_name_common
subset_for_analysis=subset_for_analysis[which(subset_for_analysis$test_name!="Other"),]
number_of_papers_subset=length(unique(subset_for_analysis$Identifier))
#create the subgroups based on frequency >=5
test_subgroup_table=as.data.frame(table(subset_for_analysis$test_name))
colnames(test_subgroup_table)=c("Test_name","Count")
subset_for_analysis=subset_for_analysis[which(subset_for_analysis$test_name!="Other"),]
subset_group=subset_for_analysis[subset_for_analysis$test_name%in%test_subgroup_table$Test_name[which(test_subgroup_table$Count>=5)],]
subset_group=subset_group[which(subset_group$TP!=0),]
#from the Diagnostic test accuracy_using R paper

NPV_logit_test_subgp <- metaprop(subset_group$TN, subset_group$TN+subset_group$FN, 
                                 comb.fixed = FALSE, comb.random = TRUE,
                                 sm = "PLOGIT", method.ci = "CP", 
                                 studlab = subset_group$Identifier,
                                 byvar = subset_group$test_name)
print(NPV_logit_test_subgp, digits= 3)
png(file = "forestplot_overall_NPV_test_subgp_freq5.png", width = 3000, height = 6000, res = 300)
forest(NPV_logit_test_subgp, digits= 3,
       xlab= "NPV",leftlabs = c("Source","NPV (95% CI)"),layout = "JAMA",
       col.by="black", addrow.overall = TRUE, addrow.subgroups = TRUE)
#col.by is the color for the subgroup headings
#addrow.overall A logical value indicating whether an empty row is printed above overall meta-analysis results.
#addrow.subgroups A logical value indicating whether an empty row is printed between results for subgroups.
dev.off()

library(dmetar)
library(dplyr)
outlier=find.outliers(NPV_logit_test_subgp)
print(outlier)

new_subset=subset_group[!(subset_group$Identifier %in%
                            c("Abusrewil, et. al. Dec 2021", 
                              "Boum, et. al. May 2021", "Cerutti, et. al. Aug 2021", 
                              "Fernandez-Montero, et. al. July 2021", "Holzner, et. al. April 2021", 
                              "Jakobsen, et. al. June 2021", "Krüger, et. al. May 2021", 
                              "Pilarowski, et. al. Jan 2021", "Pollock, et. al. Apr 2021", 
                              "Turcato, et. al. March 2021", "Allan-Blitz, et. al. Sep 2021", 
                              "Amer, et. al. Mar 2022", "Cottone, et. al. May 2022", 
                              "Garcia-Cardenas, et. al. Sep 2021", "Freire, et. al. Jun 2022", 
                              "Jakobsen, et. al. Feb 2022", "Jeewandara, et. al. Mar 2022",
                              "King, et. al. Sep 2021", "Krüger, et. al. Dec 2021", 
                              "Landaverde, et. al. Mar 2022", "Layer, et. al. Feb 2022", 
                              "Okoye, et. al. Feb 2022", "Afzal, et. al. Sep 2021", 
                              "Rohde, et. al. Feb 2022", "Nóra, et. al. Feb 2022", "Nóra, et. al. Feb 2022", 
                              "Siddiqui, et. al. Dec 2021", "Sitoe, et. al. Feb 2022", 
                              "Surasi, et. al. Nov 2021", "Thell, et. al. Nov 2021", "Thirion-Romero, et. al. Oct 2021", 
                              "Turcato, et. al. Jan 2022", "von Ahnen, et. al. Mar 2022","Afzal, et al. Sep 2021")),]
NPV_logit_test_subgp_new <- metaprop(new_subset$TN, new_subset$TN+new_subset$FN, 
                                     comb.fixed = FALSE, comb.random = TRUE,
                                     sm = "PLOGIT", method.ci = "CP", 
                                     studlab = new_subset$Identifier,
                                     byvar = new_subset$test_name)
print(NPV_logit_test_subgp_new, digits= 3)
png(file = "forestplot_overall_NPV_test_subgp_woOutliers.png", width = 3000, height=6000,
    res = 300)
forest.meta(NPV_logit_test_subgp, digits= 3,
            xlab= "NPV",leftlabs = c("Source","NPV (95% CI)"),layout = "JAMA",
            col.by="black", addrow.overall = TRUE, addrow.subgroups = TRUE) 
dev.off()

new_subset$n=new_subset$TP+new_subset$FP+new_subset$TN+new_subset$FN
new_subset %>%
  group_by(test_name) %>%
  summarise(Freq= sum(n))



#NPV analysis for symptomatic 
#Within this subgroup, we only looked at the same tests as the Overall group to be consistent.
symp=full_data_set[which(full_data_set$`Subgroups (if any; include Overall)`=="Symptomatic"),]


#filtering out the rows which have not reported TP, FP, TN and FN
symp1=symp[which(is.na(symp$`True Negative (TN)`)!=TRUE),]
symp1=symp1[which(is.na(symp1$`False Negative (FN)`)!=TRUE),]

subset_symp=data.frame("Identifier"=symp1$Identifier,
                       'TP'=symp1$`True Positive (TP)`,
                       'FP'=symp1$`False Positive (FP)`,
                       'TN'=symp1$`True Negative (TN)`,
                       'FN'=symp1$`False Negative (FN)`,
                       'test_name_common'=symp1$`Index test description - Use one line per test and subgroup. For example, If Standard Q and BinaxNOW were both used in asymptomatic and symptomatic subjects, there would be 4 lines: (1) Standard Q Asymptomatic; (2) Standard Q Symptomatic; (3) BinaxNOW asymptomatic; and (4) BinaxNOW symptomatic`,
                       'test_name_other'=symp1$`Index test if Other`)

subset_symp$test_name=subset_symp$test_name_common
subset_symp=subset_symp[which(subset_symp$test_name!="Other"),]
number_of_papers_subset=length(unique(subset_symp$Identifier))
subset_symp=subset_symp[subset_symp$test_name %in% c("BinaxNOW (Abbott)","PanBio (Abbott)",
                                                     "Roche SARS-CoV-2 Rapid Antigen Test (Roche)",
                                                     "Standard Q COVID-19 Ag (SD Biosensor)"),]

NPV_logit_symp_subgp <- metaprop(subset_symp$TN, subset_symp$TN+subset_symp$FN, 
                                 comb.fixed = FALSE, comb.random = TRUE,
                                 sm = "PLOGIT", method.ci = "CP", 
                                 studlab = subset_symp$Identifier,
                                 byvar = subset_symp$test_name)
print(NPV_logit_symp_subgp)
png(file = "forestplot_NPV_symp.png", width = 3000, height = 3000, res = 300)
forest.meta(NPV_logit_symp_subgp, digits= 3,
            xlab= "NPV",leftlabs = c("Source","NPV (95% CI)"),layout = "JAMA",
            col.by="black", addrow.overall = TRUE, addrow.subgroups = TRUE)
dev.off()

subset_symp$n=subset_symp$TP+subset_symp$FP+subset_symp$TN+subset_symp$FN
subset_symp %>%
  group_by(test_name) %>%
  summarise(Freq= sum(n))


#Asymptomatic
#Within this subgroup, we only looked at the same tests as the Overall group to be consistent.
asymp=full_data_set[which(full_data_set$`Subgroups (if any; include Overall)`=="Asymptomatic"),]
asymp1=asymp[which(is.na(asymp$`True Positive (TP)`)!=TRUE),]
asymp1=asymp1[which(is.na(asymp1$`False Positive (FP)`)!=TRUE),]
subset_asymp=data.frame("Identifier"=asymp1$Identifier,
                        'TP'=asymp1$`True Positive (TP)`,
                        'FP'=asymp1$`False Positive (FP)`,
                        'TN'=asymp1$`True Negative (TN)`,
                        'FN'=asymp1$`False Negative (FN)`,
                        'test_name_common'=asymp1$`Index test description - Use one line per test and subgroup. For example, If Standard Q and BinaxNOW were both used in asymptomatic and symptomatic subjects, there would be 4 lines: (1) Standard Q Asymptomatic; (2) Standard Q Symptomatic; (3) BinaxNOW asymptomatic; and (4) BinaxNOW symptomatic`,
                        'test_name_other'=asymp1$`Index test if Other`)

subset_asymp$test_name=subset_asymp$test_name_common
subset_asymp=subset_asymp[which(subset_asymp$test_name!="Other"),]
subset_asymp=subset_asymp[subset_asymp$test_name %in% c("BinaxNOW (Abbott)","PanBio (Abbott)",
                                                        "Roche SARS-CoV-2 Rapid Antigen Test (Roche)",
                                                        "Standard Q COVID-19 Ag (SD Biosensor)"),]



NPV_logit_asymp_subgp <- metaprop(subset_asymp$TN, subset_asymp$TN+subset_asymp$FN, 
                                  comb.fixed = FALSE, comb.random = TRUE,
                                  sm = "PLOGIT", method.ci = "CP", 
                                  studlab = subset_asymp$Identifier,
                                  byvar = subset_asymp$test_name)
print(NPV_logit_asymp_subgp)
png(file = "forestplot_NPV_asymp.png", width = 3000, height = 3000, res = 300)
forest.meta(NPV_logit_asymp_subgp, digits= 3,
            xlab= "NPV",leftlabs = c("Source","NPV (95% CI)"),layout = "JAMA",
            col.by="black", addrow.overall = TRUE, addrow.subgroups = TRUE)
dev.off()

subset_asymp$n=subset_asymp$TP+subset_asymp$FP+subset_asymp$TN+subset_asymp$FN
subset_asymp %>%
  group_by(test_name) %>%
  summarise(Freq= sum(n))





