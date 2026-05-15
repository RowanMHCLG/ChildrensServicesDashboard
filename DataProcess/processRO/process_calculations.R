###############################################################################
##                           * CALCULATIONS *                              ##
###############################################################################



# GSE
combined_data_calcs <- revenue_data_imputed %>%
  mutate(GSE = rowSums(select(., RS_NCE_AdultSocialCare, RS_NCE_ChildrenSocialCare_adj2, RS_NCE_Highwaysandtransportse, 
                              RS_NCE_HousingservicesGFRAon, RS_NCE_Culturalandrelatedserv, RS_NCE_Environmentalandregulat,
                              RS_NCE_Planninganddevelopment, RS_NCE_Fireandrescueservices, RS_NCE_Eduservices_adj2nosc, 
                              RS_NCE_Centralservices, RS_NCE_Otherservices, RS_Other_IntegratedTransportAu, 
                              RS_Other_WasteDisposalAuthorit, RS_NCE_PublicHealth), na.rm = TRUE)) 

