#processRO

gdp_deflator_data <- gdp_deflator_data %>%
  mutate(`GDP Index` = 100/gdp_deflator) %>%
  select(year, `GDP Index`)

rora_output <- left_join(ROdata, gdp_deflator_data) %>%
  filter(year %in% outturn_years) %>%
  filter(region != "-") %>%
  drop_na(region) %>%
  filter() %>%
  filter(variable %in% c("RS_NCE_AdultSocialCare", "RS_NCE_ChildrenSocialCare_adj2", "GSE")) %>%
  filter(class %in% c("UA", "SC", "SD", "MD", "LB")) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(`General Service Expenditure` = GSE*1e03,
         `Children's Social Care` = `RS_NCE_ChildrenSocialCare_adj2`*1e03,
         NCE_ASC_Total = RS_NCE_AdultSocialCare*1e03,
         CSC_and_ASC = (`RS_NCE_ChildrenSocialCare_adj2` + RS_NCE_AdultSocialCare)*1e03) %>%
  mutate(Total_excl_CSC = `General Service Expenditure`- `Children's Social Care`,
         Total_excl_CSC_ASC = `General Service Expenditure` - CSC_and_ASC) %>%
  select(-c(GSE, RS_NCE_ChildrenSocialCare_adj2, RS_NCE_AdultSocialCare)) %>%
  mutate(deflated_GSE = `General Service Expenditure`*`GDP Index`,
         deflated_CSC_total = `Children's Social Care`*`GDP Index`,
         deflated_NCE_ASC_Total = NCE_ASC_Total*`GDP Index`,
         deflated_csc_plus_asc = CSC_and_ASC*`GDP Index`) %>%
  mutate(deflated_Total_excl_CSC = deflated_GSE - deflated_CSC_total,
         deflated_Total_excl_csc_and_asc = deflated_GSE - deflated_csc_plus_asc) 



