
############################################################################
############################################################################
###                                                                      ###
###                        DESCRIPTIVE STATISTICS                        ###
###                                                                      ###
############################################################################
############################################################################




#descriptive test

dat %>% group_by(`Isolate Code`) %>% summarise(
  Mean = mean(`Chlamy_Size (mm)`),
  Median = median(`Chlamy_Size (mm)`),
  Standard_Deviation = sd(`Chlamy_Size (mm)`),
  Min = min(`Chlamy_Size (mm)`),
  Max = max(`Chlamy_Size (mm)`),
  Range = range(`Chlamy_Size (mm)`)
)
