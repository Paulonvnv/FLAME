library(basecase)
library(writexl)

mda <- read.csv("~/Library/CloudStorage/Box-Box/FLAME/Analysis/fMDA/hh_census_wcases_03JUN2025.csv")


#Drop variables that aren't helpful 
mda <-  mda[,is.element(names(mda),c("community.x" , "hhcode.x" , "intv"))]
                                       
fmda_elegible <- table(mda$community.x, mda$intv)                    




# Create table and convert to data frame
fmda_elegible_df <- as.data.frame.matrix(fmda_elegible)



# Save as Excel file
write_xlsx(fmda_elegible_df, "fmda_elegible.xlsx")






# Write to Excel
write_xlsx(fm_df, "community_by_intv_table.xlsx")
