#Script to evaluate deviations

library(dplyr)
library(REDCapR)


Sys.setenv(token_protocol = "E9135070AC82DFAAFA54E4B74BD1C127")


# Report from REDCap
deviation <- REDCapR::redcap_report(redcap_uri = "https://redcap.ucsf.edu/api/", token = Sys.getenv("token_protocol"), report_id = 223471, guess_type = F)$data
