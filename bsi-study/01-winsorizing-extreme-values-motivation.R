wants <- c('MVN', 'robustHD', 'daff', 'plyr', 'readr')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])

library(MVN)
library(daff)
library(plyr)
library(readr)
library(dplyr)
library(robustHD)

############################################################################
## Winsorizing IMI data for validation                                    ##
############################################################################

sdatIMI <- as.data.frame(read_csv('data/SourceIMI.csv'))

# winsorize the data
rdatIMI <- sdatIMI
for (cname in colnames(rdatIMI)) {
  if (cname == "UserID") next
  rdatIMI[[cname]]  <- round(winsorize(rdatIMI[[cname]]))
}
rdatIMI$Item17 <- NULL

filename <- 'data/WinsorizedIMI.csv'
if (!file.exists(filename)) {
  write_csv(rdatIMI, filename)
}

## write in latex
render_diff(wdatIMI <- diff_data(sdatIMI, rdatIMI))
filename <- 'report/latex/winsorized-IMI.tex'
write_winsorized_in_latex(
  wdatIMI, filename, in_title = "for the validation of adapted Portuguese IMI")

