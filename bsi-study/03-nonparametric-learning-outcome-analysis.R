
library(readr)
library(dplyr)
library(car)
library(afex)
library(dplyr)
library(stats)
library(ez)

participants <- read_csv('data/Participants.csv')
dat <- merge(participants, read_csv('data/PrePostTest.csv'))

# removing outliers

ivs <- c("Age", "Sex", "LikeDomainContent"
         , "LikeActionGames", "LikeAdventureGames", "LikeFightingGames"
         , "LikeFictionGames", "LikePlatformerGames", "LikePuzzleGames"
         , "LikeRacingGames", "LikeMusicalGames"#, "LikeMMORPGGames"
         , "LikeShooterGames"#, "LikeSimulationGames"
         , "LikeSportsGames"
         , "LikeStrategyGames", "LikeNovelGames")
list_ivs <- as.list(ivs)
names(list_ivs) <- ivs
info <- lapply(list_ivs, function(iv) {
  return(list(
    title = paste0("Gain Score by ", iv)
    , path = paste0("report/learning-outcome/by-",iv,"/")
    , iv = iv
  ))
})

#############################################################################
## Non-Parametric Statistic Analysis                                       ##
#############################################################################
nonparametric_results <- lapply(info, FUN = function(x) {
  cat("\n .... processing: ", x$title, " ....\n")
  dir.create(file.path(x$path), showWarnings = FALSE)
  dir.create(file.path(x$path, 'nonparametric-analysis-plots'), showWarnings = FALSE)
  
  path <- paste0(x$path, 'nonparametric-analysis-plots/')
  filename <- paste0(x$path, 'NonParametricAnalysis.xlsx')
  result <- do_nonparametric_test(dat, dv = "GainScore", iv = x$iv, between = c(x$iv))
  
  write_plots_for_nonparametric_test(
    result, ylab = "Gain Score", title = x$title
    , path = path, override = T
    , ylim = NULL, levels = NULL #c('non-gamified','ont-gamified')
  )
  write_nonparametric_test_report(
    result, ylab = "Gain Score", title = x$title
    , filename = filename, override = T
    , ylim = NULL, levels = NULL #c('non-gamified','ont-gamified')
  )
  return(result)
})

#############################################################################
## Translate latex resume                                                  ##
#############################################################################

write_kruskal_statistics_analysis_in_latex(
  nonparametric_results, info
  , filename = "report/latex/nonparametric-learning-outcome-analysis.tex"
  , in_title = paste0(" for the improvement of right answers (Gain Score) from the pre-test"
                      ," to post-test")
)




