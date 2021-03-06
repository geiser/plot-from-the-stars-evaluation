
library(readr)
library(dplyr)
library(car)
library(afex)
library(dplyr)
library(stats)
library(ez)

participants <- read_csv('data/Participants.csv')
dat <- merge(participants, read_csv('data/IMI.csv'))

dvs <- c("Perceived Competence", "Value/Usefulness", "Interest/Enjoyment", "Perceived Choice", "Intrinsic Motivation")
list_dvs <- as.list(dvs)
names(list_dvs) <- dvs

list_info <- lapply(list_dvs, FUN = function(dv) {
  name <- gsub('\\W', '', dv)
  
  ivs <- c("Age", "Sex", "AcademicDegree", "KnownDomainContent", "LikeDomainContent"
           , "LikeActionGames", "LikeAdventureGames", "LikeFightingGames"
           , "LikeFictionGames", "LikePlatformerGames", "LikePuzzleGames"
           , "LikeRacingGames", "LikeMusicalGames", "LikeMMORPGGames"
           , "LikeShooterGames", "LikeSimulationGames", "LikeSportsGames"
           , "LikeStrategyGames", "LikeNovelGames")
  list_ivs <- as.list(ivs)
  names(list_ivs) <- ivs
  info <- lapply(list_ivs, function(iv) {
    return(list(
      title = paste0(dv, " by ", iv)
      , path = paste0("report/learning-outcome/by-",iv,"/")
      , iv = iv
    ))
  })
  
  return(list(dv=dv, name =name, info = info))
})

# removing outliers
dat_map <- lapply(list_dvs, FUN = function(dv){
  userids <- dat$UserID[!dat[[dv]] %in% c(boxplot(dat[[dv]])$out)]
  return(dat[dat$UserID %in% c(userids),])
})

#############################################################################
## Non-Parametric Statistic Analysis                                       ##
#############################################################################
all_nonparametric_results <- lapply(list_dvs, function(dv) {
  info <- list_info[[dv]]$info
  sdat <- dat_map[[dv]]
  dir.create(paste0("report/motivation/", list_info[[dv]]$name), showWarnings = F)
    
  nonparametric_results <- lapply(info, FUN = function(x) {
    cat("\n .... processing: ", x$title, " ....\n")
    dir.create(file.path(x$path), showWarnings = F)
    dir.create(file.path(x$path, 'nonparametric-analysis-plots'), showWarnings = F)
    
    path <- paste0(x$path, 'nonparametric-analysis-plots/')
    filename <- paste0(x$path, 'NonParametricAnalysis.xlsx')
    result <- do_nonparametric_test(sdat, wid = 'UserID', dv = dv, iv = x$iv, between = c(x$iv))
    
    write_plots_for_nonparametric_test(
      result, ylab = "Score", title = x$title
      , path = path, override = T
      , ylim = c(1,7), levels = NULL #c('non-gamified','ont-gamified')
    )
    write_nonparametric_test_report(
      result, ylab = "Score", title = x$title
      , filename = filename, override = T
      , ylim = c(1,7), levels = NULL #c('non-gamified','ont-gamified')
    )
    return(result)
  })
})


#############################################################################
## Translate latex resume                                                  ##
#############################################################################
lapply(list_dvs, function(dv) {
  info <- list_info[[dv]]$info
  nonparametric_results <- all_nonparametric_results[[dv]]
  write_kruskal_statistics_analysis_in_latex(
    nonparametric_results, info
    , filename = paste0("report/latex/nonparametric-", list_info[[dv]]$name, "-analysis.tex")
    , in_title = paste0(" for the ", dv)
  )
})

