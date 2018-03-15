
library(readr)
library(dplyr)
library(car)
library(afex)
library(dplyr)
library(stats)
library(ez)

participants <- read_csv('data/Participants.csv')
dat <- merge(participants, read_csv('data/IMI.csv'))
dat <- dplyr::mutate(
  dat
  , `Perceived Competence` = (dat$Item33PC2+dat$Item28PC2+dat$Item25PC2+dat$Item37PC2+dat$Item30PC2+8-dat$Item34PC2)/6
  , `Value/Usefulness` = (dat$Item29VU+dat$Item26VU+dat$Item31VU+dat$Item36VU+dat$Item35VU)/5
  , `Interest/Enjoyment` = (dat$Item24IE+dat$Item08IE+dat$Item21IE+dat$Item09IE+dat$Item12IE+dat$Item11IE)/6
  , `Perceived Choice` = (32-(dat$Item05PC1+dat$Item13PC1+dat$Item18PC1+dat$Item03PC1))/4
)
dat <- dplyr::mutate(
  dat
  , `Intrinsic Motivation` = ((6*dat$`Perceived Competence`)+(5*dat$`Value/Usefulness`)+(6*dat$`Interest/Enjoyment`)+(4*dat$`Perceived Choice`))/21
)


dvs <- c("Value/Usefulness", "Interest/Enjoyment"
         #, "Perceived Choice" - a lot of data are non-normal
         , "Intrinsic Motivation", "Perceived Competence")
list_dvs <- as.list(dvs)
names(list_dvs) <- dvs

list_info <- lapply(list_dvs, FUN = function(dv) {
  name <- gsub('\\W', '', dv)
  
  ivs <- c("Age", "Sex", "AcademicDegree", "KnownDomainContent", "LikeDomainContent"
           , "LikeActionGames", "LikeAdventureGames", "LikeFightingGames"
           , "LikeFictionGames", "LikePlatformerGames", "LikePuzzleGames"
           , "LikeRacingGames", "LikeMusicalGames", "LikeMMORPGGames"
           , "LikeShooterGames", "LikeSimulationGames", "LikeSportsGames"
           , "LikeStrategyGames", "LikeNovelGames"
           )
  list_ivs <- as.list(ivs)
  names(list_ivs) <- ivs
  info <- lapply(list_ivs, function(iv) {
    return(list(
      title = paste0(dv, " by ", iv)
      , path = paste0("report/motivation/",name,"/by-",iv,"/")
      , iv = iv
    ))
  })
  
  return(list(dv=dv, name =name, info = info))
})

#############################################################################
## Parametric Statistic Analysis                                           ##
#############################################################################

# removing outliers
gdat_map <- lapply(list_dvs, FUN = function(dv){
  userids <- dat$UserID[!dat[[dv]] %in% c(boxplot(dat[[dv]])$out)]
  return(dat[dat$UserID %in% c(userids),])
})
extra_rmids <- list( # extra rmids
  "Perceived Competence" = list(
    KnownDomainContent = c(5,14,24)
  )
  , "Value/Usefulness" = list(
    Age = c(49,35,5,23,21,29)
    , Sex = c(49,35,21,42,5,23,3,29,18,37,26)
    , AcademicDegree = c(35,49,5,10,23,18,37,45,26,32,12,44,27,33,17,2,25,47)
    , KnownDomainContent = c(49,35,5,23,21,29,42,18)
    , LikeDomainContent = c(35,49,21)
    , LikeActionGames = c(49,35,5,23,29,3,21)
    , LikeAdventureGames = c(49,35,5,23,29,3,21,42,18,37,26)
    , LikeFightingGames = c(49,35,5,23,18,3,21)
    , LikeFictionGames = c(35,49,5,3,23,21,29,42,18,37,26,45,32,27)
    , LikePlatformerGames = c(49,35,5,23,3,21,42,29,18,37,26)
    , LikePuzzleGames = c(47,19,35,49,3,46,29,5,37,23,26,45,21,42,18,27,33,32)
    , LikeRacingGames = c(49,35,46,21,42,18)
    , LikeMusicalGames = c(49,35,5,23,3,29,18,21,42,26)
    , LikeMMORPGGames = c(49,35,5,23,47,25,37,3,21,46,43,29,19,42,26,45,18
                          ,44,32,12,27,33)
    , LikeSimulationGames =c(49,35,5,23,3,21,29,42,18,37,26)
    , LikeSportsGames = c(49,35,23,3,21,2942,18,37,29,42,26)
    , LikeNovelGames = c(35,5,23,3,21,29,42,18,37,26,25)
  )
  , "Interest/Enjoyment" = list(
    Sex = c(28,9,27,21,15)
    , LikeDomainContent = c(31,9,28,15)
    , LikeMMORPGGames = c(9)
  )
  , "Intrinsic Motivation" = list(
    Age = c(27,25,5,31,47)
    , Sex = c(27,25,5,31,21,42,24,35,26,28)
    , LikeAdventureGames = c(5,27,25,24,26,49,35,9,47,21,42)
    , LikeRacingGames = c(31,5)
    , LikeSimulationGames = c(27,5,24,25,43,13,31,46,35,11,36,50,1,26,47,21)
    , LikeStrategyGames = c(5,31,24,27,35,26,49,9,13,15,30,25,39,12,37,45,2,14
                            ,23,21,42,10,41)
  )
  , "Perceived Choice" = list( # TODO complete to remove non-normal data
    Age = c(1,50,49,2,3,7,6,11,14,16,17,18,19,25,48,46,45,43,40,38,32,37)
    , Sex = c(50,49,48,2,3,7,6,11,14,16,18,17,19,46,45,43,40
              ,38,37,36,34,22,8,9,30,44,13,28,47
              ,26,1,21)
    , AcademicDegree =c(1,3,41,8,22,2,6,7,11,14,16,17,50,49,48,18,48,46,45,43
                        ,40,38,37,13,9,26,32,44)
    , KnownDomainContent = c(50,49,48,46,45,2,3,6,7,11,14,17,16,43,40,38,37,36
                             ,34,1,9,13)
    , LikeDomainContent = c(3,2,7,14,6,11,9,13,26,50,49,16,17,18,25,32,48,46,19
                            ,28,21,44,45)
    , LikeActionGames = c(50,49,48,46,45,2,3,6,7,11,26,1,36,34,30,44,28,25,35,18,19)
    , LikeAdventureGames = c(50,49,48,46,45,2,3,6,7,11,26,1,36,34,30,44,28,25,35,18,19
                             ,21,17,14
                             )
  )
)
dat_map <- lapply(list_dvs, function(dv) {
  info <- list_info[[dv]]$info
  gdat <- gdat_map[[dv]]
  return(lapply(info, FUN = function(x) {
    if (!is.null(extra_rmids[[dv]][[x$iv]]) && length(extra_rmids[[dv]][[x$iv]]) > 0) {
      gdat <- gdat[!gdat[['UserID']] %in% extra_rmids[[dv]][[x$iv]],]
    }
    rmids <- get_ids_outliers(gdat, 'UserID', dv, iv = x$iv, between = c(x$iv))
    cat('\n...removing ids: ', rmids,' from: ', dv, ' by ' , x$iv,' ...\n')
    return(gdat[!gdat[['UserID']] %in% rmids,])
  }))
})


# Validate Assumptions
lapply(list_dvs, function(dv) {
  info <- list_info[[dv]]$info
  lapply(info, FUN = function(x) {
    sdat <- dat_map[[dv]][[x$iv]]
    result <- do_parametric_test(sdat, wid = 'UserID', dv = dv, iv = x$iv, between = c(x$iv))
    cat('\n... assumptions fail in normality or equality for: ', dv, ' by ', x$iv, '\n')
    print(result$test.min.size$error.warning.list)
    if (result$normality.fail) cat('\n... normality fail ...\n')
    if (result$homogeneity.fail) cat('\n... homogeneity fail ...\n')
    if (result$assumptions.fail) {
      plot_assumptions_for_parametric_test(result, x$dv)
      if (result$normality.fail) normPlot(result$data, dv)
      stopifnot(F)
    }
  })
})

## export reports and plots
all_parametric_results <- lapply(list_dvs, function(dv) {
  info <- list_info[[dv]]$info
  
  dir.create(paste0("report/motivation/", list_info[[dv]]$name), showWarnings = F)
  
  parametric_results <- lapply(info, FUN = function(x) {
    cat("\n .... processing: ", x$title, " ....\n")
    dir.create(file.path(x$path), showWarnings = F)
    dir.create(file.path(x$path, 'parametric-analysis-plots'), showWarnings = F)
    
    path <- paste0(x$path, 'parametric-analysis-plots/')
    filename <- paste0(x$path, 'ParametricAnalysis.xlsx')
    
    sdat <- dat_map[[dv]][[x$iv]]
    result <- do_parametric_test(sdat, wid = "UserID", dv = dv, iv = x$iv, between = c(x$iv))
    
    write_plots_for_parametric_test(
      result, ylab = "Score", title = x$title
      , path = path, override = T
      , ylim = c(1,7), levels = NULL
    )
    write_parametric_test_report(
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
  parametric_results <- all_parametric_results[[dv]]
  write_param_statistics_analysis_in_latex(
    parametric_results, ivs = names(info)
    , filename = paste0("report/latex/parametric-", list_info[[dv]]$name, "-analysis.tex")
    , in_title = paste0(" for the scores of ", dv)
  )
})
