library(readr)
library(dplyr)
library(psych)
library(lavaan)
library(ggraph)
library(semPlot)

library(MVN)
library(daff)
library(robustHD)

wdat <- read_csv('data/WinsorizedIMI.csv')

############################################################################
## Check Assumptions to Reliability Analysis                              ##
############################################################################

png(filename = "report/validation-IMI/univariate-histogram.png", width = 840, height = 840)
(mvn_mod <- mvn(select(wdat, starts_with("Item")), multivariateOutlierMethod = "adj", univariatePlot = "histogram", showOutliers = T))
dev.off()

estimator_cfa <- "WLSMVS" # for non-normal and ordinal data

## kmo factor adequacy
(kmo_mod <- KMO(cor(select(wdat, starts_with("Item")))))

## factorial analysis
#factanal(~Item15+Item19+Item03+Item04+Item05+Item06+Item07+Item08
#         +Item09+Item10+Item11+Item12+Item13+Item14+Item15+Item16
#         +Item17+Item18+Item19+Item20+Item21+Item22+Item23+Item24
#         , factors=4, data=wdat, rotation = "varimax")

#factanal(~Item22+Item09+Item12+Item24+Item21+Item01+Item04
#         +Item17+Item15+Item06+Item02+Item08+Item19
#         +Item16+Item14+Item18+Item11
#         +Item13+Item03+Item07
#         +Item05+Item10+Item20+Item23
#         , factors=4, data=wdat, rotation = "varimax")

# non one item less than < 0.4 loading had been removed

# removing crosloading items with loading less than < 0.2
#factanal(~Item22+Item09+Item12+Item24+Item21+Item01+Item04
#         +Item17+Item15+Item06+Item02+Item08+Item19
#         +Item16+Item14+Item18+Item11
#         +Item13+Item03+Item07
#         , factors=4, data=wdat, rotation = "varimax")

# first eingenvalue between 3 to 5 then
# we removed items that don't fit by meaning based on consult with psychometrics
#factanal(~Item22+Item09+Item12+Item24+Item21+Item01 # IE: Interest/Enjoyment
#         +Item17+Item15+Item06+Item02+Item08 # PC: Perceived Competence
#         +Item16+Item14+Item18+Item11 # PT: Pressure/Tension
#         +Item13+Item03+Item07  # EI: Effort/Importance
#         , factors=4, data=wdat, rotation = "varimax")

#(fa_mod <- fa(select( 
#  wdat
#  , starts_with("Item22"), starts_with("Item09"), starts_with("Item12"), starts_with("Item24"), starts_with("Item21"), starts_with("Item01")
#  , starts_with("Item17"), starts_with("Item15"), starts_with("Item06"), starts_with("Item02"), starts_with("Item08")
#  , starts_with("Item16"), starts_with("Item14"), starts_with("Item18"), starts_with("Item11")
#  , starts_with("Item13"), starts_with("Item03"), starts_with("Item07")
#), nfactors = 4, rotate = "varimax"))

## validating models where orthogonal means no correlation between factors

#multi_mdl <- '
#IE =~ Item22+Item09+Item12+Item24+Item21+Item01
#PC =~ Item17+Item15+Item06+Item02+Item08
#PT =~ Item16+Item14+Item18+Item11
#EI =~ Item13+Item03+Item07
#'
#second_mdl <- '
#IE =~ Item22 + Item09 + Item12 + Item24 + Item21 + Item01
#PC =~ Item17 + Item15 + Item06 + Item02 + Item08
#PT =~ Item16 + Item14 + Item18 + Item11
#EI =~ Item13 + Item03 + Item07
#IM =~ NA*IE + PC + PT + EI
#IM ~~ 1*IM
#'
#bifactor_mdl <- '
#IE =~ a*Item22 + a*Item09 + a*Item12 + a*Item24 + a*Item21 + a*Item01
#PC =~ b*Item17 + b*Item15 + b*Item06 + b*Item02 + b*Item08
#PT =~ c*Item16 + c*Item14 + c*Item18 + c*Item11
#EI =~ d*Item13 + d*Item03 + d*Item07
#IM =~ Item22 + Item09 + Item12 + Item24 + Item21 + Item01 +
#Item17 + Item15 + Item06 + Item02 + Item08 +
#Item16 + Item14 + Item18 + Item11 +
#Item13 + Item03 + Item07
#'

#(fitMeasures_df <- do.call(rbind, lapply(
#  list(
#    "Global sample" = list(
#      dat = wdat,
#      mdls = list(
#        "Multidimensional model" = list(mdl = multi_mdl, plotFile = "multidimensional-model.png")
#        , "Second-order model" = list(dat = wdat, mdl = second_mdl, plotFile = "second-order-factor-model.png")
#        , "Bi-factor model" = list(dat = wdat, mdl = bifactor_mdl, plotFile = "bi-factor-model.png")))
#    , "Pilot-study" = list(
#      dat = wdat[which(wdat$Study=="pilot"),],
#      mdls = list(
#        "Multidimensional model" = list(mdl = multi_mdl)
#        , "Second-order model" = list(mdl = second_mdl)
#        , "Bi-factor model" = list(mdl = bifactor_mdl)))
#    , "First study" = list(
#      dat = wdat[which(wdat$Study=="first"),],
#      mdls = list(
#        "Multidimensional model" = list(mdl = multi_mdl)
#        , "Second-order model" = list(mdl = second_mdl)
#        , "Bi-factor model" = list(mdl = bifactor_mdl)))
#    , "Third study" = list(
#      dat = wdat[which(wdat$Study=="third"),],
#      mdls = list(
#        "Multidimensional model" = list(mdl = multi_mdl)
#        , "Second-order model" = list(mdl = second_mdl)
#        , "Bi-factor model" = list(mdl = bifactor_mdl)))
#  )
#  , FUN = function(s) {
#    fit_df <- do.call(rbind, lapply(
#      s$mdls
#      , FUN = function(x) {
#        return(get_fitMeasures(dat = s$dat, mdl = x$mdl, estimator = estimator_cfa))
#      }
#    ))
#    return(rbind(c(NA), fit_df))
#  }))
#)

# select second-order model to measure intrinsic motivation
#(cfa_mod <- cfa(second_mdl, data = wdat, std.lv = T, estimator = estimator_cfa))


############################################################################
## Reliability Analysis Using Cronbach's alpha                            ##
############################################################################

rdat <- select(wdat, starts_with("UserID"))
rdat["Item02PC"] <- wdat["Item02"]
rdat["Item05PC"] <- wdat["Item05"]
rdat["Item06PC"] <- wdat["Item06"]
rdat["Item08PC"] <- wdat["Item08"]
rdat["Item15PC"] <- wdat["Item15"]
rdat["Item23PC"] <- wdat["Item23"]

rdat["Item25PC2"] <- wdat["Item25"]
rdat["Item26PC2"] <- wdat["Item26"]
rdat["Item27PC2"] <- wdat["Item27"]
rdat["Item28PC2"] <- wdat["Item28"]
rdat["Item29PC2"] <- wdat["Item29"]
rdat["Item30PC2"] <- wdat["Item30"]

rdat["Item31VU"] <- wdat["Item31"]
rdat["Item32VU"] <- wdat["Item32"]
rdat["Item33VU"] <- wdat["Item33"]
rdat["Item34VU"] <- wdat["Item34"]
rdat["Item35VU"] <- wdat["Item35"]
rdat["Item36VU"] <- wdat["Item36"]
rdat["Item37VU"] <- wdat["Item37"]

rdat["Item09IE"] <- wdat["Item09"]
rdat["Item12IE"] <- wdat["Item12"]
rdat["Item19IE"] <- wdat["Item19"]
rdat["Item20IE"] <- wdat["Item20"]
rdat["Item21IE"] <- wdat["Item21"]
rdat["Item22IE"] <- wdat["Item22"]
rdat["Item24IE"] <- wdat["Item24"]

rdat <- dplyr::mutate(
  rdat
  , `Interest/Enjoyment` = (rdat$Item09IE+rdat$Item12IE+rdat$Item21IE+rdat$Item22IE+rdat$Item24IE+16-(rdat$Item19IE+rdat$Item20IE))/7
  , `Perceived Choice` = (rdat$Item05PC+rdat$Item23PC+32-(rdat$Item02PC+rdat$Item06PC+rdat$Item08PC+rdat$Item15PC))/6
  , `Perceived Competence` = (rdat$Item25PC2+rdat$Item26PC2+rdat$Item27PC2+rdat$Item28PC2+rdat$Item29PC2+rdat$Item30PC2)/6
  , `Value/Usefulness` = (rdat$Item31VU+rdat$Item32VU+rdat$Item33VU+rdat$Item34VU+rdat$Item35VU+rdat$Item36VU+rdat$Item37VU)/7
)
rdat <- dplyr::mutate(
  rdat
  , `Intrinsic Motivation` = (rdat$`Interest/Enjoyment`
                              +rdat$`Perceived Choice`
                              +rdat$`Perceived Competence`
                              +rdat$`Value/Usefulness`)/4
)
if (!file.exists('data/IMI.csv')) {
  write_csv(rdat, path = 'data/IMI.csv')
}


