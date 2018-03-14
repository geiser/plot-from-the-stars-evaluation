############################################################################
## Confirmatory/Explanatory Factorial Analysis                            ##
############################################################################

library(dplyr)
library(readr)
library(psych)
library(lavaan)
library(r2excel)
library(Hmisc)

## read data
datIMI <- read_csv("data/SourceIMI.csv")

## validating sampling adequacy and removing unaceptable sampling adequacy < 0.5
print(kmo_mod <- KMO(cor(select(datIMI, starts_with("Item")))))
print(kmo_mod <- KMO(cor(select(datIMI, starts_with("Item")
                                , -starts_with("Item20")
                                , -starts_with("Item01")))))
print(kmo_mod <- KMO(cor(select(datIMI, starts_with("Item")
                                , -starts_with("Item20")
                                , -starts_with("Item01")
                                , -starts_with("Item06")
                                , -starts_with("Item07")
                                , -starts_with("Item23")))))

datIMI <- select(datIMI, starts_with("UserID"), starts_with("Item")
                 , -starts_with("Item20")
                 , -starts_with("Item01")
                 , -starts_with("Item06")
                 , -starts_with("Item07")
                 , -starts_with("Item23"))

## cfa to verify if relability analysis can be done
model <- '
f1 =~ Item08+Item09+Item11+Item17+Item21+Item24
f2 =~ Item02+Item04+Item12
f3 =~ Item03+Item05+Item10+Item13+Item16+Item18
f4 =~ Item14+Item15+Item19+Item22
f5 =~ Item25+Item28+Item30+Item33+Item34+Item37
f6 =~ Item26+Item27+Item29+Item31+Item32+Item35+Item36
'
cfa_mod <- cfa(model, data=datIMI, std.lv=T, orthogonal=F, missing="fiml")
summary(cfa_mod, fit.measures=T, standardized=T)

factanal(~Item29+Item26+Item35+Item36+Item31+Item32+Item27
         +Item33+Item28+Item34+Item25+Item37+Item30
         +Item24+Item08+Item21+Item09+Item11+Item17
         +Item05+Item13+Item18+Item03+Item10+Item16
         +Item14+Item22+Item19+Item15
         +Item04+Item02+Item12
         , factors=6, data=datIMI)

# removing items with loading < 0.4 #noting to remove


# removing cross loading items with a difference less than 0.2
factanal(~Item29+Item26+Item35+Item36+Item31
         +Item33+Item28+Item34+Item25+Item37+Item30
         +Item24+Item08+Item21+Item09+Item11
         +Item05+Item13+Item18+Item03
         +Item14+Item22
         +Item12
         , factors=6, data=datIMI)

## realiability with factors = 5
factanal(~Item29+Item26+Item35+Item36+Item31+Item32+Item27
         +Item24+Item08+Item21+Item12+Item09+Item11+Item17
         +Item33+Item28+Item34+Item25+Item37+Item30
         +Item05+Item13+Item18+Item03+Item10+Item16
         +Item14+Item22+Item19+Item15
         , factors=5, data=datIMI)

# removing items with loading < 0.4
factanal(~Item29+Item26+Item35+Item36+Item31+Item32+Item27
         +Item24+Item08+Item21+Item12+Item09+Item11+Item17
         +Item33+Item28+Item34+Item25+Item37+Item30
         +Item05+Item13+Item18+Item03+Item10+Item16
         +Item14+Item22+Item19+Item15
         , factors=5, data=datIMI)

# removing cross loading items with a difference less than 0.2
factanal(~Item29+Item26+Item35+Item36+Item31
         +Item24+Item08+Item21+Item12+Item09+Item11+Item17
         +Item33+Item28+Item34+Item25+Item37+Item30
         +Item05+Item13+Item18+Item03+Item10+Item16
         +Item19
         , factors=5, data=datIMI)

## realiability with factors = 4
factanal(~Item29+Item26+Item31+Item36+Item35+Item32+Item27+Item14
         +Item24+Item08+Item21+Item12+Item09+Item11+Item17
         +Item33+Item28+Item25+Item34+Item37+Item30
         +Item05+Item13+Item18+Item03+Item10+Item16
         , factors=4, data=datIMI)

# removing items with loading < 0.4 # nothing to remove

# removing cross loading items with a difference less than 0.2
factanal(~Item29+Item26+Item31+Item36+Item35+Item14
         +Item33+Item28+Item25+Item34+Item37+Item30
         +Item24+Item08+Item21+Item12+Item09+Item11
         +Item05+Item13+Item18+Item03
         , factors=4, data=datIMI)

model <- '
f1 =~ Item29+Item26+Item31+Item36+Item35+Item14
f2 =~ Item33+Item28+Item25+Item34+Item37+Item30
f3 =~ Item24+Item08+Item21+Item12+Item09+Item11
f4 =~ Item05+Item13+Item18+Item03
' # f1=Value/Usefulness, f2=Perceived Competence, f3=Interest/Enjoyment, f4=Perceived Choice
cfa_mod <- cfa(model, data=datIMI, std.lv=T, orthogonal=F, missing="fiml")
summary(cfa_mod, fit.measures=T, standardized=T)

datIMI <- select(
  datIMI, starts_with("UserID"), starts_with("Type")
  , starts_with("CLGroup"), starts_with("CLRole"), starts_with("PlayerRole")
  , starts_with("Item33"), starts_with("Item28"), starts_with("Item25"), starts_with("Item34"), starts_with("Item37"), starts_with("Item30")
  , starts_with("Item29"), starts_with("Item26"), starts_with("Item31"), starts_with("Item36"), starts_with("Item35")
  , starts_with("Item24"), starts_with("Item08"), starts_with("Item21"), starts_with("Item09"), starts_with("Item12"), starts_with("Item11")
  , starts_with("Item05"), starts_with("Item13"), starts_with("Item18"), starts_with("Item03")
)

## factorial analysis with nfactor=4
print(fa_mod <- fa(select(datIMI, starts_with("Item")), nfactors = 4, rotate = "varimax"))

## removing factors again with varimax
model <- '
f1 =~ Item33+Item28+Item25+Item34+Item37+Item30
f2 =~ Item29+Item26+Item31+Item36+Item35
f3 =~ Item24+Item08+Item21+Item09+Item12+Item11
f4 =~ Item05+Item13+Item18+Item03
' # f1=Perceived Competence, f2=Value/Usefulness, f3=Interest/Enjoyment, f4=Perceived Choice
cfa_mod <- cfa(model, data=datIMI, std.lv=T, orthogonal=F, missing="fiml")
summary(cfa_mod, fit.measures=T, standardized=T)

############################################################################
## Reliability Analysis Using Cronbach's alpha                            ##
############################################################################

rdatIMI <- select(datIMI, starts_with("UserID"))
rdatIMI["Item33PC2"] <- datIMI["Item33"]
rdatIMI["Item28PC2"] <- datIMI["Item28"]
rdatIMI["Item25PC2"] <- datIMI["Item25"]
rdatIMI["Item34PC2"] <- datIMI["Item34"]
rdatIMI["Item37PC2"] <- datIMI["Item37"]
rdatIMI["Item30PC2"] <- datIMI["Item30"]

rdatIMI["Item29VU"] <- datIMI["Item29"]
rdatIMI["Item26VU"] <- datIMI["Item26"]
rdatIMI["Item31VU"] <- datIMI["Item31"]
rdatIMI["Item36VU"] <- datIMI["Item36"]
rdatIMI["Item35VU"] <- datIMI["Item35"]

rdatIMI["Item24IE"] <- datIMI["Item24"]
rdatIMI["Item08IE"] <- datIMI["Item08"]
rdatIMI["Item21IE"] <- datIMI["Item21"]
rdatIMI["Item09IE"] <- datIMI["Item09"]
rdatIMI["Item12IE"] <- datIMI["Item12"]
rdatIMI["Item11IE"] <- datIMI["Item11"]

rdatIMI["Item05PC1"] <- datIMI["Item05"]
rdatIMI["Item13PC1"] <- datIMI["Item13"]
rdatIMI["Item18PC1"] <- datIMI["Item18"]
rdatIMI["Item03PC1"] <- datIMI["Item03"]

if (!file.exists('data/IMI.csv')) {
  write_csv(rdatIMI, path = 'data/IMI.csv')
}

alpha_mods <- list()

inv_keys <- c("Item34PC2", "Item05PC1", "Item13PC1", "Item18PC1", "Item03PC1")
## Intrinsic Motivation
alpha_mod <- psych::alpha(select(rdatIMI, starts_with("Item")), keys = inv_keys)
cat("\n... Intrinsic Motivation", " ...\n"); summary(alpha_mod)

alpha_mods[["IM"]] <- list(lbl = 'Intrinsic Motivation', all = alpha_mod)

inv_keys <- c("Item34PC2")
## Perceived Competence
alpha_mod <- psych::alpha(select(rdatIMI, ends_with("PC2")), keys = inv_keys)
cat("\n... Perceived Competence", " ...\n"); summary(alpha_mod)

alpha_mods[["PC2"]] <- list(lbl = 'Perceived Competence', all = alpha_mod)

inv_keys <- c()
# Value/Usefulness
alpha_mod <- psych::alpha(select(rdatIMI, ends_with("VU")), keys = inv_keys)
cat("\n... Value/Usefulness", " ...\n"); summary(alpha_mod)

alpha_mods[["VU"]] <- list(lbl = 'Value/Usefulness', all = alpha_mod)

inv_keys <- c()
## Interest/Enjoyment
alpha_mod <- alpha(select(rdatIMI, ends_with("IE")))
cat("\n... Interest/Enjoyment", " ...\n"); summary(alpha_mod)

alpha_mods[["IE"]] <- list(lbl = 'Interest/Enjoyment', all = alpha_mod)

inv_keys = c()
##  Perceived Choice
alpha_mod <- alpha(select(rdatIMI, ends_with("PC1")))
cat("\n... Perceived Choice", " ...\n"); summary(alpha_mod)

alpha_mods[["PC1"]] <- list(lbl = 'Perceived Choice', all = alpha_mod)

## Write results in an Excel Workbook
if (!file.exists("report/reliability-analysis/IMI.xlsx")) {
  filename <- "report/reliability-analysis/IMI.xlsx"
  wb <- createWorkbook(type="xlsx")
  
  write_kmo_in_workbook(kmo_mod, wb)
  write_fa_in_workbook(fa_mod, wb)
  
  write_alpha_in_workbook(alpha_mods$IM$all, wb, "Intrinsic Motivation", "IM")
  write_alpha_in_workbook(alpha_mods$PC2$all, wb, "Perceived Competence", "PC2")
  write_alpha_in_workbook(alpha_mods$VU$all, wb, "Value/Usefulness", "VU")
  write_alpha_in_workbook(alpha_mods$IE$all, wb, "Interest/Enjoyment", "IE")
  write_alpha_in_workbook(alpha_mods$PC1$all, wb, "Perceived Choice", "PC1")
  
  xlsx::saveWorkbook(wb, filename)
}

# Export summary in latex format
filename <- "report/latex/IMI-reliability-analysis.tex"
write_rel_analysis_in_latex(
  fa_mod, cfa_mod, alpha_mods
  , in_title = "adapted Portuguese version of IMI questionnaire"
  , filename = filename
  , key_labels = list('Total' = 'all')
)




