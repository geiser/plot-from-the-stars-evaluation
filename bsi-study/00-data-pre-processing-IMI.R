
wants <- c('readr', 'dplyr', 'reshape')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])
#devtools::install_github('ryentes/careless')
library(readxl)
library(readr)
library(dplyr)
library(reshape)
library(careless)


raw_dat <- read_excel("data/Respostas BSI.xlsx", sheet = "F Pós-Teste")
raw_dat["UserID"] <- raw_dat$`Endereço de e-mail`

##########################################################################
## Getting IMI                                                          ##
##########################################################################

dat <- dplyr::mutate(
  raw_dat
  , Item15 = raw_dat$`Fiz a atividade porque eu tinha que fazer.`
  , Item17 = raw_dat$`Fiz a atividade porque eu não tinha outra escolha.`
  , Item19 = raw_dat$`Achei que a atividade seria chata.`
  , Item21 = raw_dat$`Descreveria a atividade como muito interessante.`
  , Item22 = raw_dat$`Achei a atividade muito agradável.`
  , Item23 = raw_dat$`Eu senti que tive escolha para realizar a atividade.`
  , Item24 = raw_dat$`Enquanto estava fazendo a atividade, refleti o quanto eu gostei.`
  , Item02 = raw_dat$`Senti como se eu tivesse sido obrigado a fazer a atividade.`
  , Item05 = raw_dat$`Fiz a atividade porque eu quis.`
  , Item20 = raw_dat$`A atividade não chamou minha atenção.`
  , Item06 = raw_dat$`Realmente não tive escolha para realizar (ou não) a atividade.`
  , Item08 =  raw_dat$`Senti que não fiz a atividade por vontade própria.`
  , Item09 = raw_dat$`Gostei muito de fazer a atividade.`
  , Item12 = raw_dat$`A atividade foi divertida.`
  , Item25 = raw_dat$`Acho que eu sou muito bom nesta atividade.`
  , Item31 = raw_dat$`Acho que a atividade é importante para fazer porque pode me ensinar sobre matemática.`
  , Item32 = raw_dat$`Estaria disposto a fazer novamente a atividade porque tem algum valor para mim.`
  , Item27 = raw_dat$`Estou satisfeito com o meu desempenho nesta tarefa.`
  , Item33 = raw_dat$`Acredito que fazer a atividade pode ser benéfico para mim.`
  , Item26 = raw_dat$`Acredito que fiz muito bem a atividade em comparação com os outros.`
  , Item34 = raw_dat$`Acredito que esta atividade pode ser de algum valor para mim.`
  , Item35 = raw_dat$`Fazer a atividade foi útil para aprender funções.`
  , Item28 = raw_dat$`Eu era bastante habilidoso nessa atividade.`
  , Item30 = raw_dat$`Foi uma atividade que não podia fazê-la muito bem.`
  , Item36 = raw_dat$`Acho que fazer essa atividade poderia me ajudar a aprender funções.`
  , Item37 = raw_dat$`Acho que a atividade é importante.`
  , Item29 = raw_dat$`Depois de trabalhar nesta atividade por algum tempo, senti-me bastante competente.`
)

# removing careless responders
print(careless_info <- careless::longstring(select(dat, starts_with("Item"))))
dat_wo <- dat[careless_info <= 12,]

IMI <- select(dat_wo, starts_with("UserID"), starts_with("Item"))
if (!file.exists('data/SourceIMI.csv')) {
  write_csv(IMI, 'data/SourceIMI.csv')
}

userids <- IMI$UserID

##########################################################################
## Getting participants information                                     ##
##########################################################################

raw_dat <- read_excel("data/Respostas BSI.xlsx", sheet = "F Pré-Teste")
raw_dat["UserID"] <- raw_dat$`Endereço de e-mail`

dat <- dplyr::mutate(
  raw_dat
  , Age = if_else(
    raw_dat$Idade >= 25, "adulthood"
    , if_else(raw_dat$Idade >= 18, "emerging-adulthood"
              , if_else(raw_dat$Idade >= 11, "adolescence", "childhood"))
    )
  , Sex = if_else(raw_dat$Sexo == "Masculino", "male", "female")
  , LikeDomainContent = raw_dat$`Você gosta de estudar sobre funções matemáticas?`
  , LikeActionGames = if_else(
    grepl("Ação", raw_dat$`Qual(is) tipo(s) de jogo(s) você mais gosta?`), "yes", "no")
  , LikeAdventureGames = if_else(
    grepl("Aventura", raw_dat$`Qual(is) tipo(s) de jogo(s) você mais gosta?`), "yes", "no")
  , LikeFightingGames = if_else(
    grepl("Luta", raw_dat$`Qual(is) tipo(s) de jogo(s) você mais gosta?`), "yes", "no")
  , LikeFictionGames = if_else(
    grepl("Ficção Interativa", raw_dat$`Qual(is) tipo(s) de jogo(s) você mais gosta?`), "yes", "no")
  , LikePlatformerGames = if_else(
    grepl("Plataforma", raw_dat$`Qual(is) tipo(s) de jogo(s) você mais gosta?`), "yes", "no")
  , LikePuzzleGames = if_else(
    grepl("Quebra-Cabeças", raw_dat$`Qual(is) tipo(s) de jogo(s) você mais gosta?`), "yes", "no")
  , LikeRacingGames = if_else(
    grepl("Corrida", raw_dat$`Qual(is) tipo(s) de jogo(s) você mais gosta?`), "yes", "no")
  , LikeMusicalGames = if_else(
    grepl("Rítmico/Musical", raw_dat$`Qual(is) tipo(s) de jogo(s) você mais gosta?`), "yes", "no")
  , LikeMMORPGGames = if_else(
    grepl("(MMO)RPG", raw_dat$`Qual(is) tipo(s) de jogo(s) você mais gosta?`), "yes", "no")
  , LikeShooterGames = if_else(
    grepl("Tiro", raw_dat$`Qual(is) tipo(s) de jogo(s) você mais gosta?`), "yes", "no")
  , LikeSimulationGames = if_else(
    grepl("Simulacao", raw_dat$`Qual(is) tipo(s) de jogo(s) você mais gosta?`), "yes", "no")
  , LikeSportsGames = if_else(
    grepl("Esportes", raw_dat$`Qual(is) tipo(s) de jogo(s) você mais gosta?`), "yes", "no")
  , LikeStrategyGames = if_else(
    grepl("Estratégia", raw_dat$`Qual(is) tipo(s) de jogo(s) você mais gosta?`), "yes", "no")
  , LikeNovelGames = if_else(
    grepl("Visual Novel", raw_dat$`Qual(is) tipo(s) de jogo(s) você mais gosta?`), "yes", "no")
)
participants <- select(dat, starts_with("UserID")
                       , starts_with("Age"), ends_with("Sex")
                       , starts_with("AcademicDegree")
                       , starts_with("Known"), starts_with("Like"), starts_with("SpendTime"))

participants <- mutate(
  participants
  , LikeDomainContent = if_else(participants$LikeDomainContent > median(participants$LikeDomainContent), "yes", "no")
  #, KnownDomainContent = if_else(participants$KnownDomainContent > median(participants$KnownDomainContent), "yes", "no")
)

if (!file.exists('data/Participants.csv'))  {
  write_csv(participants, path = "data/Participants.csv")
}

##########################################################################
## Getting pre- and posttest information                                ##
##########################################################################

raw_dat <- read_excel("data/Respostas BSI.xlsx", sheet = "F Pré-Teste")
raw_dat["UserID"] <- raw_dat$`Endereço de e-mail`

predat <- dplyr::mutate(
  raw_dat
  , Pre01 = if_else(raw_dat$`1) Imagem acima` == "y = 2x² + 3x", 1, 0)
  , Pre02 = if_else(raw_dat$`2) Imagem acima` == "y = 4x²", 1, 0)
  , Pre03 = if_else(raw_dat$`3) Imagem acima` == "y = -0.34x²", 1, 0)
  , Pre04 = if_else(raw_dat$`4) y = -2x²` == "Resposta 3", 1, 0)
  , Pre05 = if_else(raw_dat$`5) Imagem acima` == "y = 3x + 4", 1, 0)
  , Pre06 = if_else(raw_dat$`6) y = 0.5x²` == "Resposta 1", 1, 0)
  , Pre07 = if_else(raw_dat$`7) Imagem acima` == "y = 7x²", 1, 0)
  , Pre08 = if_else(raw_dat$`8) Imagem acima` == "y = 3.2x² - 4x", 1, 0)
  , Pre09 = if_else(raw_dat$`9) Imagem acima` == "y = -3x", 1, 0)
  , Pre10 = if_else(raw_dat$`10) y = 3x` == "Resposta 3", 1, 0)
  , Pre11 = if_else(raw_dat$`11) Imagem acima` == "Azul: y = 2x² + 3x - 3 |  Vermelho: y = 2x² + 3x - 1", 1, 0)
  , Pre12 = if_else(raw_dat$`12) Imagem acima` == "y = 1.3^x", 1, 0)
  , Pre13 = if_else(raw_dat$`13) Imagem acima` == "y = 0.2^x", 1, 0)
  , Pre14 = if_else(raw_dat$`14) Imagem acima` == "y = 1.06^x", 1, 0)
  , Pre15 = if_else(raw_dat$`15) Imagem acima` == "y = 5sen(x)", 1, 0)
  , Pre16 = if_else(raw_dat$`16) Imagem acima` == "y = 0.7sen(x)", 1, 0)
  , Pre17 = if_else(raw_dat$`17) Imagem acima` == "y = sen(4x)", 1, 0)
  , Pre18 = if_else(raw_dat$`18) Imagem acima` == "y = sen(0.2x)", 1, 0)
  , Pre19 = if_else(raw_dat$`19) Imagem acima` == "y = 4sen(x - 2)", 1, 0)
  , Pre20 = if_else(raw_dat$`20) Imagem acima` == "y = 4sen(x + 2)", 1, 0)
)


raw_dat <- read_excel("data/Respostas BSI.xlsx", sheet = "F Pós-Teste")
raw_dat["UserID"] <- raw_dat$`Endereço de e-mail`

posdat <- dplyr::mutate(
  raw_dat
  , Pos01 = if_else(raw_dat$`1) Imagem acima` == "y = -8x", 1, 0)
  , Pos02 = if_else(raw_dat$`2) Imagem acima` == "y = 0.4x²", 1, 0)
  , Pos03 = if_else(raw_dat$`3) Imagem acima` == "y = 0.5x² - 4.5x", 1, 0)
  , Pos04 = if_else(raw_dat$`4) y = 0.8x² + 4x` == "Resposta 1", 1, 0)
  , Pos05 = if_else(raw_dat$`5) Imagem acima` == "Azul: y = 4x - 8 | Vermelho: y = 4x - 5", 1, 0)
  , Pos06 = if_else(raw_dat$`6) Imagem acima` == "y = -2x²", 1, 0)
  , Pos07 = if_else(raw_dat$`7) Imagem acima` == "y = 5x²", 1, 0)
  , Pos08 = if_else(raw_dat$`8) Imagem acima` == "y = 3x", 1, 0)
  , Pos09 = if_else(raw_dat$`9) Imagem acima` == "y = 3x + 2", 1, 0)
  , Pos10 = if_else(raw_dat$`10) Imagem acima` == "y = -0.2x² + 5", 1, 0)
  , Pos11 = if_else(raw_dat$`11) Imagem acima` == "y = 0.5x²", 1, 0)
  , Pos12 = if_else(raw_dat$`12) Imagem acima` == "y = 1.7^x", 1, 0)
  , Pos13 = if_else(raw_dat$`13) Imagem acima` == "y = 0.6^x", 1, 0)
  , Pos14 = if_else(raw_dat$`14) Imagem acima` == "y = 1.03^x", 1, 0)
  , Pos15 = if_else(raw_dat$`15) Imagem acima` == "y = 4sen(x)", 1, 0)
  , Pos16 = if_else(raw_dat$`16) Imagem acima` == "y = 0.6sen(x)", 1, 0)
  , Pos17 = if_else(raw_dat$`17) Imagem acima` == "y = sen(5x)", 1, 0)
  , Pos18 = if_else(raw_dat$`18) Imagem acima` == "y = sen(0.4x)", 1, 0)
  , Pos19 = if_else(raw_dat$`19) Imagem acima` == "y = 3sen(x - 1.5)", 1, 0)
  , Pos20 = if_else(raw_dat$`20) Imagem acima` == "y = 3sen(x + 1.5)", 1, 0)
)

dat <- merge(predat, posdat, by = "UserID")
prepost <- select(dat, starts_with("UserID"), starts_with("Pre"), starts_with("Pos"))

prepost <- dplyr::mutate(
  prepost
  , NPre = prepost$Pre01+prepost$Pre02+prepost$Pre03+prepost$Pre04+prepost$Pre05+prepost$Pre06+prepost$Pre07+prepost$Pre08+prepost$Pre09+prepost$Pre10
  +prepost$Pre11+prepost$Pre12+prepost$Pre13+prepost$Pre14+prepost$Pre15+prepost$Pre16+prepost$Pre17+prepost$Pre18+prepost$Pre19+prepost$Pre20
  , NPos = prepost$Pos01+prepost$Pos02+prepost$Pos03+prepost$Pos04+prepost$Pos05+prepost$Pos06+prepost$Pos07+prepost$Pos08+prepost$Pos09+prepost$Pos10
  +prepost$Pos11+prepost$Pos12+prepost$Pos13+prepost$Pos14+prepost$Pos15+prepost$Pos16+prepost$Pos17+prepost$Pos18+prepost$Pos19+prepost$Pos20
)
prepost <- dplyr::mutate(prepost, GainScore = prepost$NPos-prepost$NPre)

if (!file.exists('data/PrePostTest.csv'))  {
  write_csv(prepost, path = "data/PrePostTest.csv")
}

