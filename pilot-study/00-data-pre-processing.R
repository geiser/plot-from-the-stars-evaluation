
wants <- c('readr', 'dplyr', 'reshape')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])
#devtools::install_github('ryentes/careless')

library(readr)
library(dplyr)
library(reshape)
library(careless)

raw_dat <- read_csv('data/Avaliação Web 03%2F2018 (Responses) - Form Responses 1.csv')
raw_dat["UserID"] <- rownames(raw_dat)

##########################################################################
## Getting IMI                                                          ##
##########################################################################

dat <- dplyr::mutate(
  raw_dat
  , Item01 = raw_dat$`Não me esforcei muito para realizar bem a atividade.`
  , Item02 = raw_dat$`Eu me senti muito tenso ao realizar a atividade.`
  , Item03 = raw_dat$`Fiz a atividade porque eu tinha que fazer.`
  , Item04 = raw_dat$`Eu me senti ansioso enquanto trabalhava na atividade.`
  , Item05 = raw_dat$`Fiz a atividade porque eu não tinha outra escolha.`
  , Item06 = raw_dat$`Senti-me pressionado enquanto fazia a atividade.`
  , Item07 = raw_dat$`Achei que a atividade seria chata.`
  , Item08 = raw_dat$`Descreveria a atividade como muito interessante.`
  , Item09 = raw_dat$`Achei a atividade muito agradável.`
  , Item10 = raw_dat$`Eu senti que tive escolha para realizar a atividade.`
  , Item11 = raw_dat$`Enquanto estava fazendo a atividade, refleti o quanto eu gostei.`
  , Item12 = raw_dat$`Foi muito descontraído realizar a atividade.`
  , Item13 = raw_dat$`Senti como se eu tivesse sido obrigado a fazer a atividade.`
  , Item14 = raw_dat$`Me esforcei muito na realização da atividade.`
  , Item15 = raw_dat$`Para mim foi importante realizar bem a atividade.`
  , Item16 = raw_dat$`Fiz a atividade porque eu quis.`
  , Item17 = raw_dat$`A atividade não chamou minha atenção.`
  , Item18 = raw_dat$`Realmente não tive escolha para realizar (ou não) a atividade.`
  , Item19 = raw_dat$`Não coloquei muita esforço na atividade.`
  , Item20 = raw_dat$`Senti que não fiz a atividade por vontade própria.`
  , Item21 = raw_dat$`Gostei muito de fazer a atividade.`
  , Item22 = raw_dat$`Tive que me esforçar muito na atividade.`
  , Item23 = raw_dat$`Não me senti nervoso ao realizar a atividade.`
  , Item24 = raw_dat$`A atividade foi divertida.`
  , Item25 = raw_dat$`Acho que eu sou muito bom nesta atividade.`
  , Item26 = raw_dat$`Acho que a atividade é importante para fazer porque pode me ensinar sobre matemática.`
  , Item27 = raw_dat$`Estaria disposto a fazer novamente a atividade porque tem algum valor para mim.`
  , Item28 = raw_dat$`Estou satisfeito com o meu desempenho nesta tarefa.`
  , Item29 = raw_dat$`Acredito que fazer a atividade pode ser benéfico para mim.`
  , Item30 = raw_dat$`Acredito que fiz muito bem a atividade em comparação com os outros.`
  , Item31 = raw_dat$`Acredito que esta atividade pode ser de algum valor para mim.`
  , Item32 = raw_dat$`Fazer a atividade foi útil para aprender funções.`
  , Item33 = raw_dat$`Eu era bastante habilidoso nessa atividade.`
  , Item34 = raw_dat$`Foi uma atividade que não podia fazer-a muito bem.`
  , Item35 = raw_dat$`Acho que fazer essa atividade poderia me ajudar a aprender funções.`
  , Item36 = raw_dat$`Acho que a atividade é importante.`
  , Item37 = raw_dat$`Depois de trabalhar nesta atividade por algum tempo, senti-me bastante competente.`
)

# removing careless responders
print(careless_info <- careless::longstring(select(dat, starts_with("Item")), na=T))
dat_wo <- dat[careless_info <= 12,]

IMI <- select(dat_wo, starts_with("UserID"), starts_with("Item"))
if (!file.exists('data/SourceIMI.csv')) {
  write_csv(IMI, 'data/SourceIMI.csv')
}

userids <- IMI$UserID

##########################################################################
## Getting participants information                                     ##
##########################################################################

dat <- dplyr::mutate(
  raw_dat
  , Age = if_else(
    raw_dat$`Idade [Age]` >= 25, "adulthood"
    , if_else(raw_dat$`Idade [Age]` >= 18, "emerging-adulthood"
              , if_else(raw_dat$`Idade [Age]` >= 11, "adolescence", "childhood"))
    )
  , Sex = if_else(raw_dat$`Sexo [Sex]` == "Masculino [Male]", "male", "female")
  , AcademicDegree = if_else(
    raw_dat$`Grau de Escolaridade [Degree of Education]` == "Graduação [College]", "undergraduate"
    , if_else(raw_dat$`Grau de Escolaridade [Degree of Education]` == "Mestrado [Master's]"
              | raw_dat$`Grau de Escolaridade [Degree of Education]` == "Doutorado [Doctorate/PhD]"
              , "postgraduate"
              , "basic-school")
    )
  , LikeDomainContent = (raw_dat$`Você gosta de Matemática? [Do you like math?]`
                             +raw_dat$`Você sente prazer em aprender Matemática? [Do you enjoy learning math?]`
                             +raw_dat$`Você gosta de estudar sobre funções matemáticas? [Do you like to study mathematical functions?]`)/3
  , KnownDomainContent = (raw_dat$`Você se acha bom em Matemática? [Do you think you are good at math?]`
                              +raw_dat$`Você conhece funções matemáticas? [Do you know mathematical functions?]`
                              +14-(raw_dat$`Você tem dificuldade em Matemática? [Do you have difficulty in math?]`
                                   +raw_dat$`Você tem dificuldade em aprender sobre funções matemáticas? [Do you have difficulty learning about mathematical functions?]`))/4
  , LikeActionGames = if_else(
    grepl("Action", raw_dat$`Qual tipo de jogo você mais gosta? [What kind of game do you like more?]`), "yes", "no")
  , LikeAdventureGames = if_else(
    grepl("Adventure", raw_dat$`Qual tipo de jogo você mais gosta? [What kind of game do you like more?]`), "yes", "no")
  , LikeFightingGames = if_else(
    grepl("Fighting", raw_dat$`Qual tipo de jogo você mais gosta? [What kind of game do you like more?]`), "yes", "no")
  , LikeFictionGames = if_else(
    grepl("Interactive Fiction", raw_dat$`Qual tipo de jogo você mais gosta? [What kind of game do you like more?]`), "yes", "no")
  , LikePlatformerGames = if_else(
    grepl("Platformer", raw_dat$`Qual tipo de jogo você mais gosta? [What kind of game do you like more?]`), "yes", "no")
  , LikePuzzleGames = if_else(
    grepl("Puzzle", raw_dat$`Qual tipo de jogo você mais gosta? [What kind of game do you like more?]`), "yes", "no")
  , LikeRacingGames = if_else(
    grepl("Racing", raw_dat$`Qual tipo de jogo você mais gosta? [What kind of game do you like more?]`), "yes", "no")
  , LikeMusicalGames = if_else(
    grepl("Musical", raw_dat$`Qual tipo de jogo você mais gosta? [What kind of game do you like more?]`), "yes", "no")
  , LikeMMORPGGames = if_else(
    grepl("RPG", raw_dat$`Qual tipo de jogo você mais gosta? [What kind of game do you like more?]`), "yes", "no")
  , LikeShooterGames = if_else(
    grepl("Shooter", raw_dat$`Qual tipo de jogo você mais gosta? [What kind of game do you like more?]`), "yes", "no")
  , LikeSimulationGames = if_else(
    grepl("Simulation", raw_dat$`Qual tipo de jogo você mais gosta? [What kind of game do you like more?]`), "yes", "no")
  , LikeSportsGames = if_else(
    grepl("Sports", raw_dat$`Qual tipo de jogo você mais gosta? [What kind of game do you like more?]`), "yes", "no")
  , LikeStrategyGames = if_else(
    grepl("Strategy", raw_dat$`Qual tipo de jogo você mais gosta? [What kind of game do you like more?]`), "yes", "no")
  , LikeNovelGames = if_else(
    grepl("Visual Novel", raw_dat$`Qual tipo de jogo você mais gosta? [What kind of game do you like more?]`), "yes", "no")
  # Country and State can't be used becouse there are less of 5 data in each data
  , SpendTime = raw_dat$`Por quanto tempo em minutos você jogou?`
)
participants <- select(dat, starts_with("UserID")
                       , starts_with("Age"), ends_with("Sex")
                       , starts_with("AcademicDegree")
                       , starts_with("Known"), starts_with("Like"), starts_with("SpendTime"))
participants <- participants[participants$UserID %in% userids,]

participants <- mutate(
  participants
  , LikeDomainContent = if_else(participants$LikeDomainContent > median(participants$LikeDomainContent), "yes", "no")
  , KnownDomainContent = if_else(participants$KnownDomainContent > median(participants$KnownDomainContent), "yes", "no")
)

if (!file.exists('data/Participants.csv'))  {
  write_csv(participants, path = "data/Participants.csv")
}

##########################################################################
## Getting pre- and posttest information                                ##
##########################################################################
dat <- dplyr::mutate(
  raw_dat
  , Pre01 = if_else(raw_dat$`1) Imagem acima [Image above]` == "y = 2x² + 3x", 1, 0)
  , Pre02 = if_else(raw_dat$`2) Imagem acima [Image above]` == "y = 4x²", 1, 0)
  , Pre03 = if_else(raw_dat$`3) Imagem acima [Image above]` == "y = -0.34x²", 1, 0)
  , Pre04 = if_else(raw_dat$`4) y = -2x²` == "Resposta 3 [Answer 3]", 1, 0)
  , Pre05 = if_else(raw_dat$`5) Imagem acima [Image above]` == "y = 3x + 4", 1, 0)
  , Pre06 = if_else(raw_dat$`6) y = 0.5x²` == "Resposta 1 [Answer 1]", 1, 0)
  , Pre07 = if_else(raw_dat$`7) Imagem acima [Image above]` == "y = 7x²", 1, 0)
  , Pre08 = if_else(raw_dat$`8) Imagem acima [Image above]` == "y = 3.2x² - 4x", 1, 0)
  , Pre09 = if_else(raw_dat$`9) Imagem acima [Image above]` == "y = -3x", 1, 0)
  , Pre10 = if_else(raw_dat$`10) y = 3x` == "Resposta 3 [Answer 3]", 1, 0)
  , Pre11 = if_else(raw_dat$`11) Imagem acima [Image above]` == "Azul [Blue]: y = 2x² + 3x - 3 |  Vermelho [Red]: y = 2x² + 3x - 1", 1, 0)
  
  , Pos01 = if_else(raw_dat$`1) Imagem acima [Image above]_1` == "y = -8x", 1, 0)
  , Pos02 = if_else(raw_dat$`2) Imagem acima [Image above]_1` == "y = 0.4x²", 1, 0)
  , Pos03 = if_else(raw_dat$`3) Imagem acima [Image above]_1` == "y = 0.5x² - 4.5x", 1, 0)
  , Pos04 = if_else(raw_dat$`4) y = 0.8x² + 4x` == "Resposta 1 [Answer 1]", 1, 0)
  , Pos05 = if_else(raw_dat$`5) Imagem acima [Image above]_1` == "Azul [Blue]: y = 4x - 8 | Vermelho [Red]: y = 4x - 5", 1, 0)
  , Pos06 = if_else(raw_dat$`6) Imagem acima [Image above]` == "y = -2x²", 1, 0)
  , Pos07 = if_else(raw_dat$`7) Imagem acima [Image above]_1` == "y = 5x²", 1, 0)
  , Pos08 = if_else(raw_dat$`8) Imagem acima [Image above]_1` == "y = 3x", 1, 0)
  , Pos09 = if_else(raw_dat$`9) Imagem acima [Image above]_1` == "y = 3x + 2", 1, 0)
  , Pos10 = if_else(raw_dat$`10) Imagem acima [Image above]` == "y = -0.2x² + 5", 1, 0)
  , Pos11 = if_else(raw_dat$`11) Imagem acima [Image above]_1` == "y = 0.5x²", 1, 0)
)

prepost <- select(dat, starts_with("UserID"), starts_with("Pre"), starts_with("Pos"))
prepost <- prepost[prepost$UserID %in% userids,]

prepost <- dplyr::mutate(
  prepost
  , NPre = prepost$Pre01+prepost$Pre02+prepost$Pre03+prepost$Pre04+prepost$Pre05+prepost$Pre06+prepost$Pre07+prepost$Pre08+prepost$Pre09+prepost$Pre10+prepost$Pre11
  , NPos = prepost$Pos01+prepost$Pos02+prepost$Pos03+prepost$Pos04+prepost$Pos05+prepost$Pos06+prepost$Pos07+prepost$Pos08+prepost$Pos09+prepost$Pos10+prepost$Pos11
)
prepost <- dplyr::mutate(prepost, GainScore = prepost$NPos-prepost$NPre)

if (!file.exists('data/PrePostTest.csv'))  {
  write_csv(prepost, path = "data/PrePostTest.csv")
}

