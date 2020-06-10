library(ggplot2)
library(data.table)
library(dplyr)
library(magrittr)

#setwd("...")
data = read.csv("pax_data_410_agreements_17-04-20.csv")
datanew = data

for (predictor in colnames(datanew)) {
  predictor.num.distinct.values = length(unique(datanew[, predictor]))
  if (predictor.num.distinct.values < 2) {
    datanew[, predictor] = NULL
  }
}
list.predictors.complete.ignored = c(
  "GDisAntid",
  "GRefOth",
  "GeMeOth",
  "GeLgbti",
  "GeLgbtiPos",
  "EpsFis",
  "EpsOth",
  "CprSlav",
  "SerWork",
  "SerHeal",
  "SerSs",
  "BanXb",
  "LaRefOth"
)
for (predictor in list.predictors.complete.ignored) {
  datanew[, predictor] = NULL
}
for (i in 1:ncol(datanew)) {
  if (!is.numeric(datanew[, i]) &&
      length(unique(datanew[, i])) >= 100) {
    datanew[, i] = as.character(datanew[, i])
  }
}
datanew.bin = datanew[, sapply(datanew, is.numeric), drop = F]
for (i in 1:ncol(datanew.bin)) {
  if (names(table(datanew.bin[, i]))[1] == "0" &&
      names(table(datanew.bin[, i]))[2] == "1" &&
      is.na(names(table(datanew.bin[, i]))[3])) {
    datanew.bin[, i] = as.factor(datanew.bin[, i])
  }
}
datanew.bin = datanew.bin[, sapply(datanew.bin, is.factor), drop = F]
datanew.numeric = datanew[, sapply(datanew, is.numeric), drop = F]
for (predictor in names(datanew.bin)) {
  datanew.numeric[, predictor] = NULL
}
lasotras = datanew
for (thevar in names(datanew.bin)) {
  lasotras[, thevar] = NULL
}
lasotras = lasotras[, sapply(lasotras, is.numeric), drop = F]
for (predictor in c(
  "Lgt",
  "N_characters",
  "Dat",
  "Agt",
  "AgtId",
  "Part",
  "ThrdPart",
  "OthAgr",
  "Loc1GWNO",
  "Loc2GWNO",
  "UcdpCon",
  "UcdpAgr",
  "PamAgr",
  "CowWar",
  "Con",
  "Contp",
  "PP",
  "PPName",
  "Status",
  "Agtp",
  "Stage",
  "StageSub",
  "Loc1ISO",
  "Loc2ISO"
)) {
  lasotras[, predictor] = NULL
}
datacountry = datanew
for (predictor in colnames(datanew)) {
  if (!(predictor %in% c("Loc1ISO", "Loc2ISO", "Dat"))) {
    datacountry[, predictor] = NULL
  }
}
for (irow in 1:nrow(datacountry)) {
  if (datacountry$Loc2ISO[irow] != "") {
    datacountry = rbind(
      datacountry,
      data.frame(
        AgtId = datacountry$AgtId[irow],
        Loc1ISO = datacountry$Loc2ISO[irow],
        Loc2ISO = datacountry$Loc2ISO[irow],
        Dat = datacountry$Dat[irow]
      )
    )
  }
}
datacountry$Loc2ISO = NULL
datacountry = datacountry[!(is.na(datacountry$Loc1ISO) |
                              datacountry$Loc1ISO == ""),]
datacountry$Dat = as.integer(substring(datacountry$Dat, 0, last = 4))
print(datacountry)
datacountry = datacountry %>% group_by(Loc1ISO) %>% summarise(DatMax = max(Dat))
datacountry %<>%
  mutate(
    Country = case_when(
      Loc1ISO == "AFG" ~ "Afganistán",
      Loc1ISO == "AZE" ~ "Azerbaiyán",
      Loc1ISO == "BIH" ~ "Bosnia y Herzegovina",
      Loc1ISO == "ESP" ~ "España",
      Loc1ISO == "GBR" ~ "Reino Unido",
      Loc1ISO == "GEO" ~ "Georgia",
      Loc1ISO == "HRV" ~ "Croacia",
      Loc1ISO == "MDA" ~ "Moldavia",
      Loc1ISO == "MKD" ~ "Macedonia",
      Loc1ISO == "RUS" ~ "Rusia",
      Loc1ISO == "SRB" ~ "Serbia",
      Loc1ISO == "SVN" ~ "Eslovenia",
      Loc1ISO == "TJK" ~ "Tayikistán",
      Loc1ISO == "UKR" ~ "Ucrania",
      Loc1ISO == "ARM" ~ "Armenia",
      Loc1ISO == "GRC" ~ "Grecia",
      Loc1ISO == "IRL" ~ "Irlanda",
      TRUE ~ ""
    )
  )
datanew$DatYearInt = as.integer(substring(datanew$Dat, 0, last = 4))
datanew$Dat4 = cut(as.integer(substring(data$Dat, 0, last = 4)), 8)
datanew %<>% mutate(ImPK = case_when(ImPK >= 1 ~ 1, TRUE ~ 0))
datanew %<>% mutate(ImUN = case_when(ImUN >= 1 ~ 1, TRUE ~ 0))
datanew %<>% mutate(SsrArm = case_when(SsrArm >= 1 ~ 1, TRUE ~ 0))
datanew %<>% mutate(SsrDdr = case_when(SsrDdr >= 1 ~ 1, TRUE ~ 0))
for (predictor in colnames(datanew)) {
  if (!(predictor %in% c("DatYearInt", "Dat4", "ImPK", "ImUN", "SsrArm", "SsrDdr"))) {
    datanew[, predictor] = NULL
  }
}
write.csv(datanew, "dataset1.csv")
write.csv(datacountry, "dataset2.csv")
