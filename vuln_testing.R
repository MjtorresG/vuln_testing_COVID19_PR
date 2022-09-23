library("tidyverse")
library("epiR")
library("lubridate")
library(ggpubr)

#descriptive statistics
tests_rafa <- read_csv("testsRafaLab.csv") %>% select(-c(positives, ageRange, patientCity)) %>%
  mutate(year = year(date)) %>% select(-date) %>% group_by(year,testType) %>% summarize(tests = sum(tests))

#PRDoH datasets were too big to be in the repo:
tests_2020PRDoH <- read_csv("tests_PRDOH_2020.csv") %>% summarize(tests= n())

tests_2021PRDoH <- read_csv("tests_PRDOH2021.csv") %>% summarize(tests= n())

#RafaLab
tests2020_Rafa <- read_csv("testsRafaLab.csv")  %>% filter(year(date) == 2020) %>% mutate(year = year(date)) %>% select(-c(positives,date,testType, year)) %>% group_by(patientCity,ageRange) %>% summarize(tests = sum(tests))

tests2021_Rafa <- read_csv("testsRafaLab.csv") %>% filter(year(date) == 2021) %>% mutate(year = year(date)) %>% select(-c(positives,date,testType, year)) %>% group_by(patientCity,ageRange) %>% summarize(tests = sum(tests))

#aggregating age-groups
#2020
testsolder <- tests2020_Rafa %>% filter(ageRange>=80 | ageRange =="100 to 109" |ageRange =="110 to 119"|ageRange =="120 to 129")

eightyplus <- testsolder %>% 
  group_by(patientCity) %>% 
  summarize(tests = sum(tests)) %>%
  mutate(ageRange = rep("80+",times=79))

tests2020_Rafa_1 <- tests2020_Rafa %>% rbind(eightyplus) %>% filter(patientCity!="No reportado") %>% filter(ageRange !="80 to 89" & ageRange !="90 to 99" & ageRange !="100 to 109" & ageRange !="110 to 119" & ageRange !="120 to 129")

#2021
testsolder <- tests2021_Rafa %>% filter(ageRange>=80 | ageRange =="100 to 109" |ageRange =="110 to 119"|ageRange =="120 to 129")

eightyplus <- testsolder %>% 
  group_by(patientCity) %>% 
  summarize(tests = sum(tests)) %>%
  mutate(ageRange = rep("80+",times=79))

tests2021_Rafa_1 <- tests2021_Rafa %>% rbind(eightyplus) %>% filter(patientCity!="No reportado") %>% filter(ageRange !="80 to 89" & ageRange !="90 to 99" & ageRange !="100 to 109" & ageRange !="110 to 119" & ageRange !="120 to 129")


#age-adjustment

#standard population
st_pop_function <- function(x) {
  if (x == "0 to 9") {
    result <- 38907
  } else if (x == "10 to 19") {
    result <- 39877
  } else if (x == "20 to 29") {
    result <- 35979
  } else if (x == "30 to 39") {
    result <- 41691
  } else if (x == "40 to 49") {
    result <- 42285
  } else if (x == "50 to 59") {
    result <- 30531
  } else if (x == "60 to 69") {
    result <- 20064
  } else if (x == "70 to 79") {
    result <- 16141
  } else if (x == "80+") {
    result <- 9159
  } else {
    result <- 0
  }
  return(result)
}

# PR population estimates for age-specific indicators
pop_estimates <- read_csv("PRM-EST2020-AGESEX.csv")

pop_est_muni <- pop_estimates %>% select(YEAR, MUNICIPIO, NAME, AGE04_TOT, AGE59_TOT, AGE1014_TOT, AGE1519_TOT, AGE2024_TOT, AGE2529_TOT, AGE3034_TOT, AGE3539_TOT, AGE4044_TOT, AGE4549_TOT, AGE5054_TOT, AGE5559_TOT, AGE6064_TOT, AGE6569_TOT, AGE7074_TOT, AGE7579_TOT, AGE8084_TOT, AGE85PLUS_TOT) %>% filter(YEAR == 14) %>% mutate(AGE09=AGE04_TOT+AGE59_TOT,
                                                                                                                                                                                                                                                                                                                                          AGE1019=AGE1014_TOT+AGE1519_TOT, AGE2029=AGE2024_TOT,AGE2529_TOT, AGE3039=AGE3034_TOT+AGE3539_TOT,
                                                                                                                                                                                                                                                                                                                                          AGE4049=AGE4044_TOT+AGE4549_TOT, AGE5059=AGE5054_TOT+AGE5559_TOT, AGE6069=AGE6064_TOT+AGE6569_TOT,
                                                                                                                                                                                                                                                                                                                                          AGE7079=AGE7074_TOT+AGE7579_TOT,
                                                                                                                                                                                                                                                                                                                                          AGE80beyond=AGE8084_TOT+AGE85PLUS_TOT) %>%
  select(-c(AGE04_TOT, AGE59_TOT, AGE1014_TOT, AGE1519_TOT, AGE2024_TOT, 
            AGE2529_TOT, AGE3034_TOT, AGE3539_TOT, AGE4044_TOT, AGE4549_TOT, AGE5054_TOT,
            AGE5559_TOT, AGE6064_TOT, AGE6569_TOT, AGE7074_TOT, AGE7579_TOT,AGE8084_TOT, AGE85PLUS_TOT))

agegrp_function <- function(x) {
  if (x == "AGE09") {
    result <- "0 to 9"
  } else if (x == "AGE1019") {
    result <- "10 to 19"
  } else if (x == "AGE2029") {
    result <- "20 to 29"
  } else if (x == "AGE3039") {
    result <- "30 to 39"
  } else if (x == "AGE4049") {
    result <- "40 to 49"
  } else if (x == "AGE5059") {
    result <- "50 to 59"
  } else if (x == "AGE6069") {
    result <- "60 to 69"
  } else if (x == "AGE7079") {
    result <- "70 to 79"
  } else {
    result <- "80+"
  }
  return(result)
}

pop_est_muni2 <- pop_est_muni %>% select(-c(MUNICIPIO,YEAR)) %>% 
  pivot_longer(-NAME, names_to = "age_group", values_to = "pop_est") %>% 
  mutate(ageRange = lapply(age_group, agegrp_function)) %>% select(-age_group)

#adjusting with epiR - 2020
testrate <- tests2020_Rafa_1 %>% mutate(st_pop = lapply(ageRange, st_pop_function))

testrate2 <-testrate[order(testrate$patientCity), ] %>% cbind(pop_est_muni2$pop_est) %>% rename(pop_est = ...5)

citys <- unique(testrate2$patientCity)
ages<-c("0 to 9","10 to 19","20 to 29","30 to 39", "40 to 49","50 to 59","60 to 69", "70 to 79","80+")

obs <- matrix(testrate2$tests,nrow=78,ncol= 9, dimnames = list(citys,ages),byrow=T)

tar <- matrix(testrate2$pop_est, nrow=78, ncol=9, dimnames = list(citys,ages),byrow=T)

stpop_testr <- testrate2 %>% filter(patientCity=="Arecibo")
std <- matrix(stpop_testr$st_pop, nrow=1,dimnames = list(c("city"),ages))

std2<-as.matrix(unlist(std))
rownames(std2) <- ages
colnames(std2) <- "city" 

adjusted_test2020 <- epi.directadj(obs, tar, std2, units=100000)

adjusted_test2020_2 <- as.data.frame(adjusted_test2020) %>% select(crude.strata,crude.cov,crude.est,adj.strata.strata, adj.strata.est, adj.strata.lower, adj.strata.upper)

adjusted_test2020_3 <- adjusted_test2020_2 %>% select(adj.strata.strata, adj.strata.est, adj.strata.lower, adj.strata.upper) %>% slice(1:78)

#2021
testrate <- tests2021_Rafa_1 %>% mutate(st_pop = lapply(ageRange, st_pop_function))

testrate2 <-testrate[order(testrate$patientCity), ] %>% cbind(pop_est_muni2$pop_est) %>% rename(pop_est = ...5)

citys <- unique(testrate2$patientCity)
ages<-c("0 to 9","10 to 19","20 to 29","30 to 39", "40 to 49","50 to 59","60 to 69", "70 to 79","80+")

obs <- matrix(testrate2$tests,nrow=78,ncol= 9, dimnames = list(citys,ages),byrow=T)

tar <- matrix(testrate2$pop_est, nrow=78, ncol=9, dimnames = list(citys,ages),byrow=T)

stpop_testr <- testrate2 %>% filter(patientCity=="Arecibo")
std <- matrix(stpop_testr$st_pop, nrow=1,dimnames = list(c("city"),ages))

std2<-as.matrix(unlist(std))
rownames(std2) <- ages
colnames(std2) <- "city" 

adjusted_test2021 <- epi.directadj(obs, tar, std2, units=100000)

adjusted_test2021_2 <- as.data.frame(adjusted_test2021) %>% select(crude.strata,crude.cov,crude.est,adj.strata.strata, adj.strata.est, adj.strata.lower, adj.strata.upper)

adjusted_test2021_3  <- adjusted_test2021_2 %>% select(adj.strata.strata, adj.strata.est, adj.strata.lower, adj.strata.upper) %>% slice(1:78)

#social vulnerability indices
#CDC SVI's
cdcsvi <- read_csv("PuertoRico_COUNTY.csv") %>% select(c(COUNTY,RPL_THEME1,RPL_THEMES))
cdcsvi <-cdcsvi[order(cdcsvi$COUNTY), ]

cdcsocialvuln <- cdcsvi %>% select(-RPL_THEME1)
cdcsocioeco <- cdcsvi %>% select(-RPL_THEMES)

#PR SVI
vuln <- read_csv("SVI de tormos et al.csv") %>% select(GEOID, MunicipalArea, overall, SOCECO) %>%
  group_by(MunicipalArea,.add = T) %>% 
  filter(SOCECO>0, overall>0) %>%
  summarize(mean_overall = mean(overall), mean_socio = mean(SOCECO),.groups = "keep")

#eliminating towns with special characters, then reintroducing them because the original file gave errors due to special characters
vuln<-vuln[-c(8, 11, 15, 17, 23, 33, 39, 43, 45, 47, 50, 57, 60, 61, 64, 67),]

add1<- data.frame("Anasco",0.6886258,0.5836917)
names(add1) <- names(vuln)

add2<- data.frame("Bayamon",0.4143083,0.2868522)
names(add2) <- names(vuln)

add3<- data.frame("Canovanas",0.4454382,0.3258405)
names(add3) <- names(vuln)

add4<- data.frame("Catano",0.4400039,0.6763953)
names(add4) <- names(vuln)

add5<- data.frame("Comerio",0.6785437,0.8129088)
names(add5) <- names(vuln)

add6<- data.frame("Guanica",0.2734629,0.7454130)
names(add6) <- names(vuln)

add7<- data.frame("Juana Diaz",0.7645077,0.5473292)
names(add7) <- names(vuln)

add8<- data.frame("Las Marias",0.4456800,0.6769355)
names(add8) <- names(vuln)

add9<- data.frame("Loiza",0.5121517,0.5154944)
names(add9) <- names(vuln)

add10<- data.frame("Manati",0.7047089,0.4891578)
names(add10) <- names(vuln)

add11<- data.frame("Mayaguez",0.5204107,0.5058994)
names(add11) <- names(vuln)

add12<- data.frame("Penuelas",0.1019425,0.4413851)
names(add12) <- names(vuln)

add13<- data.frame("Rincon",0.8266456,0.5757431)
names(add13) <- names(vuln)

add14<- data.frame("Rio Grande",0.6252093,0.4758292)
names(add14) <- names(vuln)

add15<- data.frame("San German",0.6196502,0.5601976)
names(add15) <- names(vuln)

add16<- data.frame("San Sebastian",0.7806206,0.8416909)
names(add16) <- names(vuln)

vuln2 <-vuln %>% rbind(add1,add2,add3,add4,add5,add6,add7,add8,add9,add10,add11,add12,add13,add14,add15,add16) %>% select(-mean_socio) %>% arrange(MunicipalArea)


#Pearson correlations
#2020

df <- as.data.frame(matrix(c(cdcsocialvuln$RPL_THEMES, adjusted_test2020_3$adj.strata.est),
                           nrow=78,ncol=2))

ggscatter(df, "V1", "V2",
          add = "reg.line", conf.int = TRUE,
          xlab = "CDC Social Vulnerability Index", ylab = "Age-adjusted Testing Rate (per 100,000 population)",
          shape = 21, fill = "black", color = "black", size = 2) + 
  stat_cor(method = "pearson", label.x = 0.64, label.y = 60) +
  coord_cartesian(ylim = c(0,99000), xlim = c(0,1))

df1 <- as.data.frame(matrix(c(cdcsocioeco$RPL_THEME1, adjusted_test2020_3$adj.strata.est),
                            nrow=78,ncol=2))

ggscatter(df1, "V1", "V2",
          add = "reg.line", conf.int = TRUE,
          xlab = "CDC Socioeconomic Vulnerability Index", ylab = "Age-adjusted Testing Rate (per 100,000 population)",
          shape = 21, fill = "black", color = "black", size = 2) + 
  stat_cor(method = "pearson", label.x = 0.5, label.y = 7000) +
  coord_cartesian(ylim = c(0,99000), xlim = c(0,1))

df2 <- as.data.frame(matrix(c(vuln2$mean_overall, adjusted_test2020_3$adj.strata.est),
                            nrow=78,ncol=2))

ggscatter(df2, "V1", "V2",
          add = "reg.line", conf.int = TRUE,
          xlab = "Puerto Rico Social Vulnerability Index", ylab = "Age-adjusted Testing Rate (per 100,000 population)",
          shape = 21, fill = "black", color = "black", size = 2) + 
  stat_cor(method = "pearson", label.x = 0.5, label.y = 7000) +
  coord_cartesian(ylim = c(0,99000), xlim = c(0,1))

#2021
df <- as.data.frame(matrix(c(cdcsocialvuln$RPL_THEMES, adjusted_test2021_3$adj.strata.est),
                           nrow=78,ncol=2))

ggscatter(df, "V1", "V2",
          add = "reg.line", conf.int = TRUE,
          xlab = "CDC Social Vulnerability Index", ylab = "Age-adjusted Testing Rate (per 100,000 population)",
          shape = 21, fill = "black", color = "black", size = 2) + 
  stat_cor(method = "pearson", label.x = 0.64, label.y = 60) +
  coord_cartesian(ylim = c(0,300000), xlim = c(0,1))

df1 <- as.data.frame(matrix(c(cdcsocioeco$RPL_THEME1, adjusted_test2021_3$adj.strata.est),
                            nrow=78,ncol=2))

ggscatter(df1, "V1", "V2",
          add = "reg.line", conf.int = TRUE,
          xlab = "CDC Socioeconomic Vulnerability Index", ylab = "Age-adjusted Testing Rate (per 100,000 population)",
          shape = 21, fill = "black", color = "black", size = 2) + 
  stat_cor(method = "pearson", label.x = 0.5, label.y = 7000) +
  coord_cartesian(ylim = c(0,300000), xlim = c(0,1))

df2 <- as.data.frame(matrix(c(vuln2$mean_overall, adjusted_test2021_3$adj.strata.est),
                            nrow=78,ncol=2))

ggscatter(df2, "V1", "V2",
          add = "reg.line", conf.int = TRUE,
          xlab = "Puerto Rico Social Vulnerability Index", ylab = "Age-adjusted Testing Rate (per 100,000 population)",
          shape = 21, fill = "black", color = "black", size = 2) + 
  stat_cor(method = "pearson", label.x = 0.5, label.y = 7000) +
  coord_cartesian(ylim = c(0,300000), xlim = c(0,1))
