
## clear workspace before process the data
options(scipen = 999)
rm(list = ls())

## install required packages and dependencies automatically
list.of.packages <- c("car", "psych","texreg","rstudioapi","data.table", "devtools",
                      "haven", "magrittr", "igraph", "intergraph", "ggplot2", "jtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) devtools::install_cran(new.packages, dependencies = T)
if(!("patchwork" %in% installed.packages()[,"Package"])) devtools::install_github("thomasp85/patchwork")

## automatically setting working directories
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# setwd("..")

## load custom function
source("dev/helper_functions.R")

## load required packages
require(haven)
require(data.table)
require(texreg)
require(parallel)
require(magrittr) ## pipe (%>%) operator
require(igraph)
require(ggplot2)
require(patchwork)

## load data files locally (later to be deposited publicly)
## paths_to_file <- "/Users/songh95/Dropbox/(19) 2018 Summer/CR_2018/"
dat <- haven::read_sav("Dat/DiscussionForumThreeWavePanel(N=341).sav")
setDT(dat)
## total sample (341)
dat[, .N]

## check the distribution of ideological leaning of one's own writing
## by self-reported ideology
dat[, .(kv56, kv49)] %>% table # one's own left-leaning writing
dat[, .(kv57, kv49)] %>% table
dat[, .(kv58, kv49)] %>% table # one's own right-leaning writing

## check the distribution of ideological leaning of others writing
## by self-reported ideology
dat[, .(kv60, kv49)] %>% table # others' left-leaning writing
dat[, .(kv61, kv49)] %>% table
dat[, .(kv62, kv49)] %>% table # others' right-leaning writing

## difference of one's own writing and that of others, by ideology
dat[, .(kv60 - kv56, kv49)] %>% table
dat[, .(kv61 - kv57, kv49)] %>% table
dat[, .(kv62 - kv58, kv49)] %>% table


## reading log data
net <- read.csv("Dat/Reading_1113-1126_Participants(N=341)_Count(N=160836).csv")
net2 <- read.csv("Dat/Reading_1127-1219_Participants(N=341)_Count(N=160836).csv")
net2 <- data.frame(reading.time = net2$Reading.Time, reader.id = net2$Reader.Id, poster.id = net2$Poster.Id)
net <- rbind(net, net2)
setDT(net)
net[, reading.date := as.Date(reading.time, format = "%Y-%m-%d %H:%M:%S")]

## study period (27 days)
net[, length(unique(reading.date))]
date.range <- unique(sort(net[, reading.date]))

## make a network objects
g <- list()
for (i in 1:length(date.range)) {

  g[[i]] <- net[reading.date %in% date.range[i],]
  g[[i]] <- data.frame(g[[i]][,2], g[[i]][,3]) ## 2 == reader.id, 3 == poster.id
  g[[i]] <- as.matrix(g[[i]])
  g[[i]] <- igraph::graph.edgelist(g[[i]], directed = TRUE)
  g[[i]] <- igraph::get.adjacency(g[[i]], sparse = T)
}

## exposure to disagreement (reading others' posts)
## in wave 1, there are few Rs who did not indicated their initial candidate pref.
## we need to impute their preference using feeling thermo ratings
dat[is.na(canpref1), pv254 - pv255] ## all zero
## or stated "would-be candidate" they might vote for Qs
dat[, canpref1.imputed := canpref1]
dat[is.na(canpref1), canpref1.imputed := car::recode(kv2, "1 = 0; 2 = 1; else = 2")]
dat[, table(canpref1, canpref1.imputed, exclude = NULL)]

## calculate the proportion of exposure
exp.dis.daily <- lapply(seq_len(length(date.range)), function(i) {
                          if (i <= 7) {
                            out <- cal.exposure.disagree(net = g[[i]], canpref = dat$canpref1.imputed, prop = T)
                            out <- cbind(id = 1:341, out, day = i)
                          } else if (i <= 21) {
                            out <- cal.exposure.disagree(net = g[[i]], canpref = dat$canpref2, prop = T)
                            out <- cbind(id = 1:341, out, day = i)
                          } else {
                            out <- cal.exposure.disagree(net = g[[i]], canpref = dat$canpref3, prop = T)
                            out <- cbind(id = 1:341, out, day = i)
                          }
  return(out)
 })

exp.dis.daily <- setDT(as.data.frame(do.call(rbind, exp.dis.daily)))

## until W1
cleaned.data <- exp.dis.daily[day <= 7, .(safe.disc.W1 = mean(safe.disc),
                                   dangerous.disc.W1 = mean(dangerous.disc)), by = id] %>%
  cbind(., exp.disagr.offline.prcpt.W1 = dat$pv323)

cleaned.data[, cor(dangerous.disc.W1, exp.disagr.offline.prcpt.W1)]



## during W2
dat[, W2.disagree.for.liberal := rowMeans(.SD), .SDcols = c("kv61", "kv62")]
dat[, W2.disagree.for.conservative := rowMeans(.SD), .SDcols = c("kv60", "kv61")]

dat[canpref2 == 1, W2.disagree.online.perception := W2.disagree.for.liberal]
dat[canpref2 == 0, W2.disagree.online.perception := W2.disagree.for.conservative]

cleaned.data <- cleaned.data %>% merge(.,
                exp.dis.daily[(day <= 21 & day > 7),
                              .(safe.disc.W2 = mean(safe.disc),
                                dangerous.disc.W2 = mean(dangerous.disc)), by = id] %>%
                 cbind(., exp.disagr.offline.prcpt.W2 = dat$kv218) %>%
                 cbind(., exp.disagr.online.prcpt.W2 = dat$W2.disagree.online.perception),
                 by = "id")

cleaned.data[, cor(dangerous.disc.W2, exp.disagr.offline.prcpt.W2)]
bivariate.perm.test(cleaned.data, "dangerous.disc.W2", "exp.disagr.offline.prcpt.W2", cor)
#         obs  llci.0.025  ulci.0.975
# -0.04797520 -0.09458145  0.10673502

cleaned.data[, cor(dangerous.disc.W2, exp.disagr.online.prcpt.W2)]
bivariate.perm.test(cleaned.data, "dangerous.disc.W2", "exp.disagr.online.prcpt.W2", cor)
#       obs llci.0.025 ulci.0.975
# 0.2851594 -0.1049099  0.0980455

## create differences between perception and behavioral measures
## (+) values indicate the overestimation, and (-) means underestimation
cleaned.data[, diff.exp.disagree.W2 := exp.disagr.online.prcpt.W2 - dangerous.disc.W2]
cleaned.data[, summary(diff.exp.disagree.W2)]

## permutation test indicates that the difference between
## perception and objective behavior is significantly differ,
## in a way that people tend to overestimate the exposure to differences
diff.perm.test(cleaned.data, "exp.disagr.online.prcpt.W2", "dangerous.disc.W2", rep = 10000)

## during W3
dat[, W3.disagree.for.liberal := rowMeans(.SD), .SDcols = c("hv116", "hv117")]
dat[, W3.disagree.for.conservative := rowMeans(.SD), .SDcols = c("hv115", "hv116")]

dat[canpref3 == 1, W3.disagree.online.perception := W3.disagree.for.liberal]
dat[canpref3 == 0, W3.disagree.online.perception := W3.disagree.for.conservative]

cleaned.data <- cleaned.data %>% merge(.,
                exp.dis.daily[day > 21, .(safe.disc.W3 = mean(safe.disc),
                                   dangerous.disc.W3 = mean(dangerous.disc)), by = id] %>%
                cbind(., exp.disagr.offline.prcpt.W3 = dat$hv277) %>%
                cbind(., exp.disagr.online.prcpt.W3 = dat$W3.disagree.online.perception),
                by = "id")

cleaned.data[, cor(dangerous.disc.W3, exp.disagr.offline.prcpt.W3)]
bivariate.perm.test(cleaned.data, "dangerous.disc.W3", "exp.disagr.offline.prcpt.W3", cor)
#        obs  llci.0.025  ulci.0.975
# 0.03331218 -0.10793688  0.10286882

cleaned.data[, cor(dangerous.disc.W3, exp.disagr.online.prcpt.W3)]
bivariate.perm.test(cleaned.data, "dangerous.disc.W3", "exp.disagr.online.prcpt.W3", cor)
#       obs llci.0.025 ulci.0.975
# 0.2971547 -0.1130125  0.1110424

## create differences between perception and behavioral measures
## (+) values indicate the overestimation, and (-) means underestimation
cleaned.data[, diff.exp.disagree.W3 := exp.disagr.online.prcpt.W3 - dangerous.disc.W3]
cleaned.data[, summary(diff.exp.disagree.W3)]

## permutation test indicates that the difference between
## perception and objective behavior is significantly differ,
## in a way that people tend to overestimate the exposure to differences
diff.perm.test(cleaned.data, "exp.disagr.online.prcpt.W3", "dangerous.disc.W3", rep = 10000)


## eigenvector centrality
daily.centrality <- lapply(seq_len(length(date.range)), function(i) {
  centr.clo <- g[[i]] %>%
    igraph::graph_from_adjacency_matrix(., mode = "directed", weighted = TRUE) %>%
    igraph::centr_eigen(., directed = T)
  centr.eigen <- centr.clo$vector
  centr.eigen <- scale(centr.eigen)[,1]
  centr.eigen <- cbind(id = 1:341, centr.eigen = centr.eigen, day = i)
  centr.eigen
})

daily.centrality <- setDT(as.data.frame(do.call(rbind, daily.centrality)))

cleaned.data[, centr.eigen.W1 :=
               daily.centrality[day %in% c(1:7),
               .(centr.eigen = mean(centr.eigen)), by = id][, centr.eigen]]
cleaned.data[, centr.eigen.W2 := daily.centrality[day %in% c(8:20),
               .(centr.eigen = mean(centr.eigen)), by = id][, centr.eigen]]
cleaned.data[, centr.eigen.W3 := daily.centrality[day %in% c(21:27),
               .(centr.eigen = mean(centr.eigen)), by = id][, centr.eigen]]


## add a series of correlates to data.frame
  add.demographics(cleaned.data)

  ## a certainty of candidate preference (in terms of intended vote choice)
  ## 1 = very weak, 7 = very strong
  cleaned.data[, pref.certainty.W2 := car::recode(dat$pv191, "8 = NA")]
  cleaned.data[, pref.certainty.W2 := car::recode(dat$kv4, "8 = NA")]
  cleaned.data[, pref.certainty.W3 := dat$hv17]

  ## ideoloy and strength
  ## ideo: 1 = extremely liberal, 7 = extremely conservative
  ## ideo_str: 0 = weak, 3 = extremely strong
  cleaned.data[, ideo.W1 := dat$pv258]
  cleaned.data[, ideo.W2 := dat$kv49]
  cleaned.data[, ideo.W3 := dat$hv104]
  cleaned.data[, ideo_str.W1 := abs(dat$pv258 - 4)]
  cleaned.data[, ideo_str.W2 := abs(dat$kv49 - 4)]
  cleaned.data[, ideo_str.W3 := abs(dat$hv104 - 4)]

  ## perceived opinion climate based on candidate preference
  ## defined as perceived prevalence of their in-party supporters vis-a-vis out-party supporters
  ## (excluding third-party candidates)
  ## (+) values means more perceived prevalence of in-party supporters
  cleaned.data[canpref.W2 == 1, perceived.opinion.climate.W2 := dat[canpref2 == 1, kv65 - kv64]]
  cleaned.data[canpref.W2 == 0, perceived.opinion.climate.W2 := dat[canpref2 == 0, kv64 - kv65]]
  cleaned.data[canpref.W3 == 1, perceived.opinion.climate.W3 := dat[canpref3 == 1, hv120 - hv119]]
  cleaned.data[canpref.W3 == 0, perceived.opinion.climate.W3 := dat[canpref3 == 0, hv119 - hv120]]

  ## discussion motivations (invariant across waves)
    ## consistency motivation
  cleaned.data[, consistency.motivation := rowMeans(
      dat[, .SD, .SDcols = c("pv18", "pv19", "pv20", "pv21", "pv23", "pv24")] )]
    ## understanding motivation
  cleaned.data[, understanding.motivation := rowMeans(
      dat[, .SD, .SDcols = c("pv13", "pv14", "pv15", "pv16")] )]
    ## hedonic motivation
  cleaned.data[, hedonic.motivation := rowMeans(
      dat[, .SD, .SDcols = c("pv27", "pv28", "pv29")] )]

  ## internal political efficacy (based on a 7-point scale)
  ## higher value means higher political efficacy
  cleaned.data[, internal.efficacy.W1 := 8 - rowMeans(dat[, .SD, .SDcols = c("pv163", "pv164")])]
  cleaned.data[, internal.efficacy.W2 := 8 - rowMeans(dat[, .SD, .SDcols = c("kv177", "kv178")])]
  dat[, .(hv232, hv233, hv235, hv237)][, psych::alpha(as.data.frame(.SD), check.keys = T)]
  cleaned.data[, internal.efficacy.W3 := 8 - rowMeans(dat[, .SD, .SDcols = c("hv232","hv233","hv235","hv237")])]

  ## political interest
  cleaned.data[, pol.interest.W1 := rowMeans(dat[, .SD, .SDcols = c("pv165", "pv166")] )]
  cleaned.data[, pol.interest.W2 := rowMeans(dat[, .SD, .SDcols = c("kv179", "kv180")] )]
  cleaned.data[, pol.interest.W3 := rowMeans(dat[, .SD, .SDcols = c("hv238", "hv239")] )]

  ## media exposure (in hours)
    ## remove "NaN" in data -- W2
    dat[is.na(kv194), kv194 := 0 ]
    dat[is.na(kv196), kv196 := 0 ]
    dat[is.na(kv200), kv200 := 0 ]
    ## add with hours, and creaet index
    cleaned.data[, internet.news.use.W2 := dat[, (60*kv193 + kv194)/60]]
    cleaned.data[, newspaper.use.W2 := dat[, (60*kv195 + kv196)/60]]
    cleaned.data[, tv.news.use.W2 := dat[, (60*kv199 + kv200)/60]]

  ## negativity.bias
  dat[, kv93:kv97][, psych::alpha(as.data.frame(.SD), check.keys = T)]
  cleaned.data[, outgroup.negativity.bias.W2 := rowMeans(
    dat[, .SD, .SDcols = c("kv93", "kv94", "kv95", "kv96", "kv97")])]
  ## in-group favoritism
  dat[, kv160:kv164][, psych::alpha(as.data.frame(.SD), check.keys = T)]
  cleaned.data[, ingroup.favoritism.bias.W2 := rowMeans(
    dat[, .SD, .SDcols = c("kv160", "kv161", "kv162", "kv163", "kv164")])]
  ## affective polarization (ingroup - outgroup)
  cleaned.data[, affective.polarization := ingroup.favoritism.bias.W2 - outgroup.negativity.bias.W2]

## check the over-time change of key measurements
  long.data <- melt(cleaned.data, id.vars = c("id", "age.years", "female", "edu",
                                             "household.income", "residential.region",
                                             "consistency.motivation", "understanding.motivation",
                                             "hedonic.motivation", "internet.news.use.W2",
                                             "newspaper.use.W2", "tv.news.use.W2",
                                             "outgroup.negativity.bias.W2", "ingroup.favoritism.bias.W2"),
                   measure = patterns("safe.disc.W[1-3]$",
                                      "dangerous.disc.W[1-3]$",
                                      "exp.disagr.offline.prcpt.W[1-3]$",
                                      "diff.exp.disagree.W[2-3]$",
                                      "canpref.W[1-3]$",
                                      "pref.certainty.W[1-3]$",
                                      "ideo.W[1-3]$",
                                      "perceived.opinion.climate.W[2-3]$",
                                      "internal.efficacy.W[1-3]$",
                                      "pol.interest.W[1-3]$"),
                   value.name = c("safe.disc", "dangerous.disc", "exp.disagr.offline.prcpt",
                                  "diff.exp.disagree", "canpref", "pref.certainty",
                                  "ideo", "perceived.opinion.climate", "internal.efficacy", "pol.interest"),
                   variable.name = "Wave", variable.factor = T)

  ## manually correct some misassignment of variable waves
  long.data[Wave == 3, diff.exp.disagree := long.data[Wave == 2, diff.exp.disagree]]
  long.data[Wave == 2, diff.exp.disagree := long.data[Wave == 1, diff.exp.disagree]]
  long.data[Wave == 1, diff.exp.disagree := NA]
  long.data[Wave == 3, perceived.opinion.climate := long.data[Wave == 2, perceived.opinion.climate]]
  long.data[Wave == 2, perceived.opinion.climate := long.data[Wave == 1, perceived.opinion.climate]]
  long.data[Wave == 1, perceived.opinion.climate := NA]

  ## self-reported exposure to disagreement measure (exp.disagr.offline.prcpt)
  ggplot(long.data, aes(x = Wave, y = exp.disagr.offline.prcpt, group = factor(id))) +
    stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 17, size = 3) +
    stat_smooth(aes(group = 1), method = "lm", color = "red", se = T) +
    ggtitle("Dangerous discussion (proportion)")

  ## safe and dangerous discussion
  p1 <- ggplot(long.data, aes(x = Wave, y = safe.disc, group = factor(id))) +
    stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 17, size = 3) +
    stat_smooth(aes(group = 1), method = "lm", color = "red", se = T) +
    ggtitle("Safe discussion (proportion)")

  p2 <- ggplot(long.data, aes(x = Wave, y = dangerous.disc, group = factor(id))) +
    stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 17, size = 3) +
    stat_smooth(aes(group = 1), method = "lm", color = "red", se = T) +
    ggtitle("Dangerous discussion (proportion)")

  p1 + p2 + plot_layout(nrow = 1)

  ## perceived opinion climate
  ggplot(long.data[Wave %in% c(1,2),], aes(x = Wave, y = perceived.opinion.climate, group = factor(id))) +
    stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 17, size = 3) +
    stat_smooth(aes(group = 1), method = "lm", color = "red", se = T) +
    ggtitle("Perceived opinion climate") + labs(caption = "(higher value means more in-group prevalence)") +
    scale_x_discrete(labels=c("1" = "Wave 2", "2" = "Wave 3"))






