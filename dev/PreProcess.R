
## install required packages and dependencies automatically
list.of.packages <- c("car", "psych","texreg","rstudioapi","data.table", "devtools",
                      "haven", "magrittr", "igraph", "intergraph")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) devtools::install(new.packages, dependencies = T)
options(scipen = 999)

## automatically setting working directories
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")

## load custom function
source("dev/helper_functions.R")

## load required packages
require(haven)
require(data.table)
require(texreg)
require(parallel)
require(magrittr) ## pipe (%>%) operator
require(igraph)

## load data files locally (later to be deposited publicly)
paths_to_file <- "/Users/songh95/Dropbox/(19) 2018 Summer/CR_2018/"
dat <- haven::read_sav(paste0(paths_to_file, "Dat/DiscussionForumThreeWavePanel(N=341).sav"))
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
net <- read.csv(paste0(paths_to_file, "Dat/Reading_1113-1126_Participants(N=341)_Count(N=160836).csv"))
net2 <- read.csv(paste0(paths_to_file, "Dat/Reading_1127-1219_Participants(N=341)_Count(N=160836).csv"))
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



## add a series of correlates to data.frame
  add.demographics(cleaned.data)
  ## a certainty of candidate preference (in terms of intended vote choice)
  ## 1 = very weak, 7 = very strong
  cleaned.data[, pref.certainty.W2 := car::recode(dat$kv4, "8 = NA")]
  cleaned.data[, pref.certainty.W3 := car::recode(dat$pv191, "8 = NA")]
  ## ideoloy and strength
  cleaned.data[, ideo.W2 := dat$kv49] ## 1 = extremely liberal, 7 = extremely conservative
  cleaned.data[, ideo_str.W2 := abs(dat$kv49 - 4)] ## 0 = weak, 3 = extremely strong
  ## perceived distribution of majority vs. minority (based on candidate preference)
  ## perceived prevalence of their in-party supporters vis-a-vis out-party supporters
  ## (+) values means more perceived prevalence of in-party supporters
  cleaned.data[canpref.W2 == 1, perceived.opinion.climate.W2 := dat[canpref2 == 1, kv65 - kv64]]
  cleaned.data[canpref.W2 == 0, perceived.opinion.climate.W2 := dat[canpref2 == 0, kv64 - kv65]]
  ## consistency motivation
  cleaned.data[, consistency.motivation := rowMeans(
      dat[, .SD, .SDcols = c("pv18", "pv19", "pv20", "pv21", "pv23", "pv24")]
    )]
  ## understanding motivation
  cleaned.data[, understanding.motivation := rowMeans(
      dat[, .SD, .SDcols = c("pv13", "pv14", "pv15", "pv16")]
    )]
  ## hedonic motivation
  cleaned.data[, hedonic.motivation := rowMeans(
      dat[, .SD, .SDcols = c("pv27", "pv28", "pv29")]
    )]
  ## internal political efficacy
  cleaned.data[, internal.efficacy := rowMeans(
      dat[, .SD, .SDcols = c("kv177", "kv178")]
    )]
  ## political interest
  cleaned.data[, pol.interest := rowMeans(
    dat[, .SD, .SDcols = c("kv179", "kv180")]
  )]
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
  cleaned.data[, negativity.bias.W2 := rowMeans(
          dat[, .SD, .SDcols = c("kv93", "kv94", "kv95", "kv96", "kv97")]
        )]
  ## in-group favoritism
  dat[, kv160:kv164][, psych::alpha(as.data.frame(.SD), check.keys = T)]
  cleaned.data[, ingroup.favoritism.W2 := rowMeans(
    dat[, .SD, .SDcols = c("kv160", "kv161", "kv162", "kv163", "kv164")]
  )]

















# Pearson's product-moment correlation
#
# data:  dangerous.disc and exp.disagr.offline.prcpt
# t = 0.61368, df = 339, p-value = 0.5398
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.07315257  0.13902625
# sample estimates:
#        cor
# 0.03331218

datW3[, cor.test(dangerous.disc, exp.disagr.online.prcpt)]
# Pearson's product-moment correlation
#
# data:  dangerous.disc and exp.disagr.online.prcpt
# t = 5.73, df = 339, p-value = 0.00000002219
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.1971715 0.3910202
# sample estimates:
#       cor
# 0.2971547

## create differences between perception and behavioral measures
## (+) values indicate the overestimation, and (-) means underestimation
datW3[, diff.exp.disagree := exp.disagr.online.prcpt - dangerous.disc]
datW3[, summary(diff.exp.disagree)]

