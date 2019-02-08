
## clear workspace before process the data
options(scipen = 999)
rm(list = ls())

## install required packages and dependencies automatically
list.of.packages <- c("car", "psych","texreg","rstudioapi","data.table",
                      "devtools", "formula.tools", "haven", "magrittr",
                      "igraph", "intergraph", "ggplot2", "jtools", "plotROC")
new.packages <- list.of.packages[!(list.of.packages %in%
                                     installed.packages()[,"Package"])]
if(length(new.packages)) devtools::install_cran(new.packages, dependencies = T)
if(!("patchwork" %in% installed.packages()[,"Package"]))
  devtools::install_github("thomasp85/patchwork")

## automatically setting working directories
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# setwd("..")

## load custom function
source("dev/helper_functions_new.R")

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
net2 <- data.frame(reading.time = net2$Reading.Time,
                   reader.id = net2$Reader.Id,
                   poster.id = net2$Poster.Id)
net <- rbind(net, net2)
setDT(net)
net <- unique(net[order(reader.id, reading.time), ])
net[, reading.date := as.Date(reading.time, format = "%Y-%m-%d %H:%M:%S")]

## study period (27 days)
net[, length(unique(reading.date))]
date.range <- unique(sort(net[, reading.date]))

## determine exposure to disagreement
# ## in wave 1, there are few Rs who did not indicated their initial pref.
# ## we need to impute their preference using feeling thermo ratings
# dat[is.na(canpref1), pv254 - pv255] ## all zero
# ## or stated "would-be candidate" they might vote for Qs
# dat[, canpref1.imputed := canpref1]
# dat[is.na(canpref1), canpref1.imputed := car::recode(kv2, "1 = 0; 2 = 1; else = 2")]
# dat[, table(canpref1, canpref1.imputed, exclude = NULL)]

## we use W2 self-report of candidate support for reference
dat[, canpref2] %>% table(.)

## ---------------------------------------------------------------------- ##
## calculate the amount of exposure to diagreement -- cumulative exposure ##
## ---------------------------------------------------------------------- ##

## time frame referenced in Qs are two weeks prior to survey,
## therefore wave 1 exposure (vs. W2 self report) = 11/27 to 12/10
## wave 2 exposure (vs. W3 self report) = 12/7 to 12/20

# reader candidate support == poster candidate support?
cleaned.data <- net[, .(reading.date,
                id = reader.id,
                count = 1,
                exp.disagree = !(
                                 dat[net[, reader.id], canpref2] ==
                                   dat[net[, poster.id], canpref2])
                )][reading.date %in% date.range[5:18],
                    .(dangerous.disc.W1 = mean(exp.disagree, na.rm = T),
                      total.exp.W1 = sum(count)), by = id] %>% setkey(., "id")

cleaned.data <- merge(cleaned.data,
                      net[, .(reading.date,
                              id = reader.id,
                              count = 1,
                              exp.disagree = !(
                                dat[net[, reader.id], canpref3] ==
                                  dat[net[, poster.id], canpref3])
                      )][reading.date %in% date.range[15:27],
                         .(dangerous.disc.W2 = mean(exp.disagree, na.rm = T),
                           total.exp.W2 = sum(count)), by = id],
                      by = "id", all = TRUE)

## ------------------------------------------------------------------------ ##
## calculate the amount of exposure to diagreement -- mean of daily average ##
## ------------------------------------------------------------------------ ##

cleaned.data <- merge(cleaned.data,
                      net[, .(reading.date, id = reader.id,
                              exp.disagree = !(
                                dat[net[, reader.id], canpref2] ==
                                  dat[net[, poster.id], canpref2])
                      )] %>%
                        .[reading.date %in% date.range[5:18],
                          .(dangerous.disc.dlyavg.W1 =
                              mean(exp.disagree, na.rm = T)),
                          by = c("id", "reading.date")] %>%
                        .[, .(dangerous.disc.dlyavg.W1 =
                        ## daily average means we need to divide by no. of days
                        ## but not by just simple mean....
                                sum(dangerous.disc.dlyavg.W1)/14), by = id],
                      by = "id", all = TRUE)


cleaned.data <- merge(cleaned.data,
                      net[, .(reading.date, id = reader.id,
                              exp.disagree = !(
                                dat[net[, reader.id], canpref3] ==
                                  dat[net[, poster.id], canpref3])
                      )] %>%
                        .[reading.date %in% date.range[15:27],
                          .(dangerous.disc.dlyavg.W2 =
                              mean(exp.disagree, na.rm = T)),
                          by = c("id", "reading.date")] %>%
                        .[, .(dangerous.disc.dlyavg.W2 =
                                sum(dangerous.disc.dlyavg.W2)/13), by = id],
                      by = "id", all = TRUE)


## ----------------------------------------------- ##
## self-reported amount of exposure to diagreement ##
## ----------------------------------------------- ##

## Perceptoin measure, W2
dat[, W2.disagree.for.liberal :=
      rowSums(.SD), .SDcols = c("kv61", "kv62")]
dat[, W2.disagree.for.conservative :=
      rowSums(.SD), .SDcols = c("kv60", "kv61")]
dat[canpref2 == 1,
    W2.disagree.online.perception := W2.disagree.for.liberal]
dat[canpref2 == 0,
    W2.disagree.online.perception := W2.disagree.for.conservative]

cleaned.data <- cleaned.data %>%
  cbind(., dangerous.disc.prcptn.W2 = dat$W2.disagree.online.perception)

## Perceptoin measure, W3
dat[, W3.disagree.for.liberal :=
      rowSums(.SD), .SDcols = c("hv116", "hv117")]
dat[, W3.disagree.for.conservative :=
      rowSums(.SD), .SDcols = c("hv115", "hv116")]
dat[canpref3 == 1,
    W3.disagree.online.perception := W3.disagree.for.liberal]
dat[canpref3 == 0,
    W3.disagree.online.perception := W3.disagree.for.conservative]

cleaned.data <- cleaned.data %>%
  cbind(., dangerous.disc.prcptn.W3 = dat$W3.disagree.online.perception)


## get ego's network size
cleaned.data <- merge(cleaned.data,
                      net[reading.date %in% date.range[5:18],
                          .(log.netsize.W1 = log(length(unique(poster.id)))
                          ), by = reader.id] %>%
                        .[, .(id = reader.id, log.netsize.W1)],
                      by = "id")

cleaned.data <- merge(cleaned.data,
                      net[reading.date %in% date.range[15:27],
                          .(log.netsize.W2 = log(length(unique(poster.id)))
                          ), by = reader.id] %>%
                        .[, .(id = reader.id, log.netsize.W2)],
                      by = "id", all = TRUE)

## there are four cases that did not read the post at W2 exposure period..
cleaned.data <- as.data.frame(cleaned.data)
cleaned.data[is.na(cleaned.data)] <- 0
setDT(cleaned.data)

## add a series of correlates to data.frame
  add.demographics(cleaned.data, dat)

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
  ## defined as perceived prevalence of
  ## their in-party supporters vis-a-vis out-party supporters
  ## (excluding third-party candidates)
  ## (+) values means more perceived prevalence of in-party supporters
  cleaned.data[canpref.W2 == 1,
               perceived.opinion.climate.W2 :=
                 dat[canpref2 == 1, kv65 - kv64]]
  cleaned.data[canpref.W2 == 0,
               perceived.opinion.climate.W2 :=
                 dat[canpref2 == 0, kv64 - kv65]]
  cleaned.data[canpref.W3 == 1,
               perceived.opinion.climate.W3 :=
                 dat[canpref3 == 1, hv120 - hv119]]
  cleaned.data[canpref.W3 == 0,
               perceived.opinion.climate.W3 :=
                 dat[canpref3 == 0, hv119 - hv120]]
  cleaned.data[, perceived.opinion.climate.W2 :=
                 scales::rescale(perceived.opinion.climate.W2,
                                  to = c(0, 100), from = c(-100, 100))]
  cleaned.data[, perceived.opinion.climate.W3 :=
                 scales::rescale(perceived.opinion.climate.W3,
                                   to = c(0, 100), from = c(-100, 100))]

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
  cleaned.data[, internal.efficacy.W1 :=
                 8 - rowMeans(dat[, .SD, .SDcols = c("pv163", "pv164")])]
  cleaned.data[, internal.efficacy.W2 :=
                 8 - rowMeans(dat[, .SD, .SDcols = c("kv177", "kv178")])]
  dat[, .(hv232, hv233, hv235, hv237)] %>%
  .[, psych::alpha(as.data.frame(.SD), check.keys = T)]
  cleaned.data[, internal.efficacy.W3 :=
                 8 - rowMeans(dat[, .SD,
                .SDcols = c("hv232","hv233","hv235","hv237")])]

  ## political interest
  cleaned.data[, pol.interest.W1 :=
                 rowMeans(dat[, .SD, .SDcols = c("pv165", "pv166")] )]
  cleaned.data[, pol.interest.W2 :=
                 rowMeans(dat[, .SD, .SDcols = c("kv179", "kv180")] )]
  cleaned.data[, pol.interest.W3 :=
                 rowMeans(dat[, .SD, .SDcols = c("hv238", "hv239")] )]

  ## media exposure (in hours)
    ## remove "NaN" in data -- W2
    dat[is.na(kv194), kv194 := 0 ]
    dat[is.na(kv196), kv196 := 0 ]
    dat[is.na(kv200), kv200 := 0 ]
    ## add with hours, and creaet index
    cleaned.data[, internet.news.use.W2 := dat[, (60*kv193 + kv194)/60]]
    cleaned.data[, newspaper.use.W2 := dat[, (60*kv195 + kv196)/60]]
    cleaned.data[, tv.news.use.W2 := dat[, (60*kv199 + kv200)/60]]

    cleaned.data[, .(internet.news.use.W2, newspaper.use.W2, tv.news.use.W2)][,
                    psych::alpha(as.data.frame(.SD), check.keys = T),
                    .SDcols = c("internet.news.use.W2",
                                "newspaper.use.W2",
                                "tv.news.use.W2")]
    ## alpha is 0.73, therefore create a summary scale
    cleaned.data[, media.exposure.W2 := rowMeans(as.data.frame(.SD)),
                   .SDcols = c("internet.news.use.W2", "newspaper.use.W2",
                             "tv.news.use.W2")]

  ## negativity.bias
  dat[, kv93:kv97][, psych::alpha(as.data.frame(.SD), check.keys = T)]
  cleaned.data[, outgroup.negativity.bias.W2 := rowMeans(
    dat[, .SD, .SDcols = c("kv93", "kv94", "kv95", "kv96", "kv97")])]
  dat[, hv148:hv152][, psych::alpha(as.data.frame(.SD), check.keys = T)]
  cleaned.data[, outgroup.negativity.bias.W3 := rowMeans(
    dat[, .SD, .SDcols = c("hv148", "hv149", "hv150", "hv151", "hv152")])]

  ## in-group favoritism
  dat[, kv160:kv164][, psych::alpha(as.data.frame(.SD), check.keys = T)]
  cleaned.data[, ingroup.favoritism.bias.W2 := rowMeans(
    dat[, .SD, .SDcols = c("kv160", "kv161", "kv162", "kv163", "kv164")])]
  dat[, hv215:hv219][, psych::alpha(as.data.frame(.SD), check.keys = T)]
  cleaned.data[, ingroup.favoritism.bias.W3 := rowMeans(
    dat[, .SD, .SDcols = c("hv215", "hv216", "hv217", "hv218", "hv219")])]

  ## affective polarization (ingroup - outgroup)
  dat[, psych::alpha(as.data.frame(.SD), check.keys = T),
      .SDcols = c("kv93", "kv94", "kv95", "kv96", "kv97",
                  "kv160", "kv161", "kv162", "kv163", "kv164")]
  cleaned.data[, affective.polarization.W2 := rowMeans(
    dat[, .SD, .SDcols = c("kv93", "kv94", "kv95", "kv96", "kv97",
                           "kv160", "kv161", "kv162", "kv163", "kv164")])]
  dat[, psych::alpha(as.data.frame(.SD), check.keys = T),
        .SDcols = c("hv148", "hv149", "hv150", "hv151", "hv152",
                    "hv215", "hv216", "hv217", "hv218", "hv219")]
  cleaned.data[, affective.polarization.W3 := rowMeans(
    dat[, .SD, .SDcols = c("hv148", "hv149", "hv150", "hv151", "hv152",
                           "hv215", "hv216", "hv217", "hv218", "hv219")])]

  ## endorsement of discussion norm, W2
  dat[, kv118:kv122][, psych::alpha(as.data.frame(.SD), check.keys = T)]
  cleaned.data[, discussion.norm.W2 := rowMeans(
    dat[, .SD, .SDcols = c("kv118", "kv119", "kv120", "kv121", "kv122")])]

  ## endorsement of discussion norm, W3
  dat[, hv173:hv176][, psych::alpha(as.data.frame(.SD), check.keys = T)]
  cleaned.data[, discussion.norm.W3 := rowMeans(
    dat[, .SD, .SDcols = c("hv173", "hv174", "hv175", "hv176", "hv177")])]



## more recent exposure? using three most recent days

  ## in deriving this measure, we should account the fact that
  ## not all Rs have accessed the forum every day.
  ## instead of using fixed time window, we should look at the
  ## three most recent days based on "nonzero" count

  cleaned.data <- merge(cleaned.data,
                        ## same candidate support?
                        net[, .(reading.date, id = reader.id, count = 1,
                                exp.disagree = !(
                                  dat[net[, reader.id], canpref2] ==
                                    dat[net[, poster.id], canpref2])
                        )] %>%
                        ## filter by date range and derive mean
                        .[reading.date %in% date.range[16:18],
                          .(recent.dangerous.disc.W1 =
                              mean(exp.disagree, na.rm = T),
                            recent.total.exp.W1 =
                              sum(count)), by = id],
                        ## merge by id
                        by = "id", all = TRUE)

  cleaned.data <- merge(cleaned.data,
                        ## same candidate support?
                        net[, .(reading.date, id = reader.id, count = 1,
                                exp.disagree = !(
                                  dat[net[, reader.id], canpref3] ==
                                    dat[net[, poster.id], canpref3])
                        )] %>%
                        ## filter by date range and derive mean
                        .[reading.date %in% date.range[25:27],
                           .(recent.dangerous.disc.W2 =
                               mean(exp.disagree, na.rm = T),
                             recent.total.exp.W2 =
                               sum(count)), by = id],
                        ## merge by id
                        by = "id", all = TRUE)

  cleaned.data[is.na(recent.dangerous.disc.W1),
               recent.dangerous.disc.W1 := 0]

  cleaned.data[is.na(recent.total.exp.W1),
               recent.total.exp.W1 := 0]

  cleaned.data[is.na(recent.dangerous.disc.W2),
               recent.dangerous.disc.W2 := 0]

  cleaned.data[is.na(recent.total.exp.W2),
               recent.total.exp.W2 := 0]


  ## calculate duration of exposure??
  ## THIS IS NOT REALLY RELIABLE MEASURE...
  # net[, reading.date.time :=
  # strptime(reading.time, format = "%Y-%m-%d %H:%M:%S", tz = "")]
  # net <- net[order(reader.id, reading.date.time), ]
  # net[, prior.exp.time :=
  # shift(reading.date.time, n = 1), by = c("reader.id", "reading.date")]
  # net[, duration :=
  # difftime(reading.date.time, prior.exp.time, units = "mins")]

