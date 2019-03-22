
## clear workspace before process the data
options(scipen = 999)
rm(list = ls())

## install required packages and dependencies automatically
list.of.packages <- c("car", "psych","texreg","rstudioapi","data.table",
                      "devtools", "formula.tools", "haven", "magrittr",
                      "igraph", "intergraph", "ggplot2", "jtools", "plotROC",
                      "pscl", "rcompanion")
new.packages <- list.of.packages[!(list.of.packages %in%
                                     installed.packages()[,"Package"])]
if(length(new.packages)) devtools::install_cran(new.packages, dependencies = T)
if(!("patchwork" %in% installed.packages()[,"Package"]))
  devtools::install_github("thomasp85/patchwork")
if(!("interactions" %in% installed.packages()[,"Package"]))
  source("https://install-github.me/jacob-long/interactions")

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
                      total.exp.W1 = sum(count),
                      total.dangerous.disc.W1 = sum(exp.disagree),
                      total.safe.disc.W1 = sum(count) - sum(exp.disagree)),
                   by = id] %>% setkey(., "id")

cleaned.data <- merge(cleaned.data,
                      net[, .(reading.date,
                              id = reader.id,
                              count = 1,
                              exp.disagree = !(
                                dat[net[, reader.id], canpref3] ==
                                  dat[net[, poster.id], canpref3])
                      )][reading.date %in% date.range[15:27],
                         .(dangerous.disc.W2 = mean(exp.disagree, na.rm = T),
                           total.exp.W2 = sum(count),
                           total.dangerous.disc.W2 = sum(exp.disagree),
                           total.safe.disc.W2 = sum(count) - sum(exp.disagree)),
                         by = id], by = "id", all = TRUE)

## ------------------------------------------------------------------------ ##
## calculate the amount of exposure to diagreement -- mean of daily average ##
## ------------------------------------------------------------------------ ##

## ----------- ##
## OLD MEASURE ##
## ----------- ##

## disagreement W1
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

## agreement W1
cleaned.data <- merge(cleaned.data,
                      net[, .(reading.date, id = reader.id,
                              exp.agree = (
                                dat[net[, reader.id], canpref2] ==
                                  dat[net[, poster.id], canpref2])
                      )] %>%
                        .[reading.date %in% date.range[5:18],
                          .(safe.disc.dlyavg.W1 =
                              mean(exp.agree, na.rm = T)),
                          by = c("id", "reading.date")] %>%
                        .[, .(safe.disc.dlyavg.W1 =
                                ## daily average means we need to divide by no. of days
                                ## but not by just simple mean....
                                sum(safe.disc.dlyavg.W1)/14), by = id],
                      by = "id", all = TRUE)

# disagreement W2
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

# agreement W2
cleaned.data <- merge(cleaned.data,
                      net[, .(reading.date, id = reader.id,
                              exp.agree = (
                                dat[net[, reader.id], canpref3] ==
                                  dat[net[, poster.id], canpref3])
                      )] %>%
                        .[reading.date %in% date.range[15:27],
                          .(safe.disc.dlyavg.W2 =
                              mean(exp.agree, na.rm = T)),
                          by = c("id", "reading.date")] %>%
                        .[, .(safe.disc.dlyavg.W2 =
                                sum(safe.disc.dlyavg.W2)/13), by = id],
                      by = "id", all = TRUE)

## Time-of-exposure tally compared to daily, forming prior-exposure-relative score

# cleaned.data <- merge(cleaned.data,
#                       net[, .(reading.date, id = reader.id,
#                               exp.disagree = !(
#                                 dat[net[, reader.id], canpref2] ==
#                                   dat[net[, poster.id], canpref2])
#                       )] %>%
#                         .[reading.date %in% date.range[5:18],
#                           .(daily.mean.exp.disagree =
#                               mean(exp.disagree, na.rm = T)),
#                           by = c("id", "reading.date")] %>%
#                         .[, .(dangerous.disc.online.tally.W1 =
#                                 ## daily average means we need to divide by no. of days
#                                 ## but not by just simple mean....
#                                 online.tally(daily.mean.exp.disagree)), by = id],
#                       by = "id", all = TRUE)
#
# cleaned.data <- merge(cleaned.data,
#                       net[, .(reading.date, id = reader.id,
#                               exp.disagree = !(
#                                 dat[net[, reader.id], canpref2] ==
#                                   dat[net[, poster.id], canpref2])
#                       )] %>%
#                         .[reading.date %in% date.range[15:27],
#                           .(daily.mean.exp.disagree =
#                               mean(exp.disagree, na.rm = T)),
#                           by = c("id", "reading.date")] %>%
#                         .[, .(dangerous.disc.online.tally.W2 =
#                                 ## daily average means we need to divide by no. of days
#                                 ## but not by just simple mean....
#                                 online.tally(daily.mean.exp.disagree)), by = id],
#                       by = "id", all = TRUE)



## ----------------------------------------------- ##
## self-reported amount of exposure to diagreement ##
## ----------------------------------------------- ##

## Perceptoin measure, W2
dat[, W2.disagree.for.liberal :=
      rowSums(.SD), .SDcols = c("kv61", "kv62")]
dat[, W2.agree.for.liberal := kv60]
dat[, W2.disagree.for.conservative :=
      rowSums(.SD), .SDcols = c("kv60", "kv61")]
dat[, W2.agree.for.conservative := kv62]
dat[canpref2 == 1,
    W2.disagree.online.perception := W2.disagree.for.liberal]
dat[canpref2 == 1,
    W2.agree.online.perception := W2.agree.for.liberal]
dat[canpref2 == 0,
    W2.disagree.online.perception := W2.disagree.for.conservative]
dat[canpref2 == 0,
    W2.agree.online.perception := W2.agree.for.conservative]

cleaned.data <- cleaned.data %>%
  cbind(., dangerous.disc.prcptn.W2 = dat$W2.disagree.online.perception/100,
           safe.disc.prcptn.W2 = dat$W2.agree.online.perception/100)

## Perceptoin measure, W3
dat[, W3.disagree.for.liberal :=
      rowSums(.SD), .SDcols = c("hv116", "hv117")]
dat[, W3.agree.for.liberal := hv115]
dat[, W3.disagree.for.conservative :=
      rowSums(.SD), .SDcols = c("hv115", "hv116")]
dat[, W3.agree.for.conservative := hv117]
dat[canpref3 == 1,
    W3.disagree.online.perception := W3.disagree.for.liberal]
dat[canpref3 == 1,
    W3.agree.online.perception := W3.agree.for.liberal]
dat[canpref3 == 0,
    W3.disagree.online.perception := W3.disagree.for.conservative]
dat[canpref3 == 0,
    W3.agree.online.perception := W3.agree.for.conservative]

cleaned.data <- cleaned.data %>%
  cbind(., dangerous.disc.prcptn.W3 = dat$W3.disagree.online.perception/100,
           safe.disc.prcptn.W3 = dat$W3.agree.online.perception/100)


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

  ## awareness and tolerance towards different opinions
  ## alpha is .86, therefore create a summary scale
  dat[, .(hv97, hv98, hv99, hv100, hv101)] %>%
    .[, psych::alpha(as.data.frame(.SD), check.keys = T)]
  cleaned.data[, tolerance.W3 := rowMeans(dat[, .SD,
                 .SDcols = c("hv97", "hv98", "hv99", "hv100", "hv101")])]


  ## ideoloy and strength
  ## ideo: 1 = extremely liberal, 7 = extremely conservative
  ## ideo_str: 0 = weak, 3 = extremely strong
  cleaned.data[, ideo.W1 := dat$pv258]
  cleaned.data[, ideo.W2 := dat$kv49]
  cleaned.data[, ideo.W3 := dat$hv104]
  cleaned.data[, ideo_str.W1 := abs(dat$pv258 - 4)]
  cleaned.data[, ideo_str.W2 := abs(dat$kv49 - 4)]
  cleaned.data[, ideo_str.W3 := abs(dat$hv104 - 4)]


  # perceived opinion climate based on candidate preference
  # defined as perceived prevalence of
  # their in-party supporters vis-a-vis out-party supporters
  # (excluding third-party candidates)
  # (+) values means more perceived prevalence of in-party supporters
  cleaned.data[canpref.W2 == 1, ## liberal canddiate
               alt.perceived.opinion.climate.W2 :=
                 dat[canpref2 == 1, 2*kv65 - 100]] ## liberal minus conservative
  cleaned.data[canpref.W2 == 0,
               alt.perceived.opinion.climate.W2 :=
                 dat[canpref2 == 0, 2*kv64 - 100]]
  cleaned.data[canpref.W3 == 1,
               alt.perceived.opinion.climate.W3 :=
                 dat[canpref3 == 1, 2*hv120 - 100]]
  cleaned.data[canpref.W3 == 0,
               alt.perceived.opinion.climate.W3 :=
                 dat[canpref3 == 0, 2*hv119 - 100]]
  cleaned.data[, alt.perceived.opinion.climate.W2 :=
                 scales::rescale(alt.perceived.opinion.climate.W2,
                                  to = c(0, 100), from = c(-100, 100))]
  cleaned.data[, perceived.opinion.climate.W3 :=
                 scales::rescale(alt.perceived.opinion.climate.W3,
                                   to = c(0, 100), from = c(-100, 100))]


  ## new contstruction:
  ## overall ideology minus oneself, therefore discrepancy, from 0 to 6
  ## THIS DOES NOT WORK WELL!
  # cleaned.data[, perceived.opinion.climate.W2 := dat[, 6 - abs(kv50 - kv49)]]
  # cleaned.data[, perceived.opinion.climate.W3 := dat[, 6 - abs(hv105 - hv104)]]

  cleaned.data[, perceived.opinion.climate.W2 := dat[, kv85]]
  cleaned.data[, perceived.opinion.climate.W3 := dat[, hv140]]




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

  ## self-reported knowledge
  cleaned.data[, know.self.W3 := dat$hv240]

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

  ## political participation, W1 (W1 only)
  dat[, pv170:pv176][, psych::alpha(as.data.frame(.SD), check.keys = T)]
  cleaned.data[, ppart.W1 := rowMeans(
    dat[, .SD, .SDcols = c("pv170", "pv171", "pv172", "pv173",
                           "pv174", "pv175", "pv176")])]

  ## Need for social approval (related to social desirability)
  dat[, kv148:kv150][, psych::alpha(as.data.frame(.SD), check.keys = T)]
  cleaned.data[, need.for.approval.W2 := rowMeans(
    dat[, .SD, .SDcols = c("kv148", "kv149", "kv150")])]
  dat[, hv203:hv205][, psych::alpha(as.data.frame(.SD), check.keys = T)]
  cleaned.data[, need.for.approval.W3 := rowMeans(
    dat[, .SD, .SDcols = c("hv203", "hv204", "hv205")])]


## more recent exposure? using three most recent days

  ## in deriving this measure, we should account the fact that
  ## not all Rs have accessed the forum same day.
  ## So instead of using fixed time window, we should look at the
  ## three most recent days per user based on "nonzero" count

  reader.ids.W1 <- net[reading.date %in% date.range[5:18], reader.id]
  poster.ids.W1 <- net[reading.date %in% date.range[5:18], poster.id]

  cleaned.data <- merge(cleaned.data,
                        ## filter until W2 (18th days)
                        net[reading.date %in% date.range[5:18],
                            .(reading.date, id = reader.id, count = 1,
                        ## same candidate support?
                              exp.disagree = !(
                                dat[, canpref2[reader.ids.W1]] ==
                                  dat[, canpref2[poster.ids.W1]])
                        )] %>%
                        ## sum of exp by id and date
                        .[, .(exp.disagree =
                              sum(exp.disagree, na.rm = T),
                              count =
                              sum(count)), by = c("id", "reading.date")] %>%
                        ## select last three days of nonzero access count
                        .[count != 0, ] %>%
                        .[, tail(.SD, 3), by = id] %>%
                        ## derive mean of disagree and total exposure
                        .[, .(recent.dangerous.disc.W1 =
                              sum(exp.disagree)/sum(count),
                            recent.total.exp.W1 =
                              sum(count)), by = id],
                        ## merge by id
                        by = "id", all = TRUE)

  reader.ids.W2 <- net[reading.date %in% date.range[15:27], reader.id]
  poster.ids.W2 <- net[reading.date %in% date.range[15:27], poster.id]

  cleaned.data <- merge(cleaned.data,
                        net[reading.date %in% date.range[15:27],
                            .(reading.date, id = reader.id, count = 1,
                                ## same candidate support?
                                exp.disagree = !(
                                  dat[, canpref3[reader.ids.W2]] ==
                                    dat[, canpref3[poster.ids.W2]])
                              )] %>%
                          ## sum of exp by id and date
                          ## sum of exp by id and date
                          .[, .(exp.disagree =
                                  sum(exp.disagree, na.rm = T),
                                count =
                                  sum(count)), by = c("id", "reading.date")] %>%
                          ## select last three days of nonzero access count
                          .[count != 0, ] %>%
                          .[, tail(.SD, 3), by = id] %>%
                          ## derive mean of disagree and total exposure
                          .[, .(recent.dangerous.disc.W2 =
                                  sum(exp.disagree)/sum(count),
                                recent.total.exp.W2 =
                                  sum(count)), by = id],
                        ## merge by id
                        by = "id", all = TRUE)

  ## calculate duration of exposure??
  ## THIS IS NOT REALLY RELIABLE MEASURE...
  # net[, reading.date.time :=
  # strptime(reading.time, format = "%Y-%m-%d %H:%M:%S", tz = "")]
  # net <- net[order(reader.id, reading.date.time), ]
  # net[, prior.exp.time :=
  # shift(reading.date.time, n = 1), by = c("reader.id", "reading.date")]
  # net[, duration :=
  # difftime(reading.date.time, prior.exp.time, units = "mins")]

  ## create differences between perception and behavioral measures
  ## (+) values indicate the overestimation, and (-) means underestimation
  cleaned.data[, dis.accuracy.W2 :=
                 dangerous.disc.prcptn.W2 - dangerous.disc.W1]
  # cleaned.data[, summary(diff.exp.disagree.W2)]
  cleaned.data[, dis.accuracy.W3 :=
                 dangerous.disc.prcptn.W3 - dangerous.disc.W2]
  # cleaned.data[, summary(diff.exp.disagree.W3)]

  ## ------------------------------- ##
  ## some more recoding of variables ##
  ## ------------------------------- ##

  cleaned.data[, log.total.exp.W1 := log(total.exp.W1 + 1)]
  cleaned.data[, log.total.exp.W2 := log(total.exp.W2 + 1)]
  cleaned.data[, log.total.dangerous.disc.W1 := log(total.dangerous.disc.W1 + 1)]
  cleaned.data[, log.total.safe.disc.W1 := log(total.safe.disc.W1 + 1)]

  cleaned.data[, dis.accuracy.cat.W2 := ## ~ 10% = accurate
                 car::recode(dis.accuracy.W2,
                             "lo:0 = 'accurate'; else = 'over'",
                             as.factor = T)]
  cleaned.data[, dis.accuracy.cat.W3 :=
                 car::recode(dis.accuracy.W3,
                             "lo:0 = 'accurate'; else = 'over'",
                             as.factor = T)]

  # ## public opinion perception accuracy
  # ## actual distribution
  # cleaned.data[, diff(table(canpref.W2)/.N)*100]
  # ## accuracy (perceived minus actual)
  # ## (+) = overestimation of in-party supporters
  # ## (-) = underestimation of inparty supporters
  # cleaned.data[, op.accuracy.W2 := (perceived.opinion.climate.W2 - 36.07038)/100]
  #


  # recent accuracy
  cleaned.data[, log.recent.total.exp.W1 := log(recent.total.exp.W1 + 1)]
  cleaned.data[, log.recent.total.exp.W2 := log(recent.total.exp.W2 + 1)]
  cleaned.data[, recent.accuracy.W2 :=
                 dangerous.disc.prcptn.W2 - recent.dangerous.disc.W1]
  cleaned.data[, recent.accuracy.W3 :=
                 dangerous.disc.prcptn.W3 - recent.dangerous.disc.W2]
