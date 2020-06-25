
## clear workspace before process the data
options(scipen = 999)
rm(list = ls())

## install required packages and dependencies automatically
list.of.packages <- c("car", "psych","texreg","rstudioapi","data.table", "BinNor",
                      "devtools", "formula.tools", "haven", "magrittr", "MBESS",
                      "igraph", "intergraph", "ggplot2", "jtools", "plotROC",
                      "pscl", "rcompanion", "ggthemes", "patchwork", "interactions")
new.packages <- list.of.packages[!(list.of.packages %in%
                                     installed.packages()[,"Package"])]
if(length(new.packages)) devtools::install_cran(new.packages, dependencies = T)

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
## by self-reported ideology (kv49), from 1 = very liberal to 7 = very conservative
dat[, .(kv56, kv49)] %>% cor # one's own left-leaning writing
dat[, .(kv57, kv49)] %>% cor
dat[, .(kv58, kv49)] %>% cor # one's own right-leaning writing
dat[, .(kv58 - kv56, kv49)] %>% cor ## overall correlation is .696

dat[, cor(canpref2, kv49)] ## correlation b/w ideology and candidate pref is .534

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
dat[, canpref1.imputed := canpref1]
dat[is.na(canpref1), canpref1.imputed := car::recode(kv2, "1 = 0; 2 = 1; else = 2")]
dat[, table(canpref1, canpref1.imputed, exclude = NULL)]

## ---------------------------------------------------------------------- ##
## calculate the amount of exposure to diagreement -- cumulative exposure ##
## ---------------------------------------------------------------------- ##

## time frame referenced in Qs are two weeks prior to survey,
## wave 1 exposure (vs. W2 self report) = 11/27 to 12/10

## ----------------------------------------------------- ##
## reader candidate support == poster candidate support? ##
## ----------------------------------------------------- ##
## in defining exposure to disagreement, we follow Huckfeldt's approach
## (not the same party = disagreement, same party = agreement)
cleaned.data <- net[, .(reading.date, id = reader.id, count = 1,
                exp.disagree = !(dat[net[, reader.id], canpref1] == dat[net[, poster.id], canpref1]),
                ## if exact match, define as agreement
                exp.agree = (dat[net[, reader.id], canpref1] == dat[net[, poster.id], canpref1])
                )][reading.date %in% date.range[5:18],
                    .(dangerous.disc.W1.candpref = sum(exp.disagree, na.rm = T)/sum(count),
                      safe.disc.W1.candpref = sum(exp.agree, na.rm = T)/sum(count),
                      total.exp.W1 = sum(count),
                      total.dangerous.disc.W1.candpref = sum(exp.disagree, na.rm = T),
                      total.safe.disc.W1.candpref = sum(exp.agree, na.rm = T)),
                   by = id] %>% setkey(., "id")

# ## ----------------------------------------------------- ##
# ## RR1: consider ideology (lib-con), INCLUDING moderates ##
# ## ----------------------------------------------------- ##
# dat[, pv258_re3 := car::recode(pv258, "4 = 0; lo:3 = 1; 5:hi = -1")]
# ## in defining exposure to disagreement, we treat Huckfeldt's approach
# ## (not the same party = disagreement, same party = agreement)
# cleaned.data <- merge(cleaned.data,
#
#   net[, .(reading.date, id = reader.id, count = 1,
#           exp.disagree = as.numeric(!(dat[net[, reader.id], pv258_re3] == dat[net[, poster.id], pv258_re3])),
#           exp.agree = as.numeric(dat[net[, reader.id], pv258_re3] == dat[net[, poster.id], pv258_re3])
#                       )][reading.date %in% date.range[5:18],
#                          .(dangerous.disc.W1.ideo3 = sum(exp.disagree, na.rm = T)/sum(count),
#                            safe.disc.W1.ideo3 = sum(exp.agree, na.rm = T)/sum(count),
#                            total.dangerous.disc.W1.ideo3 = sum(exp.disagree, na.rm = T),
#                            total.safe.disc.W1.ideo3 = sum(exp.agree, na.rm = T)),
#                          by = id], by = "id", all = TRUE)
#
# ## ----------------------------------------------------- ##
# ## RR1: consider ideology (lib-con), EXCLUDING moderates ##
# ## ----------------------------------------------------- ##
# dat[, pv258_re2 := car::recode(pv258, "4 = NA; lo:3 = 1; 5:hi = 0")]
# ## in defining exposure to disagreement, we treat Huckfeldt's approach
# ## (not the same party = disagreement, same party = agreement)
# cleaned.data <- merge(cleaned.data,
#
#    net[, .(reading.date, id = reader.id, count = 1,
#             exp.disagree = as.numeric(!(dat[net[, reader.id], pv258_re2] == dat[net[, poster.id], pv258_re2])),
#             exp.agree = as.numeric(dat[net[, reader.id], pv258_re2] == dat[net[, poster.id], pv258_re2])
#                       )][reading.date %in% date.range[5:18],
#                          .(dangerous.disc.W1.ideo2 = sum(exp.disagree, na.rm = T)/sum(count),
#                            safe.disc.W1.ideo2 = sum(exp.agree, na.rm = T)/sum(count),
#                            total.dangerous.disc.W1.ideo2 = sum(exp.disagree, na.rm = T),
#                            total.safe.disc.W1.ideo2 = sum(exp.agree, na.rm = T)),
#                          by = id], by = "id", all = TRUE)
#
#
# ## ---------------------------------------------------------- ##
# ## RR1: consider more finer differences in ideology (absdiff) ##
# ## ---------------------------------------------------------- ##
#
# cleaned.data <- merge(cleaned.data,
#                  net[, .(reading.date, id = reader.id, count = 6, ## max diff is 6 (e.g., 7 - 1, or 1 - 7)
#                          exp.disagree = abs(dat[net[, reader.id], pv258] - dat[net[, poster.id], pv258]))][
#                            reading.date %in% date.range[5:18],
#                          .(dangerous.disc.W1.ideodiff = sum(exp.disagree, na.rm = T)/sum(count),
#                            total.dangerous.disc.W1.ideodiff = sum(exp.disagree, na.rm = T)),
#                          by = id], by = "id", all = TRUE)

## wave 2 exposure (vs. W3 self report) = 12/7 to 12/20
## ----------------------------------------------------- ##
## reader candidate support == poster candidate support? ##
## ----------------------------------------------------- ##
## in defining exposure to disagreement, we follow Huckfeldt's approach
## (not the same party = disagreement, same party = agreement)
cleaned.data <- merge(cleaned.data,
                      net[, .(reading.date,
                              id = reader.id,
                              count = 1,
                              exp.disagree = !(dat[net[, reader.id], canpref2] == dat[net[, poster.id], canpref2]),
                              exp.agree = (dat[net[, reader.id], canpref2] == dat[net[, poster.id], canpref2])
                      )][reading.date %in% date.range[15:27],
                         .(dangerous.disc.W2.candpref = mean(exp.disagree, na.rm = T),
                           total.exp.W2 = sum(count),
                           total.dangerous.disc.W2.candpref = sum(exp.disagree),
                           total.safe.disc.W2.candpref = sum(count) - sum(exp.disagree)),
                         by = id], by = "id", all = TRUE)


# ## ----------------------------------------------------- ##
# ## RR1: consider ideology (lib-con), INCLUDING moderates ##
# ## ----------------------------------------------------- ##
# dat[, kv49_re3 := car::recode(kv49, "4 = 0; lo:3 = 1; 5:hi = -1")]
# ## in defining exposure to disagreement, we treat Huckfeldt's approach
# ## (not the same party = disagreement, same party = agreement)
# cleaned.data <- merge(cleaned.data,
#
#   net[, .(reading.date, id = reader.id, count = 1,
#           exp.disagree = as.numeric(!(dat[net[, reader.id], kv49_re3] == dat[net[, poster.id], kv49_re3])),
#           exp.agree = as.numeric(dat[net[, reader.id], kv49_re3] == dat[net[, poster.id], kv49_re3])
#                       )][reading.date %in% date.range[15:27],
#                          .(dangerous.disc.W2.ideo3 = sum(exp.disagree, na.rm = T)/sum(count),
#                            safe.disc.W2.ideo3 = sum(exp.agree, na.rm = T)/sum(count),
#                            total.dangerous.disc.W2.ideo3 = sum(exp.disagree, na.rm = T),
#                            total.safe.disc.W2.ideo3 = sum(exp.agree, na.rm = T)),
#                          by = id], by = "id", all = TRUE)
#
# ## ----------------------------------------------------- ##
# ## RR1: consider ideology (lib-con), EXCLUDING moderates ##
# ## ----------------------------------------------------- ##
# dat[, kv49_re2 := car::recode(kv49, "4 = NA; lo:3 = 1; 5:hi = 0")]
# ## in defining exposure to disagreement, we treat Huckfeldt's approach
# ## (not the same party = disagreement, same party = agreement)
# cleaned.data <- merge(cleaned.data,
#
#   net[, .(reading.date, id = reader.id, count = 1,
#           exp.disagree = as.numeric(!(dat[net[, reader.id], kv49_re2] == dat[net[, poster.id], kv49_re2])),
#           exp.agree = as.numeric(dat[net[, reader.id], kv49_re2] == dat[net[, poster.id], kv49_re2])
#                       )][reading.date %in% date.range[15:27],
#                          .(dangerous.disc.W2.ideo2 = sum(exp.disagree, na.rm = T)/sum(count),
#                            safe.disc.W2.ideo2 = sum(exp.agree, na.rm = T)/sum(count),
#                            total.dangerous.disc.W2.ideo2 = sum(exp.disagree, na.rm = T),
#                            total.safe.disc.W2.ideo2 = sum(exp.agree, na.rm = T)),
#                          by = id], by = "id", all = TRUE)
#
# ## ---------------------------------------------------------- ##
# ## RR1: consider more finer differences in ideology (absdiff) ##
# ## ---------------------------------------------------------- ##
#
# cleaned.data <- merge(cleaned.data,
#                       net[, .(reading.date, id = reader.id, count = 6, ## max diff is 6 (e.g., 7 - 1, or 1 - 7)
#                               exp.disagree = abs(dat[net[, reader.id], kv49] - dat[net[, poster.id], kv49]))][
#                                 reading.date %in% date.range[15:27],
#                                 .(dangerous.disc.W2.ideodiff = sum(exp.disagree, na.rm = T)/sum(count),
#                                   total.dangerous.disc.W2.ideodiff = sum(exp.disagree, na.rm = T)),
#                                 by = id], by = "id", all = TRUE)
#


## ---------------------------- ##
## More recent days of exposure ##
## ---------------------------- ##

## more recent exposure? using three most recent days

## in deriving this measure, we should account the fact that
## not all Rs have accessed the forum same day.
## So instead of using fixed time window, we should look at the
## three most recent days per user based on "nonzero" count

dynamic.moving.window <- lapply(1:14, function(date_x) {

  w1 <- net[reading.date %in% date.range[5:18],
         .(reading.date, id = reader.id, count = 1,
           exp.disagree = !(
             dat[net[reading.date %in% date.range[5:18], reader.id], canpref1] ==
               dat[net[reading.date %in% date.range[5:18], poster.id], canpref1]),
           ## if exact match, define as agreement
           exp.agree = (dat[net[reading.date %in% date.range[5:18], reader.id], canpref1] ==
                          dat[net[reading.date %in% date.range[5:18], poster.id], canpref1])
  )] %>%
  ## sum of exp by id and date
  .[, .(exp.disagree =
          sum(exp.disagree, na.rm = T),
        count =
          sum(count)), by = c("id", "reading.date")] %>%
  ## select last three days of nonzero access count
    .[count != 0, ] %>%
    .[, tail(.SD, date_x), by = id] %>%
    ## derive mean of disagree and total exposure
    .[, .(recent.dangerous.disc.W1 =
            sum(exp.disagree, na.rm = T)/sum(count),
          recent.total.exp.W1 = log(sum(count) + 1)
    ), by = id]

  w2 <- net[reading.date %in% date.range[15:27],
            .(reading.date, id = reader.id, count = 1,
              exp.disagree = !(
                dat[net[reading.date %in% date.range[15:27], reader.id], canpref2] ==
                  dat[net[reading.date %in% date.range[15:27], poster.id], canpref2]),
              ## if exact match, define as agreement
              exp.agree = (dat[net[reading.date %in% date.range[15:27], reader.id], canpref2] ==
                             dat[net[reading.date %in% date.range[15:27], poster.id], canpref2])
            )] %>%
    ## sum of exp by id and date
    .[, .(exp.disagree =
            sum(exp.disagree, na.rm = T),
          count =
            sum(count)), by = c("id", "reading.date")] %>%
    ## select last three days of nonzero access count
    .[count != 0, ] %>%
    .[, tail(.SD, date_x), by = id] %>%
    ## derive mean of disagree and total exposure
    .[, .(recent.dangerous.disc.W2 =
            sum(exp.disagree, na.rm = T)/sum(count),
          recent.total.exp.W2 = log(sum(count) + 1)
    ), by = id]

  merge(w1, w2, by = "id")

})

## list covers most recent N days from the date of the survey
names(dynamic.moving.window) <- paste0("Most recent ", 1:14, " days")


## ----------------------------------------------- ##
## self-reported amount of exposure to diagreement ##
## ----------------------------------------------- ##

## Perception measure, W2
## based on candidate preference (canpref2)
## in defining exposure to disagreement, we follow Huckfeldt's approach
## (not the same party = disagreement, same party = agreement)
dat[, W2.disagree.for.moon := rowSums(.SD), .SDcols = c("kv64", "kv66")] ## park & other, excluding non-supporters
dat[, W2.agree.for.moon := kv65]
dat[, W2.disagree.for.park := rowSums(.SD), .SDcols = c("kv65", "kv66")] ## moon & other, excluding non-supporters
dat[, W2.agree.for.park := kv64]

dat[canpref2 == 1, ## moon
    W2.percept.disagree.canpref := W2.disagree.for.moon]
dat[canpref2 == 1,
    W2.percept.agree.canpref := W2.agree.for.moon]
dat[canpref2 == 0, ## park
    W2.percept.disagree.canpref := W2.disagree.for.park]
dat[canpref2 == 0,
    W2.percept.agree.canpref := W2.agree.for.park]

# ## based on ideology (ideo3 = including moderate, ideo2 = excluding moderate)
# ## in defining exposure to disagreement, we follow Huckfeldt's approach
# ## (not the same party = disagreement, same party = agreement)
# ## including moderate (ideo3)
# dat[, W2.disagree.for.liberal := rowSums(.SD), .SDcols = c("kv61", "kv62")]
# dat[, W2.agree.for.liberal := kv60]
# dat[, W2.disagree.for.conservative := rowSums(.SD), .SDcols = c("kv60", "kv61")]
# dat[, W2.agree.for.conservative := kv62]
# dat[, W2.disagree.for.independent := rowSums(.SD), .SDcols = c("kv62", "kv60")]
# dat[, W2.agree.for.independent := kv61]
#
# dat[kv49_re3 == 1, ## lib
#     W2.percept.disagree.ideo3 := W2.disagree.for.liberal]
# dat[kv49_re3 == 1,
#     W2.percept.agree.ideo3 := W2.agree.for.liberal]
# dat[kv49_re3 == -1, ## con
#     W2.percept.disagree.ideo3 := W2.disagree.for.conservative]
# dat[kv49_re3 == -1,
#     W2.percept.agree.ideo3 := W2.agree.for.conservative]
# dat[kv49_re3 == 0, ## ind
#     W2.percept.disagree.ideo3 := W2.disagree.for.independent]
# dat[kv49_re3 == 0,
#     W2.percept.agree.ideo3 := W2.agree.for.independent]
#
# ## excluding moderate (ideo2)
# ## in defining exposure to disagreement, we treat Huckfeldt's approach
# ## (not the same party = disagreement, same party = agreement)
#
# dat[, W2.disagree.for.liberal2 := kv62] ## excluding moderate
# dat[, W2.disagree.for.conservative2 := kv60] ## excluding moderate
#
# dat[kv49_re2 == 1,
#     W2.percept.disagree.ideo2 := W2.disagree.for.liberal2]
# dat[kv49_re2 == 1,
#     W2.percept.agree.ideo2 := W2.agree.for.liberal]
# dat[kv49_re2 == 0,
#     W2.percept.disagree.ideo2 := W2.disagree.for.conservative2]
# dat[kv49_re2 == 0,
#     W2.percept.agree.ideo2 := W2.agree.for.conservative]


## bind to data
cleaned.data <- cleaned.data %>%
  cbind(., dangerous.disc.prcptn.W2.candpref = dat$W2.percept.disagree.canpref/100,
           safe.disc.prcptn.W2.candpref = dat$W2.percept.agree.canpref/100)
           # dangerous.disc.prcptn.W2.ideo3 = dat$W2.percept.disagree.ideo3/100,
           # safe.disc.prcptn.W2.ideo3 = dat$W2.percept.agree.ideo3/100,
           # dangerous.disc.prcptn.W2.ideo2 = dat$W2.percept.disagree.ideo2/100,
           # safe.disc.prcptn.W2.ideo2 = dat$W2.percept.agree.ideo2/100)


## Perception measure, W3
## based on candidate preference (canpref3)
## in defining exposure to disagreement, we treat Huckfeldt's approach
## (not the same party = disagreement, same party = agreement)
dat[, W3.disagree.for.moon := rowSums(.SD), .SDcols = c("hv119", "hv121")] ## park plus other, excluding non-supporters
dat[, W3.agree.for.moon := hv120]
dat[, W3.disagree.for.park := rowSums(.SD), .SDcols = c("hv120", "hv121")] ## moon plus other, excluding non-supporters
dat[, W3.agree.for.park := hv119]

dat[canpref3 == 1, ## moon
    W3.percept.disagree.canpref := W3.disagree.for.moon]
dat[canpref3 == 1,
    W3.percept.agree.canpref := W3.agree.for.moon]
dat[canpref3 == 0, ## park
    W3.percept.disagree.canpref := W3.disagree.for.park]
dat[canpref3 == 0,
    W3.percept.agree.canpref := W3.agree.for.park]

# ## based on ideology (ideo3 = including moderate, ideo2 = excluding moderate)
# ## in defining exposure to disagreement, we treat Huckfeldt's approach
# ## (not the same party = disagreement, same party = agreement)
# ## including moderate (ideo3) hv115, hv116, hv117
# dat[, W3.disagree.for.liberal := rowSums(.SD), .SDcols = c("hv116", "hv117")]
# dat[, W3.agree.for.liberal := hv115]
# dat[, W3.disagree.for.conservative := rowSums(.SD), .SDcols = c("hv115", "hv116")]
# dat[, W3.agree.for.conservative := hv117]
# dat[, W3.disagree.for.independent := rowSums(.SD), .SDcols = c("hv117", "hv115")]
# dat[, W3.agree.for.independent := hv116]
#
# dat[, hv104_re3 := car::recode(hv104, "4 = 0; lo:3 = 1; 5:hi = -1")]
# dat[, hv104_re2 := car::recode(hv104, "4 = NA; lo:3 = 1; 5:hi = 0")]
#
# dat[hv104_re3 == 1, ## lib
#     W3.percept.disagree.ideo3 := W3.disagree.for.liberal]
# dat[hv104_re3 == 1,
#     W3.percept.agree.ideo3 := W3.agree.for.liberal]
# dat[hv104_re3 == -1, ## con
#     W3.percept.disagree.ideo3 := W3.disagree.for.conservative]
# dat[hv104_re3 == -1,
#     W3.percept.agree.ideo3 := W3.agree.for.conservative]
# dat[hv104_re3 == 0, ## ind
#     W3.percept.disagree.ideo3 := W3.disagree.for.independent]
# dat[hv104_re3 == 0,
#     W3.percept.agree.ideo3 := W3.agree.for.independent]

# ## excluding moderate (ideo2)
# ## in defining exposure to disagreement, we treat Huckfeldt's approach
# ## (not the same party = disagreement, same party = agreement)
#
# dat[, W3.disagree.for.liberal2 := hv117] ## excluding moderate
# dat[, W3.disagree.for.conservative2 := hv115] ## excluding moderate

# dat[hv104_re2 == 1,
#     W3.percept.disagree.ideo2 := W3.disagree.for.liberal2]
# dat[hv104_re2 == 1,
#     W3.percept.agree.ideo2 := W3.agree.for.liberal]
# dat[hv104_re2 == 0,
#     W3.percept.disagree.ideo2 := W3.disagree.for.conservative2]
# dat[hv104_re2 == 0,
#     W3.percept.agree.ideo2 := W3.agree.for.conservative]


## bind to data
cleaned.data <- cleaned.data %>%
  cbind(., dangerous.disc.prcptn.W3.candpref = dat$W3.percept.disagree.canpref/100,
        safe.disc.prcptn.W3.candpref = dat$W3.percept.agree.canpref/100)
        # dangerous.disc.prcptn.W3.ideo3 = dat$W3.percept.disagree.ideo3/100,
        # safe.disc.prcptn.W3.ideo3 = dat$W3.percept.agree.ideo3/100,
        # dangerous.disc.prcptn.W3.ideo2 = dat$W3.percept.disagree.ideo2/100,
        # safe.disc.prcptn.W3.ideo2 = dat$W3.percept.agree.ideo2/100)


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


## add a series of correlates to data.frame
  add.demographics(cleaned.data, dat)

  ## a certainty of candidate preference (in terms of intended vote choice)
  ## 1 = very weak, 7 = very strong
  cleaned.data[, pref.certainty.W1 := car::recode(dat$pv191, "8 = NA")]
  cleaned.data[, pref.certainty.W2 := car::recode(dat$kv4, "8 = NA")]
  cleaned.data[, pref.certainty.W3 := dat$hv17]

  cleaned.data[, cand.pref.strength.W1 := abs(dat$pv254 - dat$pv255)]
  cleaned.data[, cand.pref.strength.W2 := abs(dat$kv37 - dat$kv38)]
  cleaned.data[, cand.pref.strength.W3 := abs(dat$hv62 - dat$hv63)]

  cleaned.data[, ambi.W2 := (dat$kv37 + dat$kv38)/2 - abs(dat$kv37 - dat$kv38)]
  cleaned.data[, ambi.W3 := (dat$hv62 + dat$hv63)/2 - abs(dat$hv62 - dat$hv63)]

  ## awareness and tolerance towards different opinions
  ## alpha is .86, therefore create a summary scale
  dat[, .(hv99, hv100, hv101)] %>%
    .[, psych::alpha(as.data.frame(.SD), check.keys = T)]
  cleaned.data[, tolerance.W3 := rowMeans(dat[, .SD,
                 .SDcols = c("hv99", "hv100", "hv101")])]


  ## ideoloy and strength
  ## ideo: 1 = extremely liberal, 7 = extremely conservative
  ## ideo_str: 0 = weak, 3 = extremely strong
  cleaned.data[, ideo.W1 := dat$pv258]
  cleaned.data[, ideo.W2 := dat$kv49]
  cleaned.data[, ideo.W3 := dat$hv104]
  cleaned.data[, ideo_str.W1 := abs(dat$pv258 - 4)]
  cleaned.data[, ideo_str.W2 := abs(dat$kv49 - 4)]
  cleaned.data[, ideo_str.W3 := abs(dat$hv104 - 4)]


  ## Perceived opinion climate (or opinion perception)
  ## "My opinion in the presidential debate forum generally agrees with the majority."
  ## 1 = not at all, to 7 = very much
  cleaned.data[, perceived.opinion.climate.W1 := dat[, pv48]]
  cleaned.data[, perceived.opinion.climate.W2 := dat[, kv85]]
  cleaned.data[, perceived.opinion.climate.W3 := dat[, hv140]]

  ## discussion motivations (invariant across waves)
  ## consistency motivation
  cleaned.data[, consistency.motivation := rowMeans(
      dat[, .SD, .SDcols = c("pv18", "pv19", "pv20", "pv21", "pv23", "pv24")] )]
  ## understanding motivation
  dat[, pv13:pv16][, psych::alpha(as.data.frame(.SD), check.keys = T)]
  cleaned.data[, understanding.motivation := rowMeans(
      dat[, .SD, .SDcols = c("pv13", "pv14", "pv15", "pv16")] )]
  ## hedonic motivation
  cleaned.data[, hedonic.motivation := rowMeans(
      dat[, .SD, .SDcols = c("pv27", "pv28", "pv29")] )]

  ## conflict avoidance
  #dat[, pv42:pv43][, psych::alpha(as.data.frame(.SD), check.keys = T)]
  cleaned.data[, conflict.avoidance := rowMeans(
    dat[, .SD, .SDcols = c("pv42", "pv43")]
  )]


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

  ## social media use at W1, in hours
  dat[, is.na(pv320)]
  dat[is.na(pv321), pv321 := 0]
  cleaned.data[, SNS.use := log(dat[, (60*pv320 + pv321)/60] + 1)]

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

  ## endorsement of discussion norm, W1
  dat[, pv77:pv81][, psych::alpha(as.data.frame(.SD), check.keys = T)]
  cleaned.data[, discussion.norm.W1 := rowMeans(
    dat[, .SD, .SDcols = c("pv77", "pv78", "pv79", "pv80", "pv81")])]

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

  ## Wave 3, tolerance
  dat[, hv99:hv101][, psych::alpha(as.data.frame(.SD), check.keys = T)]
  cleaned.data[, tolerance.W3 := rowMeans(
    dat[, .SD, .SDcols = c("hv99", "hv100", "hv101")])]

  ## Wave 3, political participation
  cleaned.data[, participation.W2 := (7 - dat[, kv184:kv190] %>% is.na %>% rowSums)]
  cleaned.data[, participation.W3 := (7 - dat[, hv243:hv249] %>% is.na %>% rowSums)]


  ## create differences between perception and behavioral measures
  ## (+) values indicate the overestimation, and (-) means underestimation
  cleaned.data[, dis.accuracy.W2.candpref := dangerous.disc.prcptn.W2.candpref - dangerous.disc.W1.candpref]
  cleaned.data[, dis.accuracy.W3.candpref := dangerous.disc.prcptn.W3.candpref - dangerous.disc.W2.candpref]
  # cleaned.data[, dis.accuracy.W2.ideo3 := dangerous.disc.prcptn.W2.ideo3 - dangerous.disc.W1.ideo3]
  # cleaned.data[, dis.accuracy.W3.ideo3 := dangerous.disc.prcptn.W3.ideo3 - dangerous.disc.W2.ideo3]
  # cleaned.data[, dis.accuracy.W2.ideo2 := dangerous.disc.prcptn.W2.ideo2 - dangerous.disc.W1.ideo2]
  # cleaned.data[, dis.accuracy.W3.ideo2 := dangerous.disc.prcptn.W3.ideo2 - dangerous.disc.W2.ideo2]

  ## ------------------------------- ##
  ## some more recoding of variables ##
  ## ------------------------------- ##

  cleaned.data[, log.total.exp.W1 := log(total.exp.W1 + 1)]
  cleaned.data[, log.total.exp.W2 := log(total.exp.W2 + 1)]

  cleaned.data[, dis.accuracy.cat.W2 :=
                 car::recode(dis.accuracy.W2.candpref,
                             "lo:0 = 'accurate'; else = 'over'",
                             as.factor = T)]
  cleaned.data[, dis.accuracy.cat.W3 :=
                 car::recode(dis.accuracy.W3.candpref,
                             "lo:0 = 'accurate'; else = 'over'",
                             as.factor = T)]


  dynamic.moving.window <-
    lapply(dynamic.moving.window, function(x)
      merge(x, cleaned.data[, .(id,
                                ## Wave 2 measure set
                                dangerous.disc.prcptn.W2.candpref, dangerous.disc.W1.candpref,
                                ## Wave 3 measure set
                                dangerous.disc.prcptn.W3.candpref, dangerous.disc.W2.candpref)]))
