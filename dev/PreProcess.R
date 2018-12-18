
list.of.packages <- c("car", "psych","texreg","rstudioapi","data.table",
                      "haven", "magrittr", "igraph", "intergraph")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = T)
options(scipen = 999)

## automatically setting working directories
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")

## custom function
cal.exposure.disagree <- function(net, canpref, prop = TRUE) {

  out <- t(sapply(1:341, function(x) {
    tempvec <- net[x, ]
    exp.same <- canpref[x] == canpref
    safe.disc <- sum(tempvec[exp.same])
    dangerous.disc <- sum(tempvec[!exp.same])

    if (isTRUE(prop)) {
      safe.disc <- safe.disc/sum(tempvec)
      dangerous.disc <- dangerous.disc/sum(tempvec)
      if (is.nan(safe.disc)) safe.disc <- 0  ## no exposure
      if (is.nan(dangerous.disc)) dangerous.disc <- 0 ## no exposure
    }

    ## output: two-column matrix of safe.disc & dangerous.disc
    return(c(safe.disc = safe.disc, dangerous.disc = dangerous.disc))
  }))

  rownames(out) <- 1:341
  return(out)
}


## required packages
require(haven)
require(data.table)
require(texreg)
require(parallel)
require(magrittr) ## pipe (%>%) operator
require(igraph)

## load data
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
datW1 <- exp.dis.daily[day <= 7, .(safe.disc = mean(safe.disc),
                                   dangerous.disc = mean(dangerous.disc)), by = id] %>%
  cbind(., exp.disagr.offline.prcpt = dat$pv323)

datW1[, cor.test(dangerous.disc, exp.disagr.offline.prcpt)]

# Pearson's product-moment correlation
#
# data:  dangerous.disc and exp.disagr.offline.prcpt
# t = -0.88613, df = 339, p-value = 0.3762
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.15349468  0.05843185
# sample estimates:
#         cor
# -0.04807242

## during W2
dat[, W2.disagree.for.liberal := rowMeans(.SD), .SDcols = c("kv61", "kv62")]
dat[, W2.disagree.for.conservative := rowMeans(.SD), .SDcols = c("kv60", "kv61")]

dat[canpref2 == 1, W2.disagree.online.perception := W2.disagree.for.liberal]
dat[canpref2 == 0, W2.disagree.online.perception := W2.disagree.for.conservative]

datW2 <- exp.dis.daily[(day <= 21 & day > 7), .(safe.disc = mean(safe.disc),
                          dangerous.disc = mean(dangerous.disc)), by = id] %>%
         cbind(., exp.disagr.offline.prcpt = dat$kv218) %>%
         cbind(., exp.disagr.online.prcpt = dat$W2.disagree.online.perception)

datW2[, cor.test(dangerous.disc, exp.disagr.offline.prcpt)]
# Pearson's product-moment correlation
#
# data:  dangerous.disc and exp.disagr.offline.prcpt
# t = -0.88434, df = 339, p-value = 0.3771
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.15339953  0.05852896
# sample estimates:
#        cor
# -0.0479752

datW2[, cor.test(dangerous.disc, exp.disagr.online.prcpt)]
# Pearson's product-moment correlation
#
# data:  dangerous.disc and exp.disagr.online.prcpt
# t = 5.4778, df = 339, p-value = 0.00000008409
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.1845424 0.3798610
# sample estimates:
#       cor
# 0.2851594

## during W3
dat[, W3.disagree.for.liberal := rowMeans(.SD), .SDcols = c("hv116", "hv117")]
dat[, W3.disagree.for.conservative := rowMeans(.SD), .SDcols = c("hv115", "hv116")]

dat[canpref3 == 1, W3.disagree.online.perception := W3.disagree.for.liberal]
dat[canpref3 == 0, W3.disagree.online.perception := W3.disagree.for.conservative]

datW3 <- exp.dis.daily[day > 21, .(safe.disc = mean(safe.disc),
                          dangerous.disc = mean(dangerous.disc)), by = id] %>%
         cbind(., exp.disagr.offline.prcpt = dat$hv277) %>%
         cbind(., exp.disagr.online.prcpt = dat$W3.disagree.online.perception)

datW3[, cor.test(dangerous.disc, exp.disagr.offline.prcpt)]
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
