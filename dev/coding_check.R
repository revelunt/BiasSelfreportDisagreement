
## load data
require(openxlsx)
require(data.table)
require(magrittr)

coding <- openxlsx::read.xlsx("Dat/post_coding.xlsx", startRow = 3) %>% setDT
colnames(coding) <- c("coder_id", "writer_id", "writer_no_posts", "meta1", "meta2", "meta3", "meta4", "meta5",
                      "post_heading.1", "writer.1", "reading_time.1",
                      "heading_mention_candidate.1", "heading_mention_valence.1",
                      "post_mention_candidate.1", "post_mention_substantive_content.1", "post_mention_valence.1",
                      "post_heading.2", "writer.2", "reading_time.2",
                      "heading_mention_candidate.2", "heading_mention_valence.2",
                      "post_mention_candidate.2", "post_mention_substantive_content.2", "post_mention_valence.2")

## drop meta-information columns
coding <- coding[, -c(2:8)]

## double the row of data since we collaps columns for the second post
coding2 <- rbind(coding, coding)

coding2[1358:2714, `:=`(post_heading.1 = post_heading.2,
                        writer.1 = writer.2,
                        reading_time.1 = reading_time.2,
                        heading_mention_candidate.1 = heading_mention_candidate.2,
                        heading_mention_valence.1 = heading_mention_valence.2,
                        post_mention_candidate.1 = post_mention_candidate.2,
                        post_mention_substantive_content.1 = post_mention_substantive_content.2,
                        post_mention_valence.1 = post_mention_valence.2)]

coding2 <- coding2[, -c(10:17)]
colnames(coding2) <- c("coder_id", "post_heading", "poster_id", "reading_time",
                       "heading_mention_candidate", "heading_mention_valence",
                       "post_mention_candidate", "post_mention_substantive_content", "post_mention_valence")


coding <- coding2; rm(coding2)
coding <- coding[!is.na(post_heading), ]

## total number of unique posts that were coded
coding[, unique(post_heading) %>% length]


## identified duplicated coded data by post heading and poster id

index <- cbind(duplicated(coding, by = c("post_heading", "poster_id"), fromLast = F),
               duplicated(coding, by = c("post_heading", "poster_id"), fromLast = T))
index <- apply(index, 1, sum)
coding_overlap <- coding[index == 1, ]
duplicated.headings <- coding_overlap[, post_heading] %>% sapply(., as.character) %>% unname %>% unique

coding_overlap <- coding[post_heading %in% duplicated.headings, ][order(post_heading, coder_id), ]
coding_overlap <- unique(coding_overlap, by = c("post_heading", "poster_id", "coder_id"))
# View(coding_overlap)

coding_overlap$heading_mention_candidate <- as.numeric(coding_overlap$heading_mention_candidate)
coding_overlap$heading_mention_valence <- as.numeric(coding_overlap$heading_mention_valence)
coding_overlap$post_mention_candidate <- as.numeric(coding_overlap$post_mention_candidate)
coding_overlap$post_mention_substantive_content <- as.numeric(coding_overlap$post_mention_substantive_content)
coding_overlap$post_mention_valence <- as.numeric(coding_overlap$post_mention_valence)
## write.csv(coding_overlap, file = "coding_overlap.csv", fileEncoding = "EUC-KR")
## coding_overlap[, post_heading]
overlap_entries <- coding_overlap[, .N, by = post_heading][N > 1, post_heading]
coding_overlap <- coding_overlap[post_heading %in% overlap_entries, ]

library(tidyr)
require(irr)
## coding is independently done by 4 trained coders
## total unique entries coded: 235
unique(coding_overlap, by = c("post_heading", "poster_id"))

## the amount of entries coded (N = 235) correspond to
## XX% of total posts that ever posted in the forum ?




## heading_mention_candidate: alpha = .668
test <- coding_overlap[, .(coder_id, post_heading, heading_mention_candidate)] %>% na.omit(.)
test <- test[!duplicated(test, by = c("coder_id", "post_heading")), ]
spread(test, key = "post_heading", value = "heading_mention_candidate")[, -1] %>%
  as.matrix(.) %>% kripp.alpha(., method = "nominal")

## heading_mention_valence: alpha = .472
test <- coding_overlap[, .(coder_id, post_heading, heading_mention_valence)] %>% na.omit(.)
test <- test[!duplicated(test, by = c("coder_id", "post_heading")), ]
spread(test, key = "post_heading", value = "heading_mention_valence")[, -1] %>%
  as.matrix(.) %>% kripp.alpha(., method = "nominal")

    ## cf. what if we drop "7" -- no mention of candidates: alpha = .51 (ordinal)
    test[heading_mention_valence == 7, heading_mention_valence := NA]
    spread(test, key = "post_heading", value = "heading_mention_valence")[, -1] %>%
      as.matrix(.) %>% kripp.alpha(., method = "ordinal")


## post_mention_candidate: alpha = .761
test <- coding_overlap[, .(coder_id, post_heading, post_mention_candidate)] %>% na.omit(.)
## 1 = Park, 2 = Moon, 3 = other party candidate or no mention
test[post_mention_candidate %in% c(3,4,5,6,7), post_mention_candidate := 3]

test <- test[!duplicated(test, by = c("coder_id", "post_heading")), ]
spread(test, key = "post_heading", value = "post_mention_candidate")[, -1] %>%
  as.matrix(.) %>% kripp.alpha(., method = "nominal")

test <- spread(test, key = "post_heading", value = "post_mention_candidate")[, -1] %>%
  as.matrix(.) %>% t %>% unname

   lapply(1:6, function(i) {
     cols <- combn(c(1,2,3,4), m = 2)[,i] %>% as.numeric
     stats <- test[, cols] %>% agree
     stats$value
   }) %>% unlist %>% mean



## post_mention_substantive_content: alpha = .644
test <- coding_overlap[, .(coder_id, post_heading, post_mention_substantive_content)] %>% na.omit(.)
test <- test[!duplicated(test, by = c("coder_id", "post_heading")), ]
test[post_mention_substantive_content %in% c(4,7), post_mention_substantive_content := NA]
## 4 == na, 7 = no mention of candidates
spread(test, key = "post_heading", value = "post_mention_substantive_content")[, -1] %>%
  as.matrix(.) %>% kripp.alpha(., method = "nominal")

## post_mention_valence: alpha = 0.738
test <- coding_overlap[, .(coder_id, post_mention_candidate, post_heading, post_mention_valence)] %>% na.omit(.)
test[post_mention_candidate == 2, post_mention_valence := car::recode(post_mention_valence, "1=3;2=2;3=1;else=NA")]
test[post_mention_candidate == 1, post_mention_valence := car::recode(post_mention_valence, "1=1;2=2;3=3;else=NA")]
test[post_mention_candidate %in% c(3,4,5,6,7), post_mention_valence := NA]

test <- test[, .(coder_id, post_heading, post_mention_valence)] %>% na.omit(.)
test <- test[!duplicated(test, by = c("coder_id", "post_heading")), ]
spread(test, key = "post_heading", value = "post_mention_valence")[, -1] %>%
  as.matrix(.) %>% kripp.alpha(., method = "nominal")

test <- spread(test, key = "post_heading", value = "post_mention_valence")[, -1] %>%
  as.matrix(.) %>% t %>% unname

lapply(1:6, function(i) {
  cols <- combn(c(1,2,3,4), m = 2)[,i] %>% as.numeric
  stats <- test[, cols] %>% agree
  stats$value
}) %>% unlist %>% mean

## required sample size calculator
## based on Krippendorff (2011)

Pc <- 1/3 ## probablity of one category
A_min <- .738 ## min acceptable alpha
z_p <- 1.645 ## 0.05 level, z score

2*(z_p)^2*( (1+A_min)*(3-A_min)/(4*(1-A_min)*Pc*(1-Pc)) - A_min)




## ------------------------------------------------------------------------------------- ##
## Correlating poster candidate valence with self-reported candidate preference/ideology ##
## ------------------------------------------------------------------------------------- ##

## load survey data
dat <- haven::read_sav("Dat/DiscussionForumThreeWavePanel(N=341).sav")
setDT(dat)
dat$poster_id <- dat$글쓴이_닉네임_first

coding[, post_mention_valence %>% table]
coding.old <- coding

coding[post_mention_candidate == 2, post_mention_valence := car::recode(post_mention_valence, "1=1;2=0;3=-1;else=NA")]
coding[post_mention_candidate == 1, post_mention_valence := car::recode(post_mention_valence, "1=-1;2=0;3=1;else=NA")]
coding[post_mention_candidate %in% c(3,4,5,6,7), post_mention_valence := NA]
coding[post_mention_valence %in% c(4,7), post_mention_valence := NA]

test <- merge(dat[, .(poster_id, car::recode(hv16, "1=0; 2=1; else=2"))],
      coding[, .(eval = mean(post_mention_valence, na.rm = T)), by = poster_id],
      key = "poster_id")[, -1] %>% as.matrix

setDT(test <- data.frame(test))
test[, mean(eval, na.rm = T), by = V2]
colnames(test) <- c("vote_choice", "net.eval")
test$vote_choice <- factor(test$vote_choice)

ggplot(test[!is.na(net.eval), ], aes(x = net.eval, fill = vote_choice)) +
  geom_density(color = NA, alpha = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = c("#F8766D", "#619CFF", "gray")) +
  facet_wrap(~ vote_choice,
             labeller = labeller(vote_choice =
                                   c("0" = "Ruling Party",
                                     "1" = "Opposition Party",
                                     "2" = "Abstained"))) +
  geom_vline(data = test[!is.na(net.eval), mean(net.eval, na.rm = T), by = vote_choice],
             aes(xintercept = V1, color = vote_choice), linetype = "dashed", size = 0.7, show.legend = FALSE) +
  scale_color_manual(values = c("#F8766D", "#619CFF", "gray")) +
  xlab("Net message valence") + ylab("") + theme_bw()


cor.test(car::recode(test$vote_choice, "2 = NA", as.factor = F), test$net.eval, use = "complete.obs")






## Who mention Park in post contents (N = 231), merged with survey data (after merge, N = 180)
merge(dat[, .(poster_id, canpref1, canpref2, canpref3)],
      coding[post_mention_candidate == 1,
             .(park_eval = mean(post_mention_valence, na.rm = T)), by = poster_id],
      key = "poster_id")[, -1] %>% as.matrix %>% cor(., use = "complete.obs")

#            canpref1  canpref2  canpref3 park_eval
# canpref1  1.0000000 0.7537890 0.8018755 0.6027952
# canpref2  0.7537890 1.0000000 0.7347017 0.5977890
# canpref3  0.8018755 0.7347017 1.0000000 0.6317911
# park_eval 0.6027952 0.5977890 0.6317911 1.0000000

## cf. excluding neutral posts
merge(dat[, .(poster_id, canpref1, canpref2, canpref3)],
      coding[post_mention_candidate == 1,
             .(park_eval = post_mention_valence %>%
                 car::recode(., "2 = NA") %>% mean(., na.rm = T)), by = poster_id],
      key = "poster_id")[, -1] %>% as.matrix %>% cor(., use = "pairwise.complete.obs")

## cf. ideology
merge(dat[, .(poster_id, pv258, kv49, hv104)],
      coding[post_mention_candidate == 1,
             .(park_eval = mean(post_mention_valence, na.rm = T)), by = poster_id],
      key = "poster_id")[, -1] %>% as.matrix %>% cor(., use = "pairwise.complete.obs")

## Who mention Moon in post contents (N = 171), merged with survey data (after merge, N = 142)
merge(dat[, .(poster_id, canpref1, canpref2, canpref3)],
      coding[post_mention_candidate == 2,
             .(moon_eval = mean(post_mention_valence, na.rm = T)), by = poster_id],
      key = "poster_id")[, -1] %>% as.matrix %>% cor(., use = "complete.obs")

#             canpref1   canpref2   canpref3  moon_eval
# canpref1   1.0000000  0.8169007  0.8291169 -0.3936559
# canpref2   0.8169007  1.0000000  0.7758567 -0.3490185
# canpref3   0.8291169  0.7758567  1.0000000 -0.4304209
# moon_eval -0.3936559 -0.3490185 -0.4304209  1.0000000

## Who mention both Moon and Park in contents (N = 110), merged with survey data (after merge, N = 92)
temp <- merge(coding[post_mention_candidate == 1,
                     .(park_eval = mean(post_mention_valence, na.rm = T)), by = poster_id],
              coding[post_mention_candidate == 2,
                     .(moon_eval = mean(post_mention_valence, na.rm = T)), by = poster_id],
              key = "poster_id")[, .(poster_id, diff = apply(.SD, 1, diff)),
                                 .SDcols = c("park_eval", "moon_eval")] %>% setDT

merge(dat[, .(poster_id, canpref1, canpref2, canpref3)],
      temp[-1, ], by = "poster_id") [, -1] %>% as.matrix %>% cor(., use = "pairwise.complete.obs")

#           canpref1   canpref2   canpref3       diff
# canpref1  1.0000000  0.8454105  0.8709439 -0.7144667
# canpref2  0.8454105  1.0000000  0.8187646 -0.6814621
# canpref3  0.8709439  0.8187646  1.0000000 -0.7664598
# diff     -0.7144667 -0.6814621 -0.7664598  1.0000000


coding[, valence_sum := ifelse(post_mention_candidate == 1, ## mention park
                               car::recode(post_mention_valence, "1=1;3=3;2=2;else=NA"),
                               ifelse(post_mention_candidate == 2,
                                      car::recode(post_mention_valence, "3=1;1=3;2=2;else=NA"), NA))]

test <- coding[, .(mean.valence = mean(valence_sum, na.rm = T)), by = poster_id]
test <- merge(dat[, .(poster_id, kv37_36, hv63_62)],
              test, by = "poster_id") %>% as.data.frame

with(test, cor.test(x = kv37_36, y = mean.valence, use = "complete.obs"))
with(test, cor.test(x = hv63_62, y = mean.valence, use = "complete.obs"))
with(test, cor.test(x = canpref3, y = mean.valence, use = "complete.obs"))

setDT(test)
require(ggplot2)

test[, pref.all := apply(.SD, 1, sum), .SDcols = c("canpref1", "canpref2", "canpref3")]
test[, pref.all := car::recode(pref.all, "1 = 1; 2 = 1; 3 = 2")]
test[, pref.all.cat := car::recode(pref.all, "0 = 'Park'; 2 = 'Moon'; else = 'Swing'", as.factor = T)]

ggplot(test[!is.na(mean.valence), ], aes(x = mean.valence, color = pref.all.cat)) +
  geom_density() + geom_vline(
    data = test[!is.na(mean.valence), mean(mean.valence, na.rm = T), by = pref.all.cat],
                              aes(xintercept = V1, color = pref.all.cat), linetype = "dashed") +
  facet_wrap(~ pref.all.cat)

with(test[!is.na(mean.valence), ],
      cor.test(mean.valence, pref.all, use = "complete.obs"))
## test[!is.na(mean.valence), .N]


## convert to bipolar, and compute reliability



##
