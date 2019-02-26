
## Power calculations

test1 <- lapply(c(341, 1000, 5000), function(k) sim.MC.power.obj(sample_n = k))
test1 <- do.call("rbind", test1) %>% setDT(.)
test1[sample_n == 341, mean(sig < 0.05)] ## empirical power when sample size = 341
test1[sample_n == 1000, mean(sig < 0.05)] ## 0.937 when alpha = 0.05
test1[sample_n == 1000, mean(sig < 0.01)] ## 0.843 when alpha = 0.01
test1[sample_n == 5000, mean(sig < 0.001)]

test2 <- lapply(c(341, 1000, 5000), function(k) sim.MC.power.sbj(sample_n = k))
test2 <- do.call("rbind", test2) %>% setDT(.)
test2[sample_n == 341, mean(sig < 0.05)] ## empirical power when sample size = 341
test2[sample_n == 1000, mean(sig < 0.05)] ## 0.949 when alpha = 0.05
test2[sample_n == 1000, mean(sig < 0.01)] ## 0.844 when alpha = 0.01
test2[sample_n == 5000, mean(sig < 0.001)]
