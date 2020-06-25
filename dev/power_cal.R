
## Power calculations

test1 <- lapply(c(341, 1000, 2500), function(k) sim.MC.power.obj(sample_n = k))
test1 <- do.call("rbind", test1) %>% setDT(.)
test1[sample_n == 341, mean(sig < 0.05)] ## empirical power = 0.3006 when sample size = 341
test1[sample_n == 1000, mean(sig < 0.05)] ## 0.711 when alpha = 0.05
test1[sample_n == 1000, mean(sig < 0.01)] ##  0.487 when alpha = 0.01
test1[sample_n == 2500, mean(sig < 0.001)] ## 0.7786 when alpha = 0.001
test1[sample_n == 2500, mean(sig < 0.01)] ## 0.9346 when alpha = 0.01

test2 <- lapply(c(341, 1000, 2500), function(k) sim.MC.power.sbj(sample_n = k))
test2 <- do.call("rbind", test2) %>% setDT(.)
test2[sample_n == 341, mean(sig < 0.05)] ## empirical power = 0.5548 when sample size = 341
test2[sample_n == 1000, mean(sig < 0.05)] ## 0.949 when alpha = 0.05
test2[sample_n == 1000, mean(sig < 0.01)] ## 0.8544 when alpha = 0.01
test2[sample_n == 2500, mean(sig < 0.001)] ## 0.994 when alpha = 0.001
test2[sample_n == 2500, mean(sig < 0.01)] ## 1 when alpha = 0.001
