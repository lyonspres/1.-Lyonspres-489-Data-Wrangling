#poisson distribution 489
#Missingness analysis
#Simulation discussion




## Johannes' Help in Oraganising Analyses

df1$irv <- careless::irv(df1[,short])
hist(df1$irv)
df1$irv <- careless::longstring(df1[,short])
df1$irv <- careless::irv(df1[,short])
df1$long <- careless::longstring(df1[,short])
plot(df1$irv, df1$long)
df1$mah <- careless::mahad(df1[,short])
hist(df1$mah)


df1$irv <- careless::irv(df1[,short])
df1$long <- careless::longstring(df1[,short])
df1$mah <- careless::mahad(df1[,short])


ufs::scaleStructure(df1[c("td_test_3_5", "td_mem_3_5", "td_sug_3_5")])

ufs::scaleStructure(df1[c("td_test_6_11", "td_mem_6_11", "td_sug_6_11")])

ufs::scaleStructure(df1[c("id_test_adult", "id_mem_adult", "id_sug_adult")])

ufs::scaleStructure(df1[c("td_test_adult", "td_mem_adult", "td_sug_adult")])

psych::fa.parallel(df1[c(id_test_child, id_mem_child, id_sug_child)], n.iter = 1000)
psych::fa.parallel(df1[c(td_test_3_5, td_mem_3_5, td_sug_3_5)], n.iter = 1000)
psych::fa.parallel(df1[c(td_test_6_11, td_mem_6_11, td_sug_6_11)], n.iter = 1000)
psych::fa.parallel(df1[c(id_test_adult, id_mem_adult, id_sug_adult)], n.iter = 1000)
psych::fa.parallel(df1[c(td_test_adult, td_mem_adult, td_sug_adult)], n.iter = 1000)

psych::fa.parallel(df1[c(td_test_adult, td_mem_adult, td_sug_adult, id_test_adult, id_mem_adult, id_sug_adult, td_test_6_11, td_mem_6_11, td_sug_6_11, td_test_3_5, td_mem_3_5, td_sug_3_5, id_test_child, id_mem_child, id_sug_child)], n.iter = 1000)

psych::fa(df1[c(id_test_child, id_mem_child, id_sug_child)], rotate = "oblimin", nfactors = 2) %>%
  print.psych(., cut = .40, sort = T)

psych::fa(df1[c(td_test_3_5, td_mem_3_5, td_sug_3_5)], rotate = "oblimin", nfactors = 3) %>%
  print.psych(., cut = .40, sort = T)

psych::fa(df1[c(td_test_6_11, td_mem_6_11, td_sug_6_11)], rotate = "oblimin", nfactors = 2) %>%
  print.psych(., cut = .40, sort = T)

psych::fa(df1[c(id_test_adult, id_mem_adult, id_sug_adult)], rotate = "oblimin", nfactors = 3) %>%
  print.psych(., cut = .40, sort = T)

psych::fa(df1[c(td_test_adult, td_mem_adult, td_sug_adult)], rotate = "oblimin", nfactors = 3) %>%
  print.psych(., cut = .40, sort = T)

psych::fa(df1[c(id_test_adult, id_mem_adult, id_sug_adult)], rotate = "oblimin", nfactors = 1) %>%
  print.psych(., cut = .40, sort = T)

psych::fa(df1[c(td_test_adult, td_mem_adult, td_sug_adult)], rotate = "oblimin", nfactors = 1) %>%
  print.psych(., cut = .40, sort = T)

psych::fa(df1[c(td_test_adult, td_mem_adult, td_sug_adult, id_test_adult, id_mem_adult, id_sug_adult, td_test_6_11, td_mem_6_11, td_sug_6_11, td_test_3_5, td_mem_3_5, td_sug_3_5, id_test_child, id_mem_child, id_sug_child)], rotate = "oblimin", nfactors = 12) %>%
  print.psych(., cut = .40, sort = T)




df1$dn_id <- rowMeans(df1[paste0(c(id_test_child, id_mem_child, id_sug_child), "_bin")])
df1$dn_td <- rowMeans(df1[paste0(c(td_test_3_5, td_mem_3_5, td_sug_3_5, td_test_6_11, td_mem_6_11, td_sug_6_11), "_bin")])
df1$dn_id_ad <- rowMeans(df1[paste0(c(id_test_adult, id_mem_adult, id_sug_adult), "_bin")])
df1$dn_td_ad <- rowMeans(df1[paste0(c(td_test_adult, td_mem_adult, td_sug_adult), "_bin")])

```{r dkexploratoryanalysis, include=TRUE}
## Recode the scales to compare don't knows against actual answers

df1[,paste0(short, "_bin")] <- lapply(df1[,short], function(x){
  car::recode(x, "1 = 0; 2 = 0; 3 = 0; 4 = 0; 5 = 0; 6 = 0; 7 = 1")})

df1$dn_id <- rowMeans(df1[paste0(c(id_test_child, id_mem_child, id_sug_child), "_bin")])
df1$dn_td <- rowMeans(df1[paste0(c(td_test_3_5, td_mem_3_5, td_sug_3_5, td_test_6_11, td_mem_6_11, td_sug_6_11), "_bin")])
df1$dn_id_ad <- rowMeans(df1[paste0(c(id_test_adult, id_mem_adult, id_sug_adult), "_bin")])
df1$dn_td_ad <- rowMeans(df1[paste0(c(td_test_adult, td_mem_adult, td_sug_adult), "_bin")])

df1[,short] <- lapply(df1[,short], function(x){
  car::recode(x, "1 = 1; 2 = 2; 3 = 3; 4 = 4; 5 = 5; 6 = 6; else = NA")})

## Analyse the DK responses

dn_long <- 
  df1 %>%
  select(., dn_id, dn_td, dn_id_ad, dn_td_ad, id) %>%
  pivot_longer(., -id)

lmer_out <- lmer(value ~ name + (1|id), dn_long)
lm_out <- lm(value ~ name , dn_long)
sjPlot::plot_model(lmer_out, type = "pred")
summary(lmer_out)

chisq <- chisq.test(df1[,paste0(short, "_bin")])

report::report(chisq)
```