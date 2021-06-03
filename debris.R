

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