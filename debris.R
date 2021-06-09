#poisson distribution 489
#Missingness analysis
####Check who's citing these papers 
#Simulation discussion
## bonferonni correction
## moderation for 489 plz
## check IHC acronymn

Keep remembering has this helped the reader understand WHY 


Acknowledging PWID have deficits is not inherently harmful, and can be helpful if it means their additional and unique needs are met (IHC, 2017), however where it can become "dysfunctional" is when perceptions become "focus[sed] inordinately on the characteristics of their disability to the exclusion of all else" (Hehir, 2002). For instance, seeing AWID as eternal children focusses solely on their mental age, and disregards the fact that they are, in fact, adults, who have various transcendent competencies (Emerson et al., 1999; Bowles & Sharman, 2014). Literature on jurors' perceptions of AWID remains extremely scarce, 

Brown and Lewis (2013) have found support for the "mental age hypothesis", (children of the same mental age will be seen as equally competent) with mock jurors finding children of the same mental age (viz., a 7-year-old with ID and a mental age of 5, against a TD 5-year-old) as equally cognitively competent (e.g. memory/recall), suggestible, and trustworthy. This meant that jurors exhibited a capacity to overlook ID prejudice and judge on mental age alone, which is akin to applying the developmental model (cite). Biases against CWID persist when traits (e.g., less detailed testimony, recall competency) typical of ID are not accompanied by disclosure of the presence of the ID (see Henry et al., 2011b; Brown & Lewis, 2013; Crane et al., 2020). 
Peled et al., (2004) concluded that a general (negative) bias regarding the competency of witnesses with IDs may be ameliorated when jurors are presented with actual testimony brown lewis on peled
pecific questions may be problematic (for example, Agnew & Powell, 2004; Brown, Lewis, Lamb, & Stephens, 2012a; Henry & Gudjonsson, 2003). Thus, in terms of capability, it seems there is no evidence-based foundation for the exclusion of these children from judicial processes, if developmentally sensitive communication strategies are employed (Brown, Lewis, Lamb, & Stephens, 2012b). brown  lewis

Notes to include^
  
> Notes to include in full intro:
  
Peled and colleagues (2004) found that merely disclosing a youth 
(15-year-old) eyewitness has a mild intellectual disability, 
biased jurors enough so that the testimony was adjudged as 
being less credible. However, given the literature of jurors' 
perceptions of PWID remains sparse, and though increasing research 
regarding beliefs about children with intellectual disabilities (CWID),
research on beliefs about adults with intellectual disabilities (AWID) 
is particularly limited. 

>> Rare participation in justice system Henry 2011a
ental age level is a reasonably good guide to performance.
(Henry et al., 2011a) <<

  steveons - more likely to worry about them, so maybe that
means they think if they're on the stand they're more able. 
  
  Postive:
  
  Maras discusses how 
have been shown to be less culpable, 


that are longstanding and thus effect the outlook of laypersons who makeup potential jurors, which is why expert evidence is recommended in criminal trials (Stobbs & Kebbell, 2003).

  
## Not useful:

However, negative perceptions by jurors can be mitigated with the inclusion of information about witnesses or defendant's ID diagnoses. A recent study found that the presence of background information about an adult (27-year-old) defendant with autism spectrum disorder (ASD) and their disorder led to more positive perfections of the defendant amongst mock-jurors (Maras et al., 2019). The defendant was also considered more honest, likeable, and less likely to be viewed as guilty, and if so, recommended sentencing was more lenient. Conversely, mock-jurors who did not have background information had negative perceptions of the defendant, rating them as being deceitful, rule, unremorseful, and aggressive. 

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