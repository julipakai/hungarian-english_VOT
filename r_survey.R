#set working directory
setwd("C:/Juli/Education/uni/masters/york/dissertation/r_repo")
# survey data eval
# load libraries
library(plyr)
library(dplyr)
library(ggplot2)


# bilingual demographic data
survey <- read.csv("studydata.csv", encoding="UTF-8", header=TRUE, colClasses = c("part_id"="character"))
countries <- read.csv("bi_ages_countries.csv", header=TRUE, colClasses = c("character", "character", "numeric"))

# clean data
survey <- survey %>% filter(!grepl("Test", part_id))
survey <- survey %>% filter(!grepl("test", part_id))
# only evaluated participants
survey2 <- survey %>% subset(part_id=="001" | part_id=="002" | part_id=="005" | part_id=="009" | part_id=="010" | part_id=="016" | part_id=="021")
# gender
table(survey2$gender)
# education
table(survey2$education)
# age
min(survey2$age)
max(survey2$age)
# country they're from
table(survey2$birthplace)
# where they live now
table(survey2$current_country)
# how long they lived in countries
countries_sorted <- arrange(countries, participant, years)
countries_cumsum <- ddply(countries_sorted, "participant",
                          transform,
                          label_ypos=cumsum(years))
table(countries$country)
ggplot(countries_cumsum, aes(x=participant, y=years, fill=country, label=years))+
  geom_bar(stat="identity")+
  geom_text(size = 4, position = position_stack(vjust = 0.5))+
  labs(x="participant", y="time spent in country (in years)", title="Time spent in countries by participant", coulour="Country")+
  scale_fill_manual(values= c("#F9F871", "#A7F289", "#00D0CB", "#359ED3", "#6D79C5", "#8763B0", "#A33370", "#CF4C68"))
# monolingual hungarian demographic data
bea <- read.csv("bea_demographics.csv", encoding="UTF-8", header=TRUE, colClasses = c("participant"="character"))
# gender
table(bea$gender)
# age
min(bea$age)
max(bea$age)
# percentages data
bi_perc <- read.csv("bi_percetages.csv", encoding="UTF-8", header=TRUE, colClasses = c("participant"="character"))
perc_data <- bi_perc %>%  group_by(participant, language) %>% count(type) %>% 
  mutate(ratio=scales::percent(n/sum(n)))

ggplot(bi_perc) +
  aes(x=forcats::fct_rev(participant), fill=factor(type))+
  geom_bar(position="fill")+
  #stat_count(geom="text",
             # aes(label=paste(round((..count..)/sum(..count..)*100), "%")),
             # position=position_fill(vjust=0.5),
             # colour="black")+
  geom_text(data=perc_data, aes(y=n, label=ratio), size = 4, position = position_fill(vjust = 0.5))+
  labs(x="participant", y="frequency", title="Frequency of VOT types across bilingual speakers", fill="VOT type")+
  facet_wrap(~forcats::fct_rev(language))+
  coord_flip()+
  scale_fill_manual(values= c("#359ED3", "#A33370"))
# monolingual data hungarian
hundata <- read.csv("diss_mono_hu.csv", encoding="UTF-8", header=TRUE, colClasses = c("participant"="character"))
excludedhundata <- hundata %>% filter(excl==TRUE)
hundata <- hundata %>% filter(is.na(excl))
hundata$style <- hundata$task
hundata$style <- gsub("^r", "read", hundata$style)
hundata$style <- gsub("q", "free", hundata$style)
hundata$style <- gsub("c", "free", hundata$style)
hundata$style <- gsub("senr", "free", hundata$style)
hundata$style <- gsub("s", "free", hundata$style)
hundata$lingual <- "mono"
ggplot(transform(hundata,
                 stop=factor(stop, levels=c("p", "t", "k", "b", "d", "ɡ", "ɟ"))))+
  geom_density(lwd=1, colour="#359ED3")+
  aes(x=duration)+
  labs(x="VOT duration in ms", y="density", title="VOT distribution in Hungarian monolingual data")+
  facet_wrap(~stop)
hundataptk  <- hundata %>% subset(stop=="p" | stop=="t" | stop=="k")
hundataptkpos <- hundataptk %>% subset(duration>0)
plot1 <- ggplot(transform(hundataptkpos,
                 stop=factor(stop, levels=c("p", "t", "k", "b", "d", "ɡ", "ɟ"))))+
  geom_boxplot(color="808080")+
  aes(x=reorder(stop, duration), y=duration)+
  labs(x="plosive", y="VOT value in ms", title="VOT values in Hungarian monolingual data")
hundatabdggy <- hundata %>% subset(stop=="b" | stop=="d" | stop=="ɡ" | stop=="ɟ")
hundatabdggyneg <- hundatabdggy %>% subset(duration<0)
plot2 <- ggplot(transform(hundatabdggyneg,
                 stop=factor(stop, levels=c("b", "d", "ɡ", "ɟ"))))+
  geom_boxplot(color="808080")+
  aes(x=stop, y=duration)+
  labs(x="plosive", y="VOT value in ms", title="")
grid.arrange(plot1, plot2, ncol=2)

hundatap <- hundataptkpos %>% subset(stop=="p")
hundatat <- hundataptkpos %>% subset(stop=="t")
hundatak <- hundataptkpos %>% subset(stop=="k")
hundatap_read <- hundatap %>% subset(style=="read")
hundatap_free <- hundatap %>% subset(style!="read")
hundatat_read <- hundatat %>% subset(style=="read")
hundatat_free <- hundatat %>% subset(style!="read")
hundatak_read <- hundatak %>% subset(style=="read")
hundatak_free <- hundatak %>% subset(style!="read")
# monolingual data english
engdata <- read.csv("mono_eng.csv", encoding="UTF-8", header=TRUE, colClasses = c("ID"="character"))
engdata$lingual <- "mono"
engdata$duration <- engdata$duration*1000
ggplot(transform(engdata,
                 stop=factor(stop, levels=c("p", "t", "k"))))+
  geom_boxplot(color="808080")+
  aes(x=stop, y=duration)+
  labs(x="plosive", y="VOT value in ms", title="VOT values in English monolingual data (SSBE)")
ggplot(transform(engdata,
                 stop=factor(stop, levels=c("p", "t", "k"))))+
  geom_density(lwd=1, colour="#359ED3")+
  aes(x=duration)+
  labs(x="VOT duration in ms", y="density", title="VOT distribution English monolingual data (SSBE)")+
  facet_wrap(~stop)
engdatap <- engdata %>% subset(stop=="p")
engdatat <- engdata %>% subset(stop=="t")
engdatak <- engdata %>% subset(stop=="k")
amengdata <- read.csv("mono_en_us.csv", encoding="UTF-8", header=TRUE, colClasses = c("subj"="character"))
amengdata$lingual <- "mono"
amengdataptk <-  amengdata %>% subset(stop=="P" | stop=="T" | stop=="K")
amengdatabdg <-  amengdata %>% subset(stop=="B" | stop=="D" | stop=="G")
amengdatap <- amengdataptk %>% subset(stop=="P")
amengdatat <- amengdataptk %>% subset(stop=="T")
amengdatak <- amengdataptk %>% subset(stop=="K")
ggplot(transform(amengdata,
                 stop=factor(stop, levels=c("P", "T", "K", "B", "D", "G"))))+
  geom_boxplot(color="808080")+
  aes(x=stop, y=vot)+
  labs(x="plosive", y="VOT value in ms", title="VOT values in English monolingual data (AmE)")
ggplot(transform(amengdata,
                 stop=factor(stop, levels=c("P", "T", "K", "B", "D", "G"))))+
  geom_density(lwd=1, colour="#359ED3")+
  aes(x=vot)+
  labs(x="VOT duration in ms", y="density", title="VOT distribution  in English monolingual data (AmE)")+
  facet_wrap(~stop)
# bilignual data
bidata <- read.csv("diss_bi_all.csv", encoding="UTF-8", header=TRUE, colClasses = c("participant"="character"))
bidata$duration <- as.numeric(bidata$duration)
sapply(bidata, class)
bidata[bidata==""] <- NA
bidata$style <- bidata$task
bidata$style <- gsub("^q.", "free", bidata$style)
bidata$style <- gsub("^r.", "read", bidata$style)
bidata$style <- gsub("sen", "read", bidata$style)
bidata$style <- gsub("^s.", "free", bidata$style)
bidata$lingual <- "bi"
bidata$residence <- with(bidata, ifelse(
  participant=="002" | participant=="021" | participant=="005" | participant=="009", "UK", "notUK"))
bidata$stop <- gsub("g", "ɡ", bidata$stop)
bidata <- bidata %>% subset(stop != "l" & stop != "o" & stop != "c")
bidata$duration <- as.numeric(as.character(bidata$duration))
binona[binona==""] <- NA
binona <- bidata %>% subset(is.na(excl))
# binona <- binona %>% subset(!is.na(stop))
# binona <- binona %>% subset(!is.na(duration))
table(binona$stop)
binona$duration <- as.numeric(as.character(binona$duration))
binona <- binona %>% subset(!is.na(duration))
binona$duaration <- as.numeric(binona$duration)
sapply(binona, class)
ggplot(transform(binona,
                 stop=factor(stop, levels=c("p", "t", "k", "b", "d", "ɡ", "ɟ"))))+
  geom_density(lwd=1)+
  aes(x=duration, color=stop)+
  labs(x="VOT duration in ms", y="density", title="VOT distribution in bilingual data", colour="stop")+
  facet_grid(participant~language, switch="y")+
  ylim(NA, 0.10)+
  xlim(-150, 100)+
  geom_vline(xintercept=0, linetype="dashed")

bi_en <- bidata %>% subset(language == "en")
plosivedistr_bi_en <- bi_en %>% count(stop)
plosivedistr_bi_en
bi_hu <- bidata %>% subset(language == "hu")
plosivedistr_bi_hu <- bi_hu %>% count(stop)
plosivedistr_bi_hu
excludedbiengdata <- bi_en %>% subset(!is.na(excl))
excludedbihundata <- bi_hu %>% subset(!is.na(excl))
bi_en <- bi_en %>% filter(is.na(excl))
bi_hu <- bi_hu %>% filter(is.na(excl))
bi_enptk <-  bi_en %>% subset(stop=="p" | stop=="t" | stop=="k")
bi_enptkpos <- bi_enptk %>% subset(duration>0)
ggplot(transform(bi_enptkpos,
                 stop=factor(stop, levels=c("p", "t", "k"))))+
  geom_boxplot(color="#808080")+
  aes(x=reorder(stop, duration, FUN=median), y=duration, fill=participant)+
  labs(x="plosive", y="VOT value in ms", title="Boxplot of VOT values in bilingual English data")+
  scale_fill_manual(values= c("#F9F871", "#A7F289", "#00D0CB", "#359ED3", "#6D79C5", "#8763B0", "#A33370", "#CF4C68"))
bi_enp <- bi_enptkpos %>% subset(stop=="p")
bi_ent <- bi_enptkpos %>% subset(stop=="t")
bi_enk <- bi_enptkpos %>% subset(stop=="k")
bi_enp_read <- bi_enp %>% subset(style=="read")
bi_ent_read <- bi_ent %>% subset(style=="read")
bi_enk_read <- bi_enk %>% subset(style=="read")
bi_enp_read <- bi_enp %>% subset(style=="free")
bi_ent_read <- bi_ent %>% subset(style=="free")
bi_enk_read <- bi_enk %>% subset(style=="free")
bi_enbdg <-  bi_en %>% subset(stop=="b" | stop=="d" | stop=="ɡ")
bi_enbdg_posvotvals <- bi_enbdg %>% count(participant, type)
bi_enbdg_posvotvals
bi_enbdgneg <- bi_enbdg %>% subset(duration<0)
bi_enbdgpos <- bi_enbdg %>% subset(duration>0)
bi_huptk <-  bi_hu %>% subset(stop=="p" | stop=="t" | stop=="k")
bi_huptkpos <- bi_huptk %>% subset(duration>0)
ggplot(transform(bi_huptkpos,
                 stop=factor(stop, levels=c("p", "t", "k"))))+
  geom_boxplot(color="#808080")+
  aes(x=reorder(stop, duration, FUN=median), y=duration, fill=participant)+
  labs(x="plosive", y="VOT value in ms", title="Boxplot of VOT values in bilingual Hungarian data")+
  scale_fill_manual(values= c("#F9F871", "#A7F289", "#00D0CB", "#359ED3", "#6D79C5", "#8763B0", "#A33370", "#CF4C68"))
bi_hup <- bi_huptkpos %>% subset(stop=="p")
bi_hut <- bi_huptkpos %>% subset(stop=="t")
bi_huk <- bi_huptkpos %>% subset(stop=="k")
bi_hup_read <- bi_hup %>% subset(style=="read")
bi_hup_free <- bi_hup %>% subset(style!="read")
bi_hut_read <- bi_hut %>% subset(style=="read")
bi_hut_free <- bi_hut %>% subset(style!="read")
bi_huk_read <- bi_huk %>% subset(style=="read")
bi_huk_free <- bi_huk %>% subset(style!="read")
bi_hubdggy <-  bi_hu %>% subset(stop=="b" | stop=="d" | stop=="ɡ" | stop=="ɟ")
bi_hubdggy_posvotvals <- bi_hubdggy %>% count(participant, type)
bi_hubdggy_posvotvals
bi_hubdggyneg <- bi_hubdggy %>% subset(duration<0)
bi_hubdggypos <- bi_hubdggy %>% subset(duration>0)
# test style significance
kruskal.test(duration~style, data=bi_enptkpos)
kruskal.test(duration~style, data=bi_huptkpos)
kruskal.test(duration~style, data=hundataptkpos)
# test for normal distributions
# mono hu
shapiro.test(hundatap$duration)
shapiro.test(hundatat$duration)
shapiro.test(hundatak$duration)
# mono eng
shapiro.test(engdatap$duration)
shapiro.test(engdatat$duration)
shapiro.test(engdatak$duration)
# mono ameng
ks.test(amengdatap$duration, "pnorm")
ks.test(amengdatat$duration, "pnorm")
ks.test(amengdatak$duration, "pnorm")
# bi hu
shapiro.test(bi_hup$duration)
shapiro.test(bi_hut$duration)
shapiro.test(bi_huk$duration)
shapiro.test(bi_hup_read$duration)
shapiro.test(bi_hut_read$duration)
shapiro.test(bi_huk_read$duration)
shapiro.test(bi_hup_free$duration)
shapiro.test(bi_hut_free$duration)
shapiro.test(bi_huk_free$duration)
# bi en
shapiro.test(bi_enp_read$duration)
shapiro.test(bi_ent_read$duration)
shapiro.test(bi_enk_read$duration)
# means
detach(package:plyr)
amengdatabdg_means <- amengdatabdg %>% 
  group_by(subj, stop) %>% 
  summarise(mean=mean(vot, na.rm=TRUE),
            sd=sd(vot, na.rm=TRUE))
amengdatabdg_means
amengdatabdg_gmeans <- amengdatabdg_means %>% 
  group_by(stop) %>% 
  summarise(gmean=mean(mean, na.rm=TRUE),
            sd=sd(mean, na.rm=TRUE))
amengdatabdg_gmeans
amengdataptk_means <- amengdataptk %>% 
  group_by(subj, stop) %>% 
  summarise(mean=mean(vot, na.rm=TRUE),
            sd=sd(vot, na.rm=TRUE))
amengdataptk_means
amengdataptk_gmeans <- amengdataptk_means %>% 
  group_by(stop) %>% 
  summarise(gmean=mean(mean, na.rm=TRUE),
            sd=sd(mean, na.rm=TRUE))
amengdataptk_gmeans
engdata_means <- engdata %>% 
  group_by(ID, stop) %>% 
  summarise(mean=mean(duration, na.rm=TRUE),
            sd=sd(duration, na.rm=TRUE))
engdata_means
engdata_gmeans <- engdata_means %>% 
  group_by(stop) %>% 
  summarise(gmean=mean(mean, na.rm=TRUE),
            sd=sd(mean, na.rm=TRUE))
engdata_gmeans

hundataptk_means <- hundataptkpos %>% 
  group_by(participant, stop) %>% 
  summarise(mean=mean(duration, na.rm=TRUE),
            sd=sd(duration, na.rm=TRUE))
hundataptk_means
hundata_ptk_gmeans <- hundataptk_means %>% 
  group_by(stop) %>% 
  summarise(gmean=mean(mean, na.rm=TRUE),
            sd=sd(mean, na.rm=TRUE))
hundata_ptk_gmeans
hundatabdggy_means <- hundatabdggyneg %>% 
  group_by(participant, stop) %>% 
  summarise(mean=mean(duration, na.rm=TRUE),
            sd=sd(duration, na.rm=TRUE))
hundatabdggy_means
hundata_bdggy_gmeans <- hundatabdggy_means %>% 
  group_by(stop) %>% 
  summarise(gmean=mean(mean, na.rm=TRUE),
            sd=sd(mean, na.rm=TRUE))
hundata_bdggy_gmeans

bi_huptk_pos_means <- bi_huptkpos %>% 
  group_by(participant, stop) %>% 
  summarise(mean=mean(duration, na.rm=TRUE),
            sd=sd(duration, na.rm=TRUE))
bi_huptk_pos_means
bi_huptk_pos_gmeans <- bi_huptk_pos_means %>% 
  group_by(stop) %>% 
  summarise(gmean=mean(mean, na.rm=TRUE),
            sd=sd(mean, na.rm=TRUE))
bi_huptk_pos_gmeans
bi_hubdggy_neg_means <- bi_hubdggyneg %>% 
  group_by(participant, stop) %>% 
  summarise(mean=mean(duration, na.rm=TRUE),
            sd=sd(duration, na.rm=TRUE))
bi_hubdggy_neg_means
bi_hubdggy_neg_gmeans <- bi_hubdggy_neg_means %>% 
  group_by(stop) %>% 
  summarise(gmean=mean(mean, na.rm=TRUE),
            sd=sd(mean, na.rm=TRUE))
bi_hubdggy_neg_gmeans
bi_hubdggy_pos_means <- bi_hubdggypos %>% 
  group_by(participant, stop) %>% 
  summarise(mean=mean(duration, na.rm=TRUE),
            sd=sd(duration, na.rm=TRUE))
bi_hubdggy_pos_means
bi_hubdggy_pos_gmeans <- bi_hubdggy_pos_means %>% 
  group_by(stop) %>% 
  summarise(gmean=mean(mean, na.rm=TRUE),
            sd=sd(mean, na.rm=TRUE))
bi_hubdggy_pos_gmeans
bi_enptk_pos_means <- bi_enptkpos %>% 
  group_by(participant, stop) %>% 
  summarise(mean=mean(duration, na.rm=TRUE),
            sd=sd(duration, na.rm=TRUE))
bi_enptk_pos_means
bi_enptk_pos_gmeans <- bi_enptk_pos_means %>% 
  group_by(stop) %>% 
  summarise(gmean=mean(mean, na.rm=TRUE),
            sd=sd(mean, na.rm=TRUE))
bi_enptk_pos_gmeans
bi_enbdg_pos_means <- bi_enbdgpos %>% 
  group_by(participant, stop) %>% 
  summarise(mean=mean(duration, na.rm=TRUE),
            sd=sd(duration, na.rm=TRUE))
bi_enbdg_pos_means
bi_enbdg_pos_gmeans <- bi_enbdg_pos_means %>% 
  group_by(stop) %>% 
  summarise(gmean=mean(mean, na.rm=TRUE),
            sd=sd(mean, na.rm=TRUE))
bi_enbdg_pos_gmeans
bi_enbdg_neg_means <- bi_enbdgneg %>% 
  group_by(participant, stop) %>% 
  summarise(mean=mean(duration, na.rm=TRUE),
            sd=sd(duration, na.rm=TRUE))
bi_enbdg_neg_means
bi_enbdg_neg_gmeans <- bi_enbdg_neg_means %>% 
  group_by(stop) %>% 
  summarise(gmean=mean(mean, na.rm=TRUE),
            sd=sd(mean, na.rm=TRUE))
bi_enbdg_neg_gmeans

# significance testing
bi_mono_hu_common <- intersect(colnames(hundata), colnames(bidata))
bi_mono_hup_free <- rbind(hundatap_free[bi_mono_hu_common], bi_hup_free[bi_mono_hu_common])
bi_mono_hut_free <- rbind(hundatat_free[bi_mono_hu_common], bi_hut_free[bi_mono_hu_common])
bi_mono_huk_free <- rbind(hundatak_free[bi_mono_hu_common], bi_huk_free[bi_mono_hu_common])
bi_mono_hup_read <- rbind(hundatap_read[bi_mono_hu_common], bi_hup_read[bi_mono_hu_common])
bi_mono_hut_read <- rbind(hundatat_read[bi_mono_hu_common], bi_hut_read[bi_mono_hu_common])
bi_mono_huk_read <- rbind(hundatak_read[bi_mono_hu_common], bi_huk_read[bi_mono_hu_common])
kruskal.test(duration~lingual, data=bi_mono_hup_free)
kruskal.test(duration~lingual, data=bi_mono_hut_free)
kruskal.test(duration~lingual, data=bi_mono_huk_free)
kruskal.test(duration~lingual, data=bi_mono_hup_read)
kruskal.test(duration~lingual, data=bi_mono_hut_read)
kruskal.test(duration~lingual, data=bi_mono_huk_read)


colnames(amengdatap)[which(names(amengdatap)=="subj")] <- "participant"
colnames(amengdatap)[which(names(amengdatap)=="vot")] <- "duration"
colnames(amengdatat)[which(names(amengdatat)=="subj")] <- "participant"
colnames(amengdatat)[which(names(amengdatat)=="vot")] <- "duration"
colnames(amengdatak)[which(names(amengdatak)=="subj")] <- "participant"
colnames(amengdatak)[which(names(amengdatak)=="vot")] <- "duration"
colnames(engdatap)[which(names(engdatap)=="ID")] <- "participant"
colnames(engdatat)[which(names(engdatat)=="ID")] <- "participant"
colnames(engdatak)[which(names(engdatak)=="ID")] <- "participant"
bi_mono_amen_common <- intersect(colnames(amengdatap), colnames(bidata))
bi_mono_en_common <- intersect(colnames(engdatap), colnames(bidata))
bi_mono_enp_read <- rbind(engdatap[bi_mono_en_common], bi_enp_read[bi_mono_en_common])
bi_mono_ent_read <- rbind(engdatat[bi_mono_en_common], bi_ent_read[bi_mono_en_common])
bi_mono_enk_read <- rbind(engdatak[bi_mono_en_common], bi_enk_read[bi_mono_en_common])
bi_mono_amenp_read <- rbind(amengdatap[bi_mono_en_common], bi_enp_read[bi_mono_en_common])
bi_mono_ament_read <- rbind(amengdatat[bi_mono_en_common], bi_ent_read[bi_mono_en_common])
bi_mono_amenk_read <- rbind(amengdatak[bi_mono_en_common], bi_enk_read[bi_mono_en_common])
kruskal.test(duration~lingual, data=bi_mono_enp_read)
kruskal.test(duration~lingual, data=bi_mono_ent_read)
kruskal.test(duration~lingual, data=bi_mono_enk_read)
kruskal.test(duration~lingual, data=bi_mono_amenp_read)
kruskal.test(duration~lingual, data=bi_mono_ament_read)
kruskal.test(duration~lingual, data=bi_mono_amenk_read)


kruskal.test(duration~residence, data=bi_enp)
kruskal.test(duration~residence, data=bi_ent)
kruskal.test(duration~residence, data=bi_enk)

# TODO visualise


