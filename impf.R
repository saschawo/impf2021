library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)
library(padr)
library(jcolors)
library(gridExtra)
library(ggfittext)

rm(list=ls())

setwd("< Arbeitsverzeichnis >")

impf.files <- dir(pattern = "^impf_", full.names = T)
impf <- rbindlist(lapply(impf.files, fread))
impf$praep <- sapply(impf$Wortform, FUN = function (x) {
  strsplit(x, " ", fixed = T)[[1]][2]
})
names(impf)[7] <- "Rel_Frequenz"
impf$Drittes <- sapply(impf$Lemma, FUN = function (x) {
  strsplit(x, " ", fixed = T)[[1]][3]
})
impf$Datum <- as.Date(impf$Datum)

# alle --------------------------------------------------------------------

impf %>% group_by(Datum, praep) %>%
  summarize(Rel_Frequenz = sum(Rel_Frequenz)) %>%
  ungroup() %>% pad(group = "praep") %>%
  fill_by_value(Rel_Frequenz, 0) -> daten

daten$Rel_Frequenz_rm <- frollmean(daten$Rel_Frequenz, 14, align = "center")

p1 <- ggplot(daten[daten$Datum > "2020-06-30",], aes(x = Datum, y = Rel_Frequenz_rm, col = praep, group = praep)) +
  geom_line(lwd = 1, alpha = 2/3) +
  scale_color_jcolors("pal2") +
  scale_x_date(date_breaks = "2 months") +
  annotate(geom = "point", x = as.Date("2020-12-27"), y = 75) + # Impfstart
  annotate(geom = "text", x = as.Date("2020-12-27"), y = 75, label = "Impfstart",
           vjust = -1) + # Impfstart
  labs(x = "", y = "Relative Häufigkeit", col = "", title = "Impfung ...") +
  theme_minimal() + theme(axis.text.x = element_text(size = 6))

impf %>% group_by(praep) %>%
  summarize(n_Drittes = length(unique(Drittes)),
            ges_Frequenz = sum(Frequenz)) -> gesamt.df

p2 <- ggplot(gesamt.df, aes(y = praep, x = n_Drittes, fill = praep, label = n_Drittes)) +
  geom_col() + scale_y_discrete(limits = rev) +
  geom_bar_text(col = "black", min.size = 2) +
  scale_fill_jcolors("pal2") +
  labs(x = "Anzahl Fortsetzungen", y = "", fill = "") +
  guides(fill = "none") +
  theme_minimal()

p3 <- ggplot(gesamt.df, aes(y = praep, x = ges_Frequenz, fill = praep, label = ges_Frequenz)) +
  geom_col() + scale_y_discrete(limits = rev) +
  geom_bar_text(col = "black", min.size = 2) +
  scale_fill_jcolors("pal2") +
  labs(x = "Gesamthäufigkeit", y = "", fill = "") +
  guides(fill = "none") +
  theme_minimal()

p <- arrangeGrob(p1, p2, p3, layout_matrix = matrix(c(1, 1, 2, 3), ncol = 2, byrow = T), heights = c(.7, .3))

ggsave("byPraep_Time.png", p, width = 8, height = 4, bg = "white", dpi = 400)

 # gegen -------------------------------------------------------------------

impf.gegen <- impf[impf$praep == "gegen",]

impf.gegen %>% group_by(Datum) %>%
  summarize(Rel_Frequenz = sum(Rel_Frequenz)) %>%
  ungroup() %>% pad() %>%
  fill_by_value(Rel_Frequenz, 0) %>%
  arrange(Datum) -> daten

daten$Rel_Frequenz_rm <- frollmean(daten$Rel_Frequenz, 14, align = "center")

# Liste
impf.gegen %>% group_by(Drittes) %>%
  summarize(n = sum(Frequenz)) %>%
  arrange(desc(n)) -> impf.gegen.lem
impf.gegen.lem$Drittes <- factor(impf.gegen.lem$Drittes, levels = impf.gegen.lem$Drittes)

gg.pl <- ggplot(subset(impf.gegen.lem, n > 1), aes(y = Drittes, x = n, label = n)) +
  geom_col(fill = jcolors("pal2")[2]) +
  geom_bar_text(contrast = F, min.size = 8) +
  scale_y_discrete(limits = rev) + 
  labs(y = "", x = "Gesamthäufigkeit", title = "Impfung gegen ...") +
  theme_minimal() + theme(axis.text.y = element_text(size = 10))
  

# für ---------------------------------------------------------------------

impf.fuer <- impf[impf$praep == "für",]

impf.fuer %>% group_by(Datum) %>%
  summarize(Rel_Frequenz = sum(Rel_Frequenz)) %>%
  ungroup() %>% pad() %>%
  fill_by_value(Rel_Frequenz, 0) %>%
  arrange(Datum) -> daten

daten$Rel_Frequenz_rm <- frollmean(daten$Rel_Frequenz, 14, align = "center")

# Liste
impf.fuer %>% group_by(Drittes) %>%
  summarize(n = sum(Frequenz)) %>%
  arrange(desc(n)) -> impf.fuer.lem
impf.fuer.lem$Drittes <- factor(impf.fuer.lem$Drittes, levels = impf.fuer.lem$Drittes)

fuer.pl <- ggplot(subset(impf.fuer.lem, n > 2), aes(y = Drittes, x = n, label = n)) +
  geom_col(fill = jcolors("pal2")[1]) +
  geom_bar_text(contrast = T, min.size = 3) +
  scale_y_discrete(limits = rev) + 
  labs(y = "", x = "Gesamthäufigkeit", title = "Impfung für ...") +
  theme_minimal() + theme(axis.text.y = element_text(size = 10))

# von ---------------------------------------------------------------------

impf.von <- impf[impf$praep == "von",]

impf.von %>% group_by(Datum) %>%
  summarize(Rel_Frequenz = sum(Rel_Frequenz)) %>%
  ungroup() %>% pad() %>%
  fill_by_value(Rel_Frequenz, 0) %>%
  arrange(Datum) -> daten

daten$Rel_Frequenz_rm <- frollmean(daten$Rel_Frequenz, 14, align = "center")

# Liste
impf.von %>% group_by(Drittes) %>%
  summarize(n = sum(Frequenz)) %>%
  arrange(desc(n)) -> impf.von.lem
impf.von.lem$Drittes <- factor(impf.von.lem$Drittes, levels = impf.von.lem$Drittes)

von.pl <- ggplot(subset(impf.von.lem, n > 2), aes(y = Drittes, x = n, label = n)) +
  geom_col(fill = jcolors("pal2")[4]) +
  geom_bar_text(contrast = T, min.size = 3) +
  scale_y_discrete(limits = rev) + 
  labs(y = "", x = "Gesamthäufigkeit", title = "Impfung von ...") +
  theme_minimal() + theme(axis.text.y = element_text(size = 10))

# mit ---------------------------------------------------------------------

impf.mit <- impf[impf$praep == "mit",]

impf.mit %>% group_by(Datum) %>%
  summarize(Rel_Frequenz = sum(Rel_Frequenz)) %>%
  ungroup() %>% pad() %>%
  fill_by_value(Rel_Frequenz, 0) %>%
  arrange(Datum) -> daten

daten$Rel_Frequenz_rm <- frollmean(daten$Rel_Frequenz, 14, align = "center")

# Liste
impf.mit %>% group_by(Drittes) %>%
  summarize(n = sum(Frequenz)) %>%
  arrange(desc(n)) -> impf.mit.lem
impf.mit.lem$Drittes <- factor(impf.mit.lem$Drittes, levels = impf.mit.lem$Drittes)

mit.pl <- ggplot(subset(impf.mit.lem, n > 1), aes(y = Drittes, x = n, label = n)) +
  geom_col(fill = jcolors("pal2")[3]) +
  geom_bar_text(contrast = T, min.size = 3) +
  scale_y_discrete(limits = rev) + 
  labs(y = "", x = "Gesamthäufigkeit", title = "Impfung mit ...") +
  theme_minimal() + theme(axis.text.y = element_text(size = 10))

# Plot kombinieren --------------------------------------------------------

p <- arrangeGrob(gg.pl, mit.pl,
                 von.pl, fuer.pl,
                 ncol = 2)
ggsave("kombi.png", p, width = 10, height = 10, dpi = 400)

# mit - AstraZeneca vs. andere im Zeitverlauf ------------------------------

az <- unique(grep("astra", impf.mit$Drittes, value = T))
impf.mit$Drittes2 <- ifelse(impf.mit$Drittes %in% az, "*astra*", "<andere>")

impf.mit %>% group_by(Datum, Drittes2) %>%
  summarize(Rel_Frequenz = sum(Rel_Frequenz)) %>%
  ungroup() %>% pad(group = "Drittes2") %>%
  fill_by_value(Rel_Frequenz, 0) %>%
  group_by(Drittes2) %>%
  arrange(Datum) %>%
  mutate(Rel_Frequenz_rm = frollmean(Rel_Frequenz, 14, align = "center")) -> daten

daten$Drittes2 <- factor(daten$Drittes2, levels = c("*astra*", "<andere>"))

ggplot(daten, aes(x = as.Date(Datum), y = Rel_Frequenz_rm, col = Drittes2, group = Drittes2)) +
  geom_line(lwd = 1, alpha = 2/3) +
  scale_color_jcolors("pal3") +
  scale_x_date(date_breaks = "2 month") +
  labs(x = "", y = "Relative Frequenz", col = "Impfung mit ...") +
  theme_minimal()
ggsave("mit_astra.png", width = 10, height = 4, dpi = 400, bg = "white")  

# Impf-Komposita ----------------------------------------------------------

impfk <- fread("/Users/sascha/Downloads/impf+x.tsv")
names(impfk)[7] <- c("Rel_Frequenz")
impfk$Datum <- as.Date(impfk$Datum)

# Cleaning
impfk <- impfk[impfk$POS %in% c("NE", "NN") & nchar(impfk$Lemma) > 6,]
impfk$len <- nchar(impfk$Lemma)

impfk2 <- unique(impfk[,c("Lemma", "len")])
impfk2 %>% arrange(len) -> impfk2
head(impfk2, 20)

impfk <- impfk[!(impfk$Lemma %in% c("impfkam", "impfzen", "impfdos",
                                    "impfung", "impfgeg", "impfens",
                                    "impfsto", "impferei", "impfquot",
                                    "impfkomm", "impfpäss", "impfzent",
                                    "impfling", "impfunge", "impfgipf",
                                    "impfbere")),]

impfk$zweit <- gsub("^impf", "", impfk$Lemma)

# Was gibt es?
impfk %>% group_by(Lemma) %>%
  summarize(Frequenz = sum(Frequenz)) %>%
  arrange(desc(Frequenz)) -> impfk.lem

impfk.lem$zweit <- gsub("^impf", "", impfk.lem$Lemma)
impfk.lem$zweit <- factor(impfk.lem$zweit, levels = impfk.lem$zweit)

ggplot(impfk.lem[1:30,], aes(y = zweit, x = Frequenz, label = Frequenz)) +
  geom_col(fill = jcolors("pal2")[1]) +
  geom_bar_text(min.size = 4) +
  scale_y_discrete(limits = rev) +
  labs(x = "Häufigkeit", y = "", title = "Impf...") +
  theme_minimal()
ggsave("impf_komp.png", width = 4, height = 5, dpi = 400, bg = "white")

to.plot <- c("gipfel", "start", "gegner", "nachweis")
impfk.sub <- impfk[impfk$zweit %in% to.plot,]

impfk %>% group_by(Datum, zweit) %>%
  dplyr::filter(zweit %in% to.plot) %>%
  summarize(Rel_Frequenz = sum(Rel_Frequenz)) %>%
  ungroup() %>% pad(group = "zweit") %>%
  fill_by_value(Rel_Frequenz, 0) %>%
  group_by(zweit) %>%
  arrange(Datum) %>%
  mutate(Rel_Frequenz_rm = frollmean(Rel_Frequenz, 14, align = "center")) -> daten

p1 <- ggplot(daten, aes(x = as.Date(Datum), y = Rel_Frequenz_rm, col = zweit, group = zweit)) +
  geom_line(lwd = 1, alpha = 2/3) +
  scale_color_jcolors("pal3") +
  scale_x_date(date_breaks = "2 month", limits = as.Date(c("2020-10-01", today()))) +
  labs(x = "", y = "Relative Frequenz", col = "Impf...") +
  theme_minimal()

to.plot <- c("gipfel", "start", "gegner", "nachweis", "stoff")
impfk.sub <- impfk[impfk$zweit %in% to.plot,]

impfk %>% group_by(Datum, zweit) %>%
  dplyr::filter(zweit %in% to.plot) %>%
  summarize(Rel_Frequenz = sum(Rel_Frequenz)) %>%
  ungroup() %>% pad(group = "zweit") %>%
  fill_by_value(Rel_Frequenz, 0) %>%
  group_by(zweit) %>%
  arrange(Datum) %>%
  mutate(Rel_Frequenz_rm = frollmean(Rel_Frequenz, 14, align = "center")) -> daten

p2 <- ggplot(daten, aes(x = as.Date(Datum), y = Rel_Frequenz_rm, col = zweit, group = zweit)) +
  geom_line(lwd = 1, alpha = 2/3) +
  scale_color_jcolors("pal3") +
  scale_x_date(date_breaks = "2 month", limits = as.Date(c("2020-10-01", today()))) +
  labs(x = "", y = "Relative Frequenz", col = "Impf...") +
  theme_minimal()

p <- arrangeGrob(p1, p2, ncol = 1)
ggsave("impf_komp_zeit.png", p, width = 8, height = 5, dpi = 400, bg = "white")
