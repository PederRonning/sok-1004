load(url("https://bit.ly/2YBntjg"))

library(tidyverse)
library(gglorenz)
library(ineq)

billionaires %>%
  ggplot(aes(TNW)) +
  stat_lorenz(desc = FALSE) +
  coord_fixed() +
  geom_abline(linetype = "dashed") +
  theme_minimal() +
  labs(x = "Cumulative Percentage of the Top 500 Billionaires",
       y = "Cumulative Percentage of Total Net Worth",
       title = "Inequality Among Billionaires",
       caption = "Source: https://www.bloomberg.com/billionaires/ (accessed February 8, 2018)")

inntekt <- skattetall %>%
  arrange(inntekt)

inntekt %>% 
  ggplot(aes(inntekt)) +
  stat_lorenz(desc = FALSE) +
  coord_fixed() +
  geom_abline(linetype = "dashed") +
  theme_minimal() +
  labs(x = "Befolkning",
       y = "Inntekt",
       title= "Gini for inntekt i Troms (2015)") +
  annotate_ineq(inntekt$inntekt)

befolkning <- inntekt %>% 
  summarise(n=n())

tibble(
  "Fylke" = c("Troms"),
  "Gini" = ineq(inntekt$inntekt)*100,
  "inntekt" = mean(skattetall$inntekt),
  "Befolkning" = c(befolkning$n)
)
# oppgave b)
# bruk koden recode, eller if else
# summerise per gruppe

skattetall1 <- skattetall %>% 
  mutate(kommune=kommnr)
skattetall1 <- skattetall1 %>% 
  mutate(kommune=recode(kommune,
                        "1902" = "Tromsø",
                        "1903" = "Harstad",
                        ))
skattetall1[is.na(skattetall1)] <- "Omegn"
gini.kommune <- skattetall1 %>% 
  group_by(kommune) %>% 
  summarise(gini=ineq(inntekt))
inntekt <- skattetall1 %>% 
  group_by(kommune) %>% 
  summarise(mean(inntekt))
befolkning <- skatetall1 %>% 
  group_by(kommune) %>% 
  summarise(n=n())
pros_bef <- skattetall1 %>% 
  group_by(kommune) %>% 
  summarise( percent = 100*n()/nrow(skattetall1))

# oppgave c)
# bruk funksjonen mutate. lag ny variabel
#fjerne alle negative tall, og sette de som 0
#Mange økonomer mener at det blir mer riktig å beregne Gini på disponibel inntekt
#dvs inntekt fratrukket skatt. Beregn denne størrelsen.
#Dersom skatt er større enn inntekt vil disponibel inntekt bli negativ.
#Sett alle negative verdier lik 0. Lag en Lorenz-kurve for disponibel inntekt i
#Troms i 2015 der Gini indeksen vises i figuren. Kommenter dine funn,
#og sammenlign dem med det du fant i a).

skattetall1 <- skattetall1 %>% 
  mutate(disponibel_inntekt=inntekt-skatt) 
skattetall1[skattetall1 < 0] <- 0

disponibel_inntekt <- skattetall1 %>% 
  arrange(disponibel_inntekt)

disponibel_inntekt %>% 
  ggplot(aes(disponibel_inntekt)) +
  stat_lorenz(desc = FALSE) +
  coord_fixed() +
  geom_abline(linetype = "dashed") +
  theme_minimal() +
  labs(x = "Befolkning",
       y = "Disponibel inntekt",
       title= "Gini for inntekt i Troms (2015)") +
  annotate_ineq(disponibel_inntekt$disponibel_inntekt)

# oppgave d)
#ggplot. geompoint. tenke over hvordan plottet blir til slutt. (oversiktelig)

inntekt <- skattetall %>%
  arrange(inntekt)

inntekt %>% 
  ggplot(aes(x=skatt, y=inntekt)) +
  geom_point(aes(y=inntekt), color="dark red") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(title="Inntekt og Skatt",
       x="Skatt",
       y="Inntekt") +
  theme_bw()

# oppgave e)
# ggplot

Formue <- skattetall %>%
  arrange(formue)

Formue %>% 
  ggplot(aes(x=skatt, y=formue)) +
  geom_point(aes(y=formue), color="dark red") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(title="Inntekt og Formue",
       x="Skatt",
       y="Formue") +
  theme_bw()

# oppgave f)

tromso <- skattetall %>%
  filter(kommnr==1902)
harstad <- skattetall %>% 
  filter(kommnr==1903)
omegn <- skattetall %>% 
  filter(kommnr>=1904)

for_omegn <- mean(omegn$formue)
for_tromso <- mean(tromso$formue)
for_harstad <- mean(harstad$formue)
for_troms <- mean(skattetall$formue)
gini_for_omegn <- ineq(omegn$formue)
gini_for_tromso <- ineq(tromso$formue)
gini_for_harstad <- ineq(harstad$formue)
gini_for_troms <- ineq(skattetall$formue)
per_omegn <- omegn %>% 
  summarise(n=n())
per_tromso <- tromso %>% 
  summarise(n=n())
per_harstad <- harstad %>% 
  summarise(n=n())
pros_bef <- skattetall1 %>% 
  group_by(kommune) %>% 
  summarise(percent = 100*n()/nrow(skattetall1))
personer <- skattetall1 %>% 
  summarise(n=n())
pros_total <- personer %>% 
  group_by(n) %>% 
  summarise(percent = 100*n()/nrow(personer))
tibble(
  "kommune" = c("Harstad", "Tromsø", "Omegn", "Troms"),
  "Gini (%)" = c(gini_for_harstad*100, gini_for_tromso*100, gini_for_omegn*100, gini_for_troms*100),
  "Formue" = c(for_harstad, for_tromso, for_omegn, for_troms),
  "Personer" = c(per_tromso$n, per_harstad$n, per_omegn$n, personer$n),
  "Andel (%)" = c(pros_bef$percent, pros_total$percent))


# Oppgacve g)
# variabel aldersgruppe. beregne gini per aldersgruppe. lage et plott med gini per aldersgruppe. 
# 1. bruke groupe by. 2. summerise

aldersgruppe <- skattetall %>% 
  group_by(aldersgruppe) %>% 
  summarise(gini_aldersgruppe=ineq(inntekt))

aldersgruppe %>% 
  ggplot(aes(x=aldersgruppe, y=gini_aldersgruppe)) +
  geom_point(size=3, color="dark red") + 
  geom_segment(aes(x=aldersgruppe, xend=aldersgruppe, y=0, yend=gini_aldersgruppe)) +
  labs(title="Inntekt og Formue",
       x="Aldersgruppe",
       y="Gini") +
  theme_bw()

# Oppgave h)

Menn <- skattetall %>%
  filter(kjonn=="M")
Kvinner <- skattetall %>% 
  filter(kjonn=="F")

aldersgruppe_menn <- Menn %>% 
  group_by(aldersgruppe) %>% 
  summarise(gini_aldersgruppe=ineq(inntekt))
aldersgruppe_kvinner <- Kvinner %>% 
  group_by(aldersgruppe) %>% 
  summarise(gini_aldersgruppe=ineq(inntekt))


ggplot(NULL, aes(x= aldersgruppe, y=gini_aldersgruppe)) + 
  geom_bar(data=aldersgruppe_menn, aes(color='Menn'), stat = "identity",
           width=.4, position=position_nudge(x = -0.2)) +
  geom_bar(data=aldersgruppe_kvinner, aes(color='Kvinner'), stat = "identity", 
           width=.4, position=position_nudge(x = 0.2)) +
  labs(title="Gini & Aldersgruppe",
       x="Aldersgruppe",
       y="Gini")
