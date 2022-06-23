packages <- c("data.table","tidyverse","skimr","here",
              "VGAM","survival","sandwich","lmtest")

for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos='http://lib.stat.cmu.edu/R/CRAN')
  }
}

for (package in packages) {
  library(package, character.only=T)
}

thm <- theme_classic() +
  theme(
    legend.position = "top",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)

eager_base_imputed <- read_csv(here("data","eager_base_imputed.csv"))

eager_base_imputed <- eager_base_imputed %>% 
  mutate(ptb = if_else(outcome=="live birth"&GA<37&GA>25,1,0),
         outcome = if_else(outcome == "efuwp","No Detected Pregnancy", outcome),
         time = if_else(outcome != "live birth",
                        difftime(final_date,
                                 randomization_date,
                                 units = "weeks"),
                        difftime(final_date,
                                 conception_date,
                                 units = "weeks")+GA))

eager_base_imputed %>% count(ptb)

eager_base_imputed %>% count(outcome)

ggplot(eager_base_imputed) + 
  geom_histogram(aes(GA,fill=outcome),bins=60) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  geom_vline(xintercept = 37, linetype=2, color="red") +
  ylab("Count") + xlab("Gestational Age")

ggplot(eager_base_imputed) + 
  geom_histogram(aes(time,fill=outcome),bins=60) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  ylab("Count") + xlab("Time on Study")

ggplot(eager_base_imputed) + 
  geom_point(aes(time,GA,color=outcome)) +
  scale_x_continuous(expand=c(0.1,0.1)) +
  scale_y_continuous(expand=c(0.1,0.1)) +
  xlab("Time on Study") + ylab("Gestational Age")
  

livebirth <- subset(eager_base_imputed, outcome == "live birth")

livebirth_1 <- subset(eager_base_imputed, outcome == "live birth" | outcome == "withdrawal") 

table(livebirth$outcome)
table(livebirth_1$outcome)

table(livebirth$ptb,livebirth$treatment)

## Analysis 1: cumulative risk of ptb in live birth cohort 
## with time since randomization as timescale

surv_model1 <- survfit(
  Surv(time = time, 
       event = ptb) ~ 1, 
  data = subset(livebirth, treatment==1))
surv_model0 <- survfit(
  Surv(time = time, 
       event = ptb) ~ 1, 
  data = subset(livebirth, treatment==0))

plot_dat1 <- tibble(time = surv_model1$time, 
                    risk = 1 - surv_model1$surv,
                    treatment=1)
plot_dat0 <- tibble(time = surv_model0$time, 
                    risk = 1 - surv_model0$surv,
                    treatment=0)

plot_dat <- rbind(plot_dat0,plot_dat1)

plot1 <- ggplot(plot_dat) + 
  scale_y_continuous(expand = c(0,0), limits = c(0, .25)) + 
  scale_x_continuous(expand = c(0,0), limits = c(55,90)) + 
  ylab("Cumulative Risk") + 
  xlab("Week on Study") + 
  geom_step(aes(x = time, y = risk, color=factor(treatment)))
plot1

## adding censoring

table(livebirth_1$ptb,livebirth_1$outcome)

surv_model1 <- survfit(
  Surv(time = time, 
       event = ptb) ~ 1, 
  data = subset(livebirth_1, treatment==1))
surv_model0 <- survfit(
  Surv(time = time, 
       event = ptb) ~ 1, 
  data = subset(livebirth_1, treatment==0))

plot_dat1 <- tibble(time = surv_model1$time, 
                    risk = 1 - surv_model1$surv,
                    treatment=1)
plot_dat0 <- tibble(time = surv_model0$time, 
                    risk = 1 - surv_model0$surv,
                    treatment=0)

plot_dat <- rbind(plot_dat0,plot_dat1)

plot2 <- ggplot(plot_dat) + 
  scale_y_continuous(expand = c(0,0), limits = c(0, .25)) + 
  scale_x_continuous(expand = c(0,0), limits = c(55,90)) + 
  ylab("Cumulative Risk") + 
  xlab("Week on Study") + 
  geom_step(aes(x = time, y = risk, color=factor(treatment)))
plot2