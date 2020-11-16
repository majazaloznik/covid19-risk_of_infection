###############################################################################
## explore 
###############################################################################
source("code/01-clean.R")

# add 7 day rolling mean on all vars, calculate ratio
data %>% 
  mutate( across(where(is.numeric), 
                 ~ rollmean(.x, 7,na.pad = TRUE),
                 .names = "{col}.7d" )) %>% 
  mutate(ratio = infect.med/confirmed,
         ratio.7d = infect.med/confirmed.7d,
         lag.infect.med = lag(infect.med, 11),
         lag.ratio = lag.infect.med/confirmed ,
         lag.ratio.7d = lag.infect.med/confirmed.7d,
         lag.inv.ratio = confirmed/lag.infect.med ,
         lag.inv.ratio.7d = confirmed.7d/lag.infect.med) -> data


# plot ratio of infectious to confirmed 11 days later
###############################################################################

# png(filename="figures/ratio.png", 800, 480)
plot(data$date, data$ratio, 
     ylab = "",
     xlab = "", 
     ylim = c(0,80),
     xlim = as.Date(c("2020-09-07", "2020-11-08")),
     axes = FALSE,
     panel.first={
       abline(v = as.Date("2020-10-26"), lty = "92", col = "darkgrey")
     })

axis.Date(1, data$date, at = seq(min(data$date), max(data$date), by="7 day"))
axis(2)
lines(data$date, data$ratio.7d)

mtext(side = 3, line = 2.5,  adj = 0, cex = 1.2,
      "ratio of no. of infectious individuals (modelled) to no. of confirmed cases (hard data)")
mtext(side = 3, line = 1.5,  adj = 0, cex = 1, 
      "(line is based on 7 day rolling average of confirmed cases)")
mtext(side = 2, line = 2.5,   cex = 1, 
      "ratio of infectious to confirmed cases")
mtext(side = 1, line = 2.5,   cex = 1, "date")
# dev.off()


# https://sledilnik.slack.com/archives/CV9JJCE67/p1605030090393100
# plot ratio of infectious to confirmed 11 days later
###############################################################################

# png(filename="figures/ratio.lagged.png", 800, 480)
plot(data$date, data$lag.ratio, 
     ylab = "",
     xlab = "", 
     ylim = c(0,80),
     xlim = as.Date(c("2020-09-07", "2020-11-08")),
     axes = FALSE,
     panel.first={
       abline(v = as.Date("2020-10-26"), lty = "92", col = "darkgrey")
     })

axis.Date(1, data$date, at = seq(min(data$date), max(data$date), by="7 day"))
axis(2)
lines(data$date, data$lag.ratio.7d)

mtext(side = 3, line = 2.5,  adj = 0, cex = 1.2,
      "ratio of no. of infectious individuals to no. of confirmed cases 11 days later")
mtext(side = 3, line = 1.5,  adj = 0, cex = 1, 
      "(line is based on 7 day rolling average of confirmed cases)")
mtext(side = 2, line = 2.5,   cex = 1, 
      "ratio of infectious to confirmed cases")
mtext(side = 1, line = 2.5,   cex = 1, "date")
# dev.off()



# plot ratio of  confirmed cases to infectious 11 days earlier 
###############################################################################

# png(filename="figures/ratio.lagged.inv.png", 800, 480)
plot(data$date, data$lag.inv.ratio, 
     ylab = "",
     xlab = "", 
     ylim = c(0,1),
     xlim = as.Date(c("2020-09-07", "2020-11-08")),
     axes = FALSE,
     panel.first={
       abline(v = as.Date("2020-10-26"), lty = "92", col = "darkgrey")
     })

axis.Date(1, data$date, at = seq(min(data$date), max(data$date), by="7 day"))
axis(2)
lines(data$date, data$lag.inv.ratio.7d)

mtext(side = 3, line = 2.5,  adj = 0, cex = 1.2,
      "ratio of confirmed cases to infectious individuals 11 days earlier")
mtext(side = 3, line = 1.5,  adj = 0, cex = 1, 
      "(line is based on 7 day rolling average of confirmed cases)")
mtext(side = 2, line = 2.5,   cex = 1, 
      "ratio of confirmed cases to infectious ind.")
mtext(side = 1, line = 2.5,   cex = 1, "date")
# dev.off()

