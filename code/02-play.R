###############################################################################
## explore 
###############################################################################
source("code/01-clean.R")

# add 7 day rolling mean on all vars, calculate ratio
data %>% 
  mutate( across(where(is.numeric), 
                 ~ rollmean(.x, 7,na.pad = TRUE),
                 .names = "{col}_7d" )) %>% 
  mutate(ratio = infect.med/age,
         ratio_7d = infect.med/age_7d) -> data

# plot 
png(filename="figures/ratio.png", 800, 480)

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
lines(data$date, data$ratio_7d)

mtext(side = 3, line = 2.5,  adj = 0, cex = 1.2,
      "ratio of no. of infectious individuals to no. of confirmed cases")
mtext(side = 3, line = 1.5,  adj = 0, cex = 1, 
      "(line is based on 7 day rolling average of confirmed cases)")
mtext(side = 2, line = 2.5,   cex = 1, 
      "ratio of infectious to confirmed cases")
mtext(side = 1, line = 2.5,   cex = 1, "date")

dev.off()
