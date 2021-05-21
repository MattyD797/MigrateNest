
library(kableExtra)

State <- c("State 1", "State 2", "State 3")
distance <- c("40m", "1300m", "57km")
distance.CI <- c("39-41m", "128-138km", "55-60km")
angle <- c("177°", "166°", "0.8°")
angle.CI <- c("173-180°", "140-192°", "-1.8-3.4°")
beh <- c("chick brooding, foraging", "local flights", "migratory")
des <- c("circuitous/short", "local/semidirected", "distant/straight")
Table <- data.frame(cbind(distance, distance.CI, angle, angle.CI, beh, des))
colnames(Table)<-c("Distance", "Distance 95% CI", "Angle", "Angle 95% CI", "Behavior", "Description")
Tab <- rbind(State, t(Table))
rownames(Tab)[1] <- c("")
kable(Tab) %>%
  kable_styling(html_font = "Georgia", bootstrap_options = "striped")%>%
  column_spec(1, bold = F, background = "#d7f7d2")%>%
  row_spec(1, bold = T, background = "#eaeaea")
