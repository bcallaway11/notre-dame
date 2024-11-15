# Minimum wage code, acknowledging variation in amount of minimum wage changes

load("data2.RData")
library(dplyr)
library(ggplot2)

treated_state_list <- unique(subset(data2, G != 0)$state_name)

plot_df <- unique(select(subset(data2, G != 0), state_name, state_mw, year))
ggplot(plot_df, aes(x = year, y = state_mw, color = state_name)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.5) +
  ylim(c(5.15, 7.00)) +
  theme_bw()

load("post_res.RData")

ggplot(post_res, aes(x = year, y = attst, color = state)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.5) +
  ylim(c(-.2, .05)) +
  theme_bw()



ggplot(post_res, aes(x = year, y = attst.per, color = state)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.5) +
  ylim(c(-.2, .05)) +
  theme_bw()


es_att <- c()
es_se <- c()
for (e in 0:3) {
  this_res <- subset(post_res, event_time == e)
  es_att[e + 1] <- weighted.mean(this_res$attst.per, this_res$state_size)
  es_se[e + 1] <- weighted.mean(this_res$attst.per.se, this_res$state_size)
}

ggplot(
  data.frame(es_att = es_att, es_se = es_se, e = 0:3),
  aes(x = e, y = es_att)
) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(size = 1.5, color = "steelblue") +
  geom_line(aes(y = es_att + 1.96 * es_se), linetype = "dashed") +
  geom_line(aes(y = es_att - 1.96 * es_se), linetype = "dashed") +
  ylim(c(-.2, .05)) +
  theme_bw()
