library(ggplot2)
ggplot(data = df_plot_utr, aes(x = year, y = mean)) +
  geom_line(data = df_plot_utr, aes(x = year, y = mean, colour = "red", group=1)) +
  geom_point(data = df_plot_ams, aes(x = year, y = mean, colour = "blue",)) +
  geom_line(data = df_plot_eid, aes(x = year, y = mean, colour = "green", group=1)) +
  scale_color_identity(name="",
                       breaks=c("red","blue","green"
                       ),
                       labels=c("Utrecht", "Amsterdam","Eindhoven"),
                       guide="legend"
  )+
  labs(y="mean", x="year") + 
  theme_minimal()

