library(gapminder)
library(ggplot2)

graph0 <- ggplot(gapminder, aes(x = gdpPercap, y = lifeExp))

graph0 + geom_point()
graph0 + geom_smooth()
graph0 + geom_point() + geom_smooth(color = "blue")
graph0 + geom_point(color="white") + geom_smooth(color="lightblue") +
  theme_bw() +
  theme(
    text = element_text(
      size = 12,
      face = "bold",
      color = "white"
    ),
    legend.text = element_text(color = "white", size = 8),
    axis.text.x = element_text(size = 8, color = "white"), axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 8, color = "white"), axis.title.y = element_text(size = 16),
    plot.background = element_rect(fill = "black")
  )

graph1 <- ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, color = 'red'))

graph1 + geom_point() + 
  geom_smooth(method = "loess") + 
  scale_x_log10()

graph1 + geom_point(alpha = 0.4) + 
  geom_smooth(method = "lm", color = "darkred") + 
  scale_x_log10() +
  theme_bw() +
  scale_x_log10(labels = scales::label_number(scale = 1, accuracy = 0.1, prefix = "", suffix = "")) 
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

  
graph2 <- ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, color = continent))

graph2 + geom_point(alpha = 0.5, size = 1) + 
  geom_smooth(method = "loess") + 
  scale_x_log10(labels = scales::label_number(scale = 1, accuracy = 0.1, prefix = "", suffix = "")) +
  theme_linedraw() +
  theme(
      text = element_text(
        size = 10,
        face = "italic",
        color = "darkblue"
      ),
      legend.text = element_text(color = "darkturquoise", size = 8),
      axis.text.x = element_text(size = 8, color = "darkturquoise"), axis.title.x = element_text(size = 16),
      axis.text.y = element_text(size = 8, color = "darkturquoise"), axis.title.y = element_text(size = 16)
    )


graph3 <- ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(alpha = 0.8, size = 1, aes(color = continent)) + 
  geom_smooth(method = "loess") + 
  scale_x_log10(labels = scales::label_number(scale = 1, accuracy = 0.1, prefix = "", suffix = "")) +
  theme_linedraw()

graph3


graph4 <- ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) + 
  geom_point(color = "black") + 
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE, color = "blue", size = 2, alpha = 0.1) + 
  scale_x_log10(labels = scales::label_number(scale = 1, accuracy = 0.1, prefix = "", suffix = "")) 
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "gray80", size = 0.2))
graph4


graph5 <- ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(size = 2, aes(color = log(pop))) + 
  scale_x_log10() +
  theme_test() +
  scale_x_log10(labels = scales::label_number(scale = 1, accuracy = 0.1, prefix = "", suffix = "")) +
  theme(panel.grid.major = element_line(color = "gray80", size = 0.2),
        panel.grid.minor = element_line(color = "gray80"))

graph5



  