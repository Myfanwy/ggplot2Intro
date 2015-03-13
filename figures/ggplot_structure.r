ggplot() + layer(
  data = diamonds, aes(x=carat, y=price), geom = “point”, stat = “identity”, position = “identity”) +
  scale_y_continuous() +
  scale_x_continuous() +
  coord_cartesian()

head(diamonds)

#Layer 1
p <- ggplot() + layer(
  data = diamonds, mapping = aes(x = carat, y = price) ,
  geom = "point", stat = "identity" , position = "identity"
)
p

# layer 2
p <- p + layer(
  data = diamonds, mapping = aes(x=carat, y = price) ,
  geom = "smooth" , position = "identity" , 
  stat = "smooth" , method = lm
)
p

# scale spec
p <- p + scale_y_log10() + scale_x_log10()

# coord spec
p + coord_polar()

# facet spec
p + facet_wrap(~color, nrow = 2)
head(diamonds)
p + facet_grid(~cut)
