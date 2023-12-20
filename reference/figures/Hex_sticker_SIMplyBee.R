library(ggplot2)
library(ggimage)
library(hexSticker)



p <- ggplot(data.frame(x=s_x,y=s_y,image="/Users/s2122596/Downloads/SIMplyBee_logo.png"), aes(x,y)) +
  geom_image(aes(image=image), size=s_width) + theme_void()

sticker <- hexSticker::sticker(
  subplot = p,
  package = "SIMplyBee", 
  s_x = 1.085,
  s_y = 0.965, 
  s_width = 2.75,
  s_height = 2.76,
  p_x = 1,
  p_y = 1.17,
  h_color = "orange",
  h_fill = "grey",
  h_size = 1.2,
  p_color = "black",
  p_size = 17,
  filename="/Users/s2122596/Downloads/sticker.png")
sticker




