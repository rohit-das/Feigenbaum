

npoints = 2000
inc = 0.0005

dat = NULL
y = c(1:npoints)

for(l in seq(3.543,4,by = inc)){
  x = 0.45
  for(i in 1:100){
    x = l*x*(1-x)
  }
  y[1] = x
  for(i in 2:npoints){
     y[i] = l*y[i-1]*(1-y[i-1])
  }
  dat = rbind(dat,data.frame(l,y))
}


# npoints = 8
# inc = 0.005
# 
# dat = NULL
# y = c(1:npoints)
# 
# for(l in seq(3.543,4,by = inc)){
#   x = 0.45
#   for(i in 1:100){
#     x = l*x*(1-x)
#   }
#   y = c(1:npoints)
#   y[1] = x
#   for(i in 2:npoints){
#     y[i] = l*y[i-1]*(1-y[i-1])
#   }
#   dat = rbind(dat,data.frame(l,y))
#   npoints = min(1000,unique(round(npoints,5))*2)
# }


library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(viridis) 

ggplot(dat, aes(l, y)) + 
  geom_point(alpha = 0.2, size = 0.01, color = l)
  
# saveRDS(dat,file = "fageinbaumData.rdata")
setwd("~/Documents/Analysis")
dat <- readRDS(file = "fageinbaumData.rdata")
ggplot(dat, aes(x=l, y=y)) + geom_point()
  geom_point(alpha = 0.2, size = 0.01, color = l)

# jpeg('fageinbaunPlot1.jpg')
ggplot(dat, aes(l, y)) + 
  geom_point(alpha = 0.2, size = 0.01, color = l) +
  ggsave("fageinbaunPlot1.jpg")

ggplot(dat, aes(l, y)) + 
  geom_point(alpha = 0.15, size = 0.005, color = l) +
  ggsave("fageinbaunPlot2.png",width = 11, height = 8, units = "in")
# dev.off()

fig <- image_graph(width = 2000, height = 1500, res = 96, clip = FALSE)
ggplot2::ggplot(dat %>% sample_frac(1),aes(l, y)) + 
  geom_point(aes(color = as.factor(l)),alpha = 0.15,size = 0.005) + theme_void() + theme(legend.position="none") +
  scale_color_viridis(discrete = TRUE, option = "D")
  # ggsave("fageinbaunPlot3.png",scale = 2, width = 22, height = 16, units = "in")
dev.off()
print(fig)
out <- image_resize(fig, "800x",'Lagrange')
print(out) 
image_write(out, path = "fageinbaunPlot3.png", format = "png")

out <- image_resize(fig, "2000x",'Cosine')
image_write(out, path = "fageinbaunPlot3_large.png", format = "png")

 
 library(magick)
 img = image_read("fageinbaunPlot3.png")


