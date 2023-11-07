##############################################
############    MAPA Y BRUJULA    ############

work.space <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(work.space)

options(scipen = 999)
options(stringsAsFactors = FALSE)

library(dplyr)
library(tidyr)
library(viridis)
library(ggplot2)
library(ggridges)
library(scales)



#--- IMPORTO LOS DATOS ----
FN <- read.delim("2119.csv", sep=",") %>%
  select(FL1.A, FL4.A) %>% rename(NKX2.5 = FL1.A, TNNT2 = FL4.A)



#---- THEME SET ----
theme_set(theme_bw()+
  theme(axis.title.y=element_text(size = 12),
        axis.text.y=element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)))

#---- FN2.1 wild type ----

TEXTO <- data.frame(NKX2.5 = c(100,8000000,100,8000000),
                    TNNT2 = c(4000000,4000000,300,300),
                    Label = c('TNNT+/NKX2.5-','TNNT+/NKX2.5+','TNNT-/NKX2.5-','TNNT-/NKX2.5+'))

my_pal <- colorRampPalette(c('#5e4fa2ff','#4d71b2ff','#358abcff',
                             '#5db2acff','#7ecaa5ff','#ddf19aff','#fff9b6ff','#fed27fff','#fca65dff',
                             '#f7824cff','#ed6346ff','#ca354dff','#9e0142ff'))(250)
my_pal2 <- colorRampPalette(c('gray70','gray40','gray20'))(250)


# - - - DEL WILD TYPE - - - - - - - - - - - - - - - - 

pdf("2119.pdf", height =4, width =4.75)
ggplot(FN, aes(x = TNNT2, y= NKX2.5))+
  geom_hex(binwidth = 0.05, alpha = 0.9)+
  guides(fill = guide_colourbar(barwidth = 0.7, barheight = 15))+
  scale_y_log10(labels = trans_format("log10", math_format(10^.x)))+
  scale_x_log10(labels = trans_format("log10", math_format(10^.x)))+
  geom_text(data= TEXTO,aes(x=TNNT2, y = NKX2.5, label= Label )) +
  scale_color_gradientn(colors = my_pal) +
  scale_fill_gradientn(colors = my_pal) +
  coord_cartesian(xlim=c(100,10000000),ylim=c(100,10000000))+
  geom_vline(xintercept=100000,linetype="dashed",linewidth = 0.3, color = "gray35")+
  geom_hline(yintercept=100000,linetype="dashed",linewidth = 0.3, color = "gray35")
dev.off()




