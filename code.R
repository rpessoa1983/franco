 
library(ggplot2)
library(jpeg)
library(grid)
library(ggimage)


img <- readJPEG('figure/franco_paredebege.jpg')


#tempo <- c(1:12)
#obs <- rpois(12,12)

#df <- data.frame(tempo,obs)


# Dados 

motivo<- c("Tentativa de feminicídio/Agressão física",
"Feminicídio",
"Violência sexual/estupro",
"Homicídio",
"Agressão verbal/Ameaça",
"Tortura/Cárcere privado/Sequestro",
"Tentativa de homicídio",
"Bala perdida",
"Outros")


obs <- c(133,
         128,
         51,
         148,
         8,
         12,
         22,
         4,
         2)

df <- data.frame(motivo,obs)

library(dplyr)
library(magrittr)

df %<>% arrange(-obs)



base <- ggplot(data = df, aes(x = obs, y = motivo)) +
  annotation_custom(rasterGrob(img, 
                               width = unit(1,"npc"),
                               height = unit(1,"npc")), 
                    -Inf, Inf) +
  geom_blank() +
  theme_nothing()
  

g <- ggplotGrob(ggplot(data = df, aes(x = obs, y = reorder(motivo,obs))) +
  geom_bar(stat = 'identity',color='white',fill='white') +
  xlab(" ") +
  ylab(" ") + 
    theme(title = element_text(size = 12,colour = "white"))+
    theme(axis.title.x=element_text(size=11)) +
    theme(axis.title.y=element_text(size=11)) +
    theme(axis.text=element_text(face="bold", color="#3B2F53",size =12))+
    ## ALTERACAO BACKGROUD DA AREA DE PLOTAGEM  - PARTE EXTERNA
    theme(plot.background = element_rect(fill = NA,color = NA))+
    ## ALTERACAO BACKGROUD DA AREA DE PLOTAGEM  - PARTE INTERNA
    theme(panel.grid = element_blank(), 
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank())+
    theme(panel.border = element_rect(fill=NA, color = NA, size = 2))+
    theme(panel.background = element_rect(fill=NA,color = NA, size = 2),
          panel.grid.major = element_line(color = NA, size = .5),
          panel.grid.minor = element_line(color = NA, size = .25))  
  #+
  #theme_nothing()
  )


base +
  annotation_custom(grob = g, xmin = 10, xmax = Inf, ymin = 1, ymax = 5)
