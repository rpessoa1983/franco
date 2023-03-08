 
library(ggplot2)
library(jpeg)
library(png)
library(grid)
library(ggimage)


img <- readJPEG('figure/franco_paredebege.jpg')
img2 <- readPNG('figure/DALL·E 2023-03-08 14.39.35 - Uma foto de Marielle Franco na Plenária com fundo de cor Sólida.png')

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
    labs(caption = "Fonte: Rede de Observatórios de Segurança")+
  ggtitle('Tipos de feminicídio e violência contra mulher - Bahia (Jun/2019-Mai/2021)')+
    theme(plot.title = element_text(hjust = -1, vjust = -80))+
     theme(title = element_text(size = 11,colour = "white"))+
    theme(axis.title.x=element_text(size=11)) +
    theme(axis.title.y=element_text(size=11)) +
    theme(axis.text=element_text(color="#3B2F53",size =12))+
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
          panel.grid.minor = element_line(color = NA, size = .25))+
    geom_text(aes(label = obs), size = 3, hjust = 1, vjust = 1, position =     "stack") 
  #+
  #theme_nothing()
  )


base +
  annotation_custom(grob = g, xmin = 5, xmax = Inf, ymin = 1, ymax = 5)


#######################################################################
#DALL-E-2



base2 <- ggplot(data = df, aes(x = obs, y = motivo)) +
  annotation_custom(rasterGrob(img2, 
                               width = unit(1,"npc"),
                               height = unit(1,"npc")), 
                    -Inf, Inf) +
  geom_blank() +
  theme_nothing()


g2 <- ggplotGrob(ggplot(data = df, aes(x = obs, y = reorder(motivo,obs))) +
                  geom_bar(stat = 'identity',color='white',fill='white') +
                  xlab(" ") +
                  ylab(" ") + 
                  labs(caption = "Fonte: Rede de Observatórios de Segurança")+
                  ggtitle('Tipos de feminicídio e violência contra mulher - Bahia (Jun/2019-Mai/2021)')+
                  theme(plot.title = element_text(hjust = -3, vjust = -75))+
                  theme(title = element_text(size = 11,colour = "white"))+
                  theme(axis.title.x=element_text(size=11)) +
                  theme(axis.title.y=element_text(size=11)) +
                  theme(axis.text=element_text(color="#3B2F53",size =12))+
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
                        panel.grid.minor = element_line(color = NA, size = .25))+
                  geom_text(aes(label = obs), size = 3, hjust = 1, vjust = 1, position =     "stack") 
                #+
                #theme_nothing()
)


base2 +
  annotation_custom(grob = g2, xmin = 1, xmax = Inf, ymin = 1, ymax = 5)






