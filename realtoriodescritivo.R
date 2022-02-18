#Indicando Diretorio

getwd()
setwd("C:/Users/Erthal/OneDrive/Documentos/UNB/.SEM 4/METODOS 2/analise_descritiva")

#Carregando Pacotes

pacman::p_load(plyr, tidyverse, ggcorrplot, scales, stringr, cowplot, RColorBrewer, xtable, readxl,lubridate, janitor)

pacman::p_load(e1071, moments)

#Lendo os dados

bd <- read.csv("amostra_200016741.csv")

bd_codigo <- read_xlsx("relatorio_codigos.xlsx", sheet=1, col_names=TRUE)
bd_codigo$MUNICIPIO <- as.numeric(bd_codigo$MUNICIPIO)
bd_codigo <- bd_codigo[, c("MUNICIPIO", "Nome_Municipio") ]


bd_municipios <- join(bd, bd_codigo, type = "inner")


#=======FAZER SEMPRE
bd_espec$CLASS <- c("Class", "Class")

bd_espec$numero <- c("1", "2")

#======================AREA



bd_area$AREA <- c("Capital", "Interior")

bd_area$numero <- c("1", "2")


bd %>% distinct(AREA)

bd_area <- bd %>% group_by(numero) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = round((freq/sum(freq)),4))

ggplot(bd_area, aes(x=AREA, y=freq_relativa)) + 
  geom_bar(stat="identity",position='dodge', fill = "#696969") + 
  scale_y_continuous(limits = c(0,1), expand = c(0,0), breaks = seq(0,1,.1),labels = paste0(seq(0,100,10),'%')) +
  labs(x="Área (Capital e Interior)", y="Porcentagem") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")

xtable(table(bd$AREA))



#========================LOCALIZAÇÃO

bd %>% distinct(LOCALIZACAO)


bd_local <- bd %>% group_by(numero) %>%
  summarise(freq_6 = n()) %>%
  mutate(freq_relativa6 = round((freq_6/sum(freq_6)),4))





ggplot(bd_local, aes(x= LOCALIZACAO, y=freq_relativa6)) + 
  geom_bar(stat="identity",position='dodge', fill = "#696969") + 
  scale_y_continuous(limits = c(0,0.9), expand = c(0,0), breaks = seq(0,0.9,.1),labels = paste0(seq(0,90,10),'%')) +
  labs(x="Localização (Rural e Urbana)", y="Porcentagem") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")

bd_local$LOCALIZACAO <- c("Urbana", "Rural")

bd_local$numero <- c("1", "2")


#================================REGIAO

bd_regiao$REGIAO <- c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")

bd_regiao$numero <- c("1", "2","3","4","5")

bd_regiao <- bd %>% 
  group_by(numero) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = round((freq/sum(freq))*100,2))

porcentagens <- str_c(bd_regiao$freq_relativa, '%') %>%
  str_replace('\\.',',')

bd_regiao %>% 
  ggplot(aes(x=REGIAO, y=freq_relativa, label=porcentagens)) +
  geom_bar(stat="identity", fill="#A11D21") +
  geom_text(vjust = -0.5, size = 4) +
  labs(x="Região", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))


#=====================================UF
# 15,23,29,31,33,35,41,42,43

bd_uf %>% distinct(UF)


bd_uf <- bd %>% group_by(UF) %>% 
  summarise(freq_absoluta1 = n()) 

bd_uf <- bd_uf %>% na.omit() 
bd_uf <-  bd_uf %>% 
  mutate(frequencia_relativa1 = round((freq_absoluta1/sum(freq_absoluta1)),4)) %>% 
  top_n(frequencia_relativa1, n = 9)


names(bd_uf) <- c("UF", "freq", "freq_relativa", "numero")

bd_uf$UFnova <- c("PA", "CE", "BA", "MG", "RJ", "SP",
               "PR", "SC", "RS")

bd_uf$numero <- c("1", "2", "3", "4", "5", "6",
                   "7", "8", "9")

ordem1 <- c("SP","MG", "RJ", "BA", "RS", "PR", "CE", "PA","SC")

bd_uf <- bd %>% 
  group_by(numero) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = round((freq/sum(freq))*100,2))

porcentagens <- str_c(bd_uf$freq_relativa, '%') %>%
  str_replace('\\.',',')

bd_uf %>% 
  ggplot(aes(x=factor(UFnova, levels = ordem1), y=freq_relativa, label=porcentagens)) +
  geom_bar(stat="identity", fill="#A11D21") +
  geom_text(vjust = -0.5, size = 4) +
  labs(x="UF", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))


#================================ Municipios
ordem2 <- c("São Paulo", "Rio", "Belo Horizonte", "Brasília", "Manaus", "Fortaleza", "Boa Vista","Curitiba", "Guarulhos", "Campinas" )

bd_muni <- bd_municipios %>% group_by(Nome_Municipio) %>% 
  summarise(freq = n()) %>%
  mutate(freq_relativa = round((freq/sum(freq)),4)) %>% top_n(freq, n = 10) %>% arrange(desc(freq) )

bd_muni <- bd %>% 
  group_by(numero) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = round((freq/sum(freq))*100,2))

porcentagens <- str_c(bd_muni$freq_relativa*100, '%') %>%
  str_replace('\\.',',')

bd_muni %>% 
  ggplot(aes(x=factor(Nome_Municipio, levels = ordem2), y=freq_relativa, label=porcentagens)) +
  geom_bar(stat="identity", fill="#A11D21") +
  geom_text(vjust = -0.5, size = 4) +
  labs(x="Municípios", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=8.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))


bd_muni$Nome_Municipio <- c("São Paulo",      "Rio", "Belo Horizonte" ,"Brasília" ,      "Manaus"  ,       "Fortaleza"    ,  "Boa Vista" ,    
                             "Curitiba"   ,    "Guarulhos"    ,  "Campinas")

#===================================Dependencia ADM


bd_dependencia <- bd %>% group_by(DEPENDENCIA_ADM) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = round((freq/sum(freq)),4))

bd_dependencia$DEPENDENCIA_ADM <- c("Federal", "Estadual", "Municipal")

bd_dependencia$numero <- c("1", "2","3")

porcentagens <- str_c(bd_dependencia$freq_relativa*100, '%') %>%
  str_replace('\\.',',')

bd_dependencia %>% 
  ggplot(aes(x=DEPENDENCIA_ADM, y=freq_relativa, label=porcentagens)) +
  geom_bar(stat="identity", fill="#A11D21") +
  geom_text(vjust = -0.5, size = 4) +
  labs(x="Dependência Administrativa", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=8.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

#=======================================Raça Cor

bd_raca <- bd %>% group_by(RACA_COR) %>%
  summarise(freq = n())


bd_raca <- bd_raca %>% na.omit()  %>%
  mutate(freq_relativa = round((freq/sum(freq)),4))

bd_raca$RACA_COR[1] <- "NAO"
bd_raca <- bd_raca[!(bd_raca$RACA_COR=="NAO"),]

bd_raca$RACA_COR <- c("Branca", "Preta", "Parda","Amarela", "Indígena", "Não quer declarar")

bd_raca$numero <- c("1", "2","3","4","5","6")



porcentagens <- str_c(bd_raca$freq_relativa*100, '%') %>%
  str_replace('\\.',',')

bd_raca %>% 
  ggplot(aes(x=RACA_COR, y=freq_relativa, label=porcentagens)) +
  geom_bar(stat="identity", fill="#A11D21") +
  geom_text(vjust = -0.5, size = 4) +
  labs(x="Raça Cor", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=8.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))+
  ggsave("GRAFICO-RACA-1.png", width = 158, height = 93, units = "mm")


#=======================================SEXO

bd_sexo <- bd %>% group_by(SEXO) %>% 
  summarise(Freq_1 = n())  

bd_sexo$SEXO <- c("Masculino", "Femenino")

bd_sexo <-bd_sexo %>% na.omit() %>%
  mutate(Freq_relativa1 = round((Freq_1/sum(Freq_1))*100,2))

porcentagens_1<- str_c(bd_sexo$Freq_relativa1, "%") %>%
  str_replace('\\.',',')

ggplot(bd_sexo, aes(x ="", y=Freq_relativa1, fill= SEXO)) + geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0, direction = -1) + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    axis.text.x=element_blank(),
    legend.title = element_blank()) + 
  geom_text(data = bd_sexo, 
            aes(x ="", y=Freq_relativa1, label = porcentagens_1),
            position = position_stack(vjust = 0.5))+
  ggsave("GRAFICO-SEXO-1.png", width = 158, height = 93, units = "mm")

#============================================COMPUTADOR


bd_computador <- bd %>% group_by(COMPUTADOR) %>%
  summarise(freq = n()) 


bd_computador <- bd_computador %>% na.omit() %>%
  mutate(freq_relativa = round((freq/sum(freq)),4))

bd_computador$COMPUTADOR[1] <- "NAO"
bd_computador <- bd_computador[!(bd_computador$COMPUTADOR=="NAO"),]

bd_computador$COMPUTADOR <- c("Não tem", "Sim, um", "Sim, dois","Sim, três", "Sim, quatro ou mais")

bd_computador$numero <- c("1", "2","3","4","5","6")

porcentagens <- str_c(bd_computador$freq_relativa*100, '%') %>%
  str_replace('\\.',',')

ordem3 <- c("Não tem", "Sim, um", "Sim, dois","Sim, três", "Sim, quatro ou mais")

bd_computador %>% 
  ggplot(aes(x=factor(COMPUTADOR, levels = ordem3), y=freq_relativa, label=porcentagens)) +
  geom_bar(stat="identity", fill="#A11D21") +
  geom_text(vjust = -0.5, size = 4) +
  labs(x="Computador", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=8.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))+
  ggsave("GRAFICO-COMPUTADOR-1.png", width = 158, height = 93, units = "mm")


#===================================MORA MAE

bd_mae <- bd %>% group_by(MORA_MÃE) %>%
  summarise(freq = n()) 


bd_mae <- bd_mae %>% na.omit() %>%
  mutate(freq_relativa = round((freq/sum(freq)),4))

bd_mae$MORA_MÃE[1] <- "NAO"
bd_mae <- bd_mae[!(bd_mae$MORA_MÃE=="NAO"),]

bd_mae$MORA_MÃE <- c("Sim", "Não", "Não, mas mora com outra mulher responsável")

bd_mae$numero <- c("1", "2","3")

porcentagens <- str_c(bd_mae$freq_relativa*100, '%') %>%
  str_replace('\\.',',')

ordem4 <- c("Sim", "Não", "Não, mas mora com outra mulher responsável")

bd_mae %>% 
  ggplot(aes(x=factor(MORA_MÃE, levels = ordem4), y=freq_relativa, label=porcentagens)) +
  geom_bar(stat="identity", fill="#A11D21") +
  geom_text(vjust = -0.5, size = 4) +
  labs(x="Mora com a Mãe", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=8.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))+
  ggsave("GRAFICO-MORAMAE-1.png", width = 158, height = 93, units = "mm")


#===================================MORA Pai

bd_pai <- bd %>% group_by(MORA_PAI) %>%
  summarise(freq = n()) 


bd_pai <- bd_pai %>% na.omit() %>%
  mutate(freq_relativa = round((freq/sum(freq)),4))

bd_pai$MORA_PAI[1] <- "NAO"
bd_pai <- bd_pai[!(bd_pai$MORA_PAI=="NAO"),]

bd_pai$MORA_PAI <- c("Sim", "Não", "Não, mas mora com outro homem responsável")

bd_pai$numero <- c("1", "2","3")

porcentagens <- str_c(bd_pai$freq_relativa*100, '%') %>%
  str_replace('\\.',',')

ordem5 <- c("Sim", "Não", "Não, mas mora com outro homem responsável")

bd_pai %>% 
  ggplot(aes(x=factor(MORA_PAI, levels = ordem5), y=freq_relativa, label=porcentagens)) +
  geom_bar(stat="identity", fill="#A11D21") +
  geom_text(vjust = -0.5, size = 4) +
  labs(x="Mora com o Pai", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=8.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))+
  ggsave("GRAFICO-MORAPAI-1.png", width = 158, height = 93, units = "mm")

#============================================Uso tempo telas


bd_uso_telas <- bd %>% group_by(USO_TEMPO_TELAS) %>% 
  summarise(Freq_2 = n()) 

bd_uso_telas$USO_TEMPO_TELAS[1] <- "NAO"
bd_uso_telas <- bd_uso_telas[!(bd_uso_telas$USO_TEMPO_TELAS=="NAO"),]

bd_uso_telas$USO_TEMPO_TELAS <- c(" Menos de 1 Hora", "Entre 1 e 2 horas", "De 2 à 3 horas", "Mais de 3 horas", "Não usa telas")

bd_uso_telas$numero <- c("1", "2","3","4", "5")

ordem6 <- c("Não usa telas"," Menos de 1 Hora", "Entre 1 e 2 horas", "De 2 à 3 horas", "Mais de 3 horas")
bd_uso_telas <- bd_uso_telas  %>% na.omit()  %>%
  mutate(Freq_relativa2 = round((Freq_2/sum(Freq_2))*100,2))


level_order11 <- c("Mais de 3 horas", "Entre 1 e 2 horas", "Mais de 2 horas, até 3 horas",
                   "Menos de 1 hora", "Não vejo TV, não navego na internet e não jogo jogos eletrônicos" = "Não vejo TV, não navego na internet e não jogo jogos eletrônicos")




porcentagem1<- str_c(bd_uso_telas$Freq_relativa2, "%") %>%
  str_replace('\\.',',')



ggplot(bd_uso_telas, aes(x ="", y=Freq_relativa2, fill= factor(USO_TEMPO_TELAS, levels = ordem6))) + geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0, direction = -1) + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    axis.text.x=element_blank(),
    legend.title = element_blank()) + 
  geom_text(data = bd_uso_telas, 
            aes(x ="", y=Freq_relativa2, label = porcentagem1),
            position = position_stack(vjust = 0.5))+
  ggsave("GRAFICO-USOTELAS-1.png", width = 158, height = 93, units = "mm")

#======================================Afazeres

bd_afazeres <- bd %>%
  group_by(AFAZERES_DOM) %>% 
  summarise(freq_1 = n()) 

bd_afazeres$AFAZERES_DOM[1] <- "NAO"
bd_afazeres <- bd_afazeres[!(bd_afazeres$AFAZERES_DOM=="NAO"),]

bd_afazeres$AFAZERES_DOM <- c(" Menos de 1 Hora", "Entre 1 e 2 horas", "De 2 à 3 horas", "Mais de 3 horas", "Não faz trabalhos domésticos")

bd_afazeres$numero <- c("1", "2","3","4", "5")

ordem7 <- c("Não faz trabalhos domésticos"," Menos de 1 Hora", "Entre 1 e 2 horas", "De 2 à 3 horas", "Mais de 3 horas")

bd_afazeres <- bd_afazeres %>% na.omit() %>% mutate(freq_relativa_1 = round((freq_1/sum(freq_1))*100,2))

factor(AFAZERES_DOM, levels = ordem7)


porcentagem_1<- str_c(bd_afazeres$freq_relativa_1, "%") %>%
  str_replace('\\.',',')



ggplot(bd_afazeres, aes(x ="", y=freq_relativa_1, fill= factor(AFAZERES_DOM, levels = ordem7)
 )) + geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0, direction = -1) + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    axis.text.x=element_blank(),
    legend.title = element_blank()) + 
  geom_text(data = bd_afazeres, 
            aes(x ="", y=freq_relativa_1, label = porcentagem_1),
            position = position_stack(vjust = 0.5))+
  ggsave("GRAFICO-AFAZERES-1.png", width = 158, height = 93, units = "mm")


xtable(table(bd$AFAZERES_DOM))


#===================================Nota lp

ggplot(bd, aes(x=NOTA_LP)) + 
  geom_histogram(colour="white", fill="#A11D21",binwidth=7)+
  labs(x="Proeficiência em Língua Portuguesa", y="Frequência Absoluta") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

#===================================Nota mt


ggplot(bd, aes(x=NOTA_MT)) + 
  geom_histogram(colour="white", fill="#A11D21",binwidth=7)+
  labs(x="Proeficiência em Matemática", y="Frequência Absoluta") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))+
  ggsave("GRAFICO-NOTAMT-1.png", width = 158, height = 93, units = "mm")


#=================================== Idade

bd_idade <- bd %>% group_by(IDADE) %>%
  summarise(freq = n())

bd_idade$IDADE[1] <- "NAO"
bd_idade <- bd_idade[!(bd_idade$IDADE=="NAO"),]

bd_idade$IDADE <- c(" 8 anos ou menos", "9 anos", "10 anos", "11 anos", "12 anos", "13 anos" , "14 anos", "15 anos ou mais")

bd_idade$numero <- c("1", "2","3","4", "5","6","7","8")

ordem8 <- c(" 8 anos ou menos", "9 anos", "10 anos", "11 anos", "12 anos", "13 anos" , "14 anos", "15 anos ou mais")

bd_idade <- bd_idade  %>% na.omit()  %>%
  mutate(freq_relativa = round((freq/sum(freq))*100,2))

porcentagem_1<- str_c(bd_idade$freq_relativa, "%") %>%
  str_replace('\\.',',')

bd_idade %>% 
  ggplot(aes(x=factor(IDADE, levels = ordem8), y=freq_relativa, label=porcentagem_1)) +
  geom_bar(stat="identity", fill="#A11D21") +
  geom_text(vjust = -0.5, size = 4) +
  labs(x="Idade", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=8.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))+
  ggsave("GRAFICO-IDADE-1.png", width = 158, height = 93, units = "mm")


#=================================== Reunioes

bd_reunioes <- bd %>% group_by(REUNIÕES_ESCOLARES) %>%
  summarise(freq = n())

bd_reunioes$REUNIÕES_ESCOLARES[1] <- "NAO"
bd_reunioes <- bd_reunioes[!(bd_reunioes$REUNIÕES_ESCOLARES=="NAO"),]

bd_reunioes$REUNIÕES_ESCOLARES <- c("Sempre ou quase sempre", "De vez em quando", "Nunca ou quase nunca")

bd_reunioes$numero <- c("1", "2","3")

ordem9 <- c("Sempre ou quase sempre", "De vez em quando", "Nunca ou quase nunca")

bd_reunioes <- bd_reunioes  %>% na.omit()  %>%
  mutate(freq_relativa = round((freq/sum(freq))*100,2))

porcentagem_1<- str_c(bd_reunioes$freq_relativa, "%") %>%
  str_replace('\\.',',')

bd_reunioes %>% 
  ggplot(aes(x=factor(REUNIÕES_ESCOLARES, levels = ordem9), y=freq_relativa, label=porcentagem_1)) +
  geom_bar(stat="identity", fill="#A11D21") +
  geom_text(vjust = -0.5, size = 4) +
  labs(x="Frequência dos pais ou responsáveis à reuniões escolares", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=8.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))+
  ggsave("GRAFICO-REUNIOES-1.png", width = 158, height = 93, units = "mm")

#============================================Biblioteca

bd_biblioteca <- bd %>% group_by(BIBLIOTECA) %>%
  summarise(freq = n())


bd_biblioteca$BIBLIOTECA <- c("Sempre ou quase sempre", "De vez em quando", "Nunca ou quase nunca")

bd_biblioteca$numero <- c("1", "2","3")

ordem10 <- c("Sempre ou quase sempre", "De vez em quando", "Nunca ou quase nunca")

bd_biblioteca <- bd_biblioteca  %>% na.omit()  %>%
  mutate(freq_relativa = round((freq/sum(freq))*100,2))

porcentagem_1<- str_c(bd_biblioteca$freq_relativa, "%") %>%
  str_replace('\\.',',')

bd_biblioteca %>% 
  ggplot(aes(x=factor(BIBLIOTECA, levels = ordem10), y=freq_relativa, label=porcentagem_1)) +
  geom_bar(stat="identity", fill="#A11D21") +
  geom_text(vjust = -0.5, size = 4) +
  labs(x="Uso da Biblioteca/Sala de leitura", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=8.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))+
  ggsave("GRAFICO-BIBLIOTECA-1.png", width = 158, height = 93, units = "mm")


