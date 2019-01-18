install.packages("likert")
library(likert)
library(tidyverse)
library(here)
library(readxl)
library(lubridate)

file_name = "encuesta2019.xlsx"
data_path <- here::here("data", "2019", file_name) # local file

#likert_data_raw <- read_excel(data_path)

likert_data_raw <-  
  read_excel(data_path,
    col_types = c("date", "numeric", "text", "numeric", "numeric","numeric","numeric","numeric"),
    na = c("NA",""))

likert_levels <- c(-2, -1, 0, 1, 2)
likert_labels <- c("Muy en desacuerdo","En desacuerdo","Neutro","De acuerdo","Muy de acuerdo")
turno_labels <- c("Primer: 10-12", "Segundo: 12-14")
#likert_data_raw$turno <- factor(likert_data_raw$turno, turno_levels, turno_labels)
likert_data_raw[4:8] <- lapply(likert_data_raw[4:8], factor, levels=likert_levels, labels=likert_labels)
likert_data_raw$turno <- sapply(X = likert_data_raw$turno, FUN = function(x) {turno_labels[x]}) 


likert_data_raw_df <- as.data.frame(select(likert_data_raw, c("fecha","turno","p1","p2","p3","p4","p5")))
names(likert_data_raw_df) <- c("Fecha", 
                               "Turno",
                               "La clase me ha pasado volando", 
                               "El caso práctica ha sido útil para entender los conceptos",
                               "La forma de organizar el trabajo en equipo me ha gustado",
                               "Me gustaría saber mas sobre sensores y programación",
                               "Ha sido fácil seguir el desarrollo del proyecto")

#' En caso de agrupar por "turno", https://stackoverflow.com/questions/48340901/using-likert-package-in-r-for-analyzing-real-survey-data
likert_data_df <- likert(items=likert_data_raw_df[,c(3:7)])
likert_data_df <- likert(items=likert_data_raw_df[,c(3:7)], grouping = factor(likert_data_raw_df$Fecha))

summary(likert_data_df)
       
title <- "Encuesta 'Practica la UJI' 2019"
plot(likert_data_df, centered = FALSE) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=guide_legend(title=c("Respuestas"), nrow = 1))


file_name = "encuesta2019.svg"
data_path <- here::here("figs", file_name) # local file
svg(filename=data_path, width=6.5, height=3, pointsize=10)
plot(likert_data_df, centered = FALSE) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) + 
    guides(fill=guide_legend(title=c("Respuestas"), nrow = 1))
dev.off()


plot(likert_data_df, type = 'heat') + ggtitle(title) + 
  theme(legend.position = 'none')

