library(likert)
library(tidyverse)
library(here)
library(readxl)

file_name = "encuesta2019.xlsx"
data_path <- here::here("data", "2019", file_name) # local file

#likert_data_raw <- read_excel(data_path)
likert_data_raw <-  
  read_excel(data_path,
    col_types = c("date", "numeric", "text", "numeric", "numeric","numeric","numeric","numeric"),
    na = c("NA",""))

likert_levels <- c(-2, -1, 0, 1, 2)
likert_labels <- c("Muy en desacuerdo [Strognly disagree]","En desacuerdo [Disagree]","Neutro [Neutral]","De acuerdo [Agree]","Muy de acuerdo [Strongly agree]")
turno_labels <- c("Primer: 10-12", "Segundo: 12-14")
#likert_data_raw$turno <- factor(likert_data_raw$turno, turno_levels, turno_labels)
likert_data_raw[4:8] <- lapply(likert_data_raw[4:8], factor, levels=likert_levels, labels=likert_labels)
likert_data_raw$turno <- sapply(X = likert_data_raw$turno, FUN = function(x) {turno_labels[x]}) 

num_respondents <- nrow(likert_data_raw)

# Update: 25 Feb 2019. 
# Flag to generate long or short name for questions. 
short_names <- TRUE

likert_data_raw_df <- as.data.frame(select(likert_data_raw, c("fecha","turno","p1","p2","p3","p4","p5")))
if (short_names) {
  names(likert_data_raw_df) <- c("Fecha", 
                                 "Turno",
                                 "P1 [Q1]", 
                                 "P2 [Q2]",
                                 "P3 [Q3]",
                                 "P4 [Q4]",
                                 "P5 [Q5]")
} else {
  names(likert_data_raw_df) <- c("Fecha", 
                                 "Turno",
                                 "La clase me ha pasado volando [The class has flown by]", 
                                 "El caso práctica ha sido útil para entender los conceptos [The practical case has been useful to understand the concepts]",
                                 "La forma de organizar el trabajo en equipo me ha gustado [I liked the way of organizing teamwork]",
                                 "Me gustaría saber mas sobre sensores y programación [I would like to know more about sensors and programming]",
                                 "Ha sido fácil seguir el desarrollo del proyecto [It has been easy to follow the development of the project]")
}

#' En caso de agrupar por "turno", https://stackoverflow.com/questions/48340901/using-likert-package-in-r-for-analyzing-real-survey-data
likert_data_df <- likert(items=likert_data_raw_df[,c(3:7)])
likert_data_df <- likert(items=likert_data_raw_df[,c(3:7)], grouping = factor(likert_data_raw_df$Fecha))

summary(likert_data_df)
       
title <- paste0("Encuesta 'Practica la UJI' 2019 [Survey 'Practica la UJI' 2019]", ", N=", num_respondents)
plotlikert <- plot(likert_data_df, centered = FALSE) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=guide_legend(title=c("Respuestas [Responses]"), nrow = 2))
plotlikert

file_name = "encuesta2019-likert.svg"
data_path <- here::here("figs", file_name) # local file
svg(filename=data_path, width=13, height=6, pointsize=10)
plotlikert
dev.off()

file_name = "encuesta2019-likert.png"
data_path <- here::here("figs", file_name) # local file
png(filename=data_path, width=850, height=480, units="px", pointsize=12)
plotlikert
dev.off()

plotheat <- plot(likert_data_df, type = 'heat') + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = 'none')
plotheat

file_name = "encuesta2019-heat.svg"
data_path <- here::here("figs", file_name) # local file
svg(filename=data_path, width=10, height=6, pointsize=10)
plotheat
dev.off()

file_name = "encuesta2019-heat.png"
data_path <- here::here("figs", file_name) # local file
png(filename=data_path, width=650, height=480, units="px", pointsize=10)
plotheat
dev.off()


