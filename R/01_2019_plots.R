install.packages("likert")
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
likert_labels <- c("Muy en desacuerdo","En desacuerdo","Neutro","De acuerdo","Muy de acuerdo")
turno_labels <- c("Primer: 10-12", "Segundo: 12-14")
#likert_data_raw$turno <- factor(likert_data_raw$turno, turno_levels, turno_labels)
likert_data_raw[4:8] <- lapply(likert_data_raw[4:8], factor, levels=likert_levels, labels=likert_labels)
likert_data_raw$turno <- sapply(X = likert_data_raw$turno, FUN = function(x) {turno_labels[x]}) 


likert_data_raw_df <- as.data.frame(select(likert_data_raw, c("turno","p1","p2","p3","p4","p5")))
names(likert_data_raw_df) <- c("Turno",
                           "1. Pregunta 1", 
                           "2. Pregunta 2",
                           "3. Pregunta 3",
                           "4. Pregunta 4",
                           "5. Pregunta 5")


#' En caso de agrupar por "turno", https://stackoverflow.com/questions/48340901/using-likert-package-in-r-for-analyzing-real-survey-data
likert_data_df <- likert(items=likert_data_raw_df[,c(2:6)])
likert_data_df <- likert(items=likert_data_raw_df[,c(2:6)], grouping = likert_data_raw_df$Turno)


title <- "Encuesta 'Practica la UJI' 2019"
plot(likert_data_df, centered = TRUE) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=guide_legend(title=c("Respuestas"), nrow = 1))

