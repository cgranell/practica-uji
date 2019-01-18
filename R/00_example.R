install.packages("likert")
library(likert)
library(tidyverse)
library(here)

file_name = "fake.csv"
data_path <- here::here("data", file_name) # local file

likert_data_raw <-  
  read_csv(data_path,
    col_types = cols(
      fecha = col_character(),
      turno = col_integer(),
      curso = col_character(),
      p1 = col_character(),
      p2 = col_character(),
      p3 = col_character(),
      p4 = col_character(),
      p5 = col_character()
    ),
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
likert_data_df <- likert(items=likert_data_raw_df[,c(2:6)], grouping = order(likert_data_raw_df$Turno))


title <- "'Ejemplo Likert con datos simulados'"
plot(likert_data_df, centered = TRUE) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=guide_legend(title=NULL, nrow = 1))

