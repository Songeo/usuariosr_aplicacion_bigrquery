

library(tidyverse)
library(bigrquery)

project <- "acoustic-field-186719" # put your project ID here



# nacimientos data ----
query_nacims <- lapply(2010:2016, function(year.nom){
  print(year.nom)
  sql <- paste0("SELECT  COUNT(EDAD_MADN), ANO_REG, EDAD_MADN", 
                ", EDAD_PADN FROM [acoustic-field-186719:nacimientos.nacim", 
                year.nom,
                "] group by ANO_REG, EDAD_MADN, EDAD_PADN")
  tt <- query_exec(sql, project = project)
}) 

df_nacims <- query_nacims %>% 
  bind_rows() %>% 
  as.tibble() %>% 
  rename(count = f0_, ANIO_REG = ANO_REG)  %>% 
  mutate(EDAD_MADN = ifelse(EDAD_MADN == 99, NA, EDAD_MADN),
         EDAD_PADN = ifelse(EDAD_PADN == 99, NA, EDAD_PADN),
         edad_gpo_madre = cut(ifelse(EDAD_MADN == 99, NA, EDAD_MADN), 
                              right = F,
                              breaks = seq(10, 100, by = 2), 
                              include.lowest = T),
         edad_gpo_padre = cut(ifelse(EDAD_PADN == 99, NA, EDAD_PADN), 
                              right = F,
                              breaks = seq(12, 80, by = 5), 
                              include.lowest = T), 
         edad_gpo_madre = fct_explicit_na(edad_gpo_madre, na_level = "(No registrado)"),
         edad_gpo_padre = fct_explicit_na(edad_gpo_padre, na_level = "(No registrado)"))
df_nacims %>% head

tab <- df_nacims %>% 
  filter(EDAD_MADN < 50,
         EDAD_MADN >= 10) %>%
  group_by(edad_gpo_madre, edad_gpo_padre) %>% 
  summarise(n_acum = sum(count)) %>% 
  group_by(edad_gpo_madre) %>% 
  mutate( prop = 100*n_acum/sum(n_acum)) %>% 
  ungroup
tab

col.gradient.cut <- c(colorRampPalette(c("#9999ff", "yellow", "#E67400"))(13), 
                      "gray50")
tab %>% 
  ggplot(aes(x = edad_gpo_madre, 
             y = prop, 
             fill= edad_gpo_padre)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = col.gradient.cut) + 
  ylab("Porcentaje (%)") + 
  xlab("Edad de la madre") +
  guides(fill = guide_legend("Edad del\npadre")) +
  ggtitle("Proporción de nacims. por edad de la madre \ndado edad de la madre") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90)) 


# public data ----
# 137,826,763 renglones

query_public <- "SELECT count(*), mother_age, father_age FROM [publicdata:samples.natality] GROUP BY mother_age, father_age;"

data <- query_exec(query = query_public, project = project)
tbl_data <- data %>% 
  as.tibble() %>% 
  dplyr::rename(count = f0_) %>% 
  mutate(edad_gpo_madre = cut(mother_age,
                              right = F,
                              breaks = seq(10, 100, by = 2), 
                              include.lowest = T),
         edad_gpo_padre = cut(father_age, 
                              right = F,
                              breaks = seq(12, 80, by = 5), 
                              include.lowest = T), 
         edad_gpo_madre = fct_explicit_na(edad_gpo_madre, na_level = "(No registrado)"),
         edad_gpo_padre = fct_explicit_na(edad_gpo_padre, na_level = "(No registrado)"))%>% 
  filter(mother_age < 50,
         mother_age >= 10) %>%
  group_by(edad_gpo_madre, edad_gpo_padre) %>% 
  summarise(n_acum = sum(count)) %>% 
  group_by(edad_gpo_madre) %>% 
  mutate( prop = 100*n_acum/sum(n_acum)) %>% 
  ungroup

tbl_data %>% 
  ggplot(aes(x = edad_gpo_madre, 
             y = prop, 
             fill= edad_gpo_padre)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = col.gradient.cut) + 
  ylab("Porcentaje (%)") + 
  xlab("Edad de la madre") +
  guides(fill = guide_legend("Edad del\npadre")) +
  ggtitle("Proporción de nacims. por edad de la madre \ndado edad de la madre", 
          "Public data") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90)) 





# font-import: http://fonts.googleapis.com/css?family=Risque
# font-family: 'Risque'

