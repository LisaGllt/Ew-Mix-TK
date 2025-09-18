library(here)
source(file = here::here("functions/fun.R"))
f_load_libraries_colors()

# 10 vers ont été mis à un par cosme contenant 200 g de sol des Closeaux et 3 g de crottin pendant une semaine
# Ils ont ensuite été mis dans des boites de Pétri sur papier filtre humidifié puis pesés à différent temps 
# pour estimer la dynamique de dépuration du contenu digestif des vers. 

# Per_Weight > 100% : le ver a bu de l'eau.

df_gut <- read_excel(here::here("data/Data_GUT.xlsx")) |> 
  mutate(
    ID = as.factor(ID)
  ) |> 
  group_by(ID) |> 
  mutate(
    Per_Weight = 100 * Weight / Weight[t == 0]
    ) |> 
  ungroup()

p <- ggplot(
  data = df_gut, 
  aes(
    x = t,
    y = Weight,
    color = ID
  )
)+
  geom_line(
    linewidth = 0.8,
    alpha = 0.8
  )+
  geom_point(
    alpha = 0.5,
    size = 2.5
  )+
  
  labs(
    x = "Time (hours)",
    y = "Weight (mg)"
  )+
  ylim(50, NA)+
  scale_color_manual(values = c(Nord_aurora, Nord_frost, Nord_polar))+
  theme_minimal()

p <- ggplot(
  data = df_gut, 
  aes(
    x = t,
    y = Per_Weight,
    color = ID
  )
)+
  geom_line(
    linewidth = 0.8,
    alpha = 0.8
  )+
  geom_point(
    alpha = 0.5,
    size = 2.5
  )+
  geom_abline(
    intercept = 75, 
    slope = 0,
    color = col_red,
    linetype = "dotted"
    )+
  
  labs(
    x = "Time (hours)",
    y = "Percentage of initial weight (%)"
  )+
  ylim(50, NA)+
  scale_color_manual(values = c(Nord_aurora, Nord_frost, Nord_polar))+
  theme_minimal()
p
