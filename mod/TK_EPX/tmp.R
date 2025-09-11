library(here)
source(file = here::here("functions/fun.R"))
f_load_libraries_colors()

df_sim <- f_MCSim_read_sim(here::here("mod/TK_EPX/sim.out"))

p <- ggplot(
  data = df_sim,
  aes(
    x = Time,
    y = C_exposition
  )
)+
  geom_line()+
  ylim(0,1)

p




