# Process QOG data for Ch. 4

library(tidyverse)
library(haven)
library(writexl)

qog_raw <- read_csv("https://www.qogdata.pol.gu.se/data/qog_bas_ts_jan23.csv")

qog <- qog_raw |> 
  filter(ht_region %in% c(2, 10) | 
           cname %in% c("Canada", "Mexico", "United States of America (the)")) |>
  filter(between(year, 1990, 2020)) |> 
  mutate(region = countrycode::countrycode(ccodealp, "iso3c", "region23"),
         ht_colonial = case_match(
           ht_colonial, 
           0 ~ "Never colonized",
           1 ~ "Dutch",
           2 ~ "Spanish",
           5 ~ "British",
           6 ~ "French",
           7 ~ "Portuguese"
         )) |> 
  select(cname, ccodealp, year, region, wdi_pop, vdem_polyarchy, vdem_corr,
         ht_colonial)
  
write_rds(qog, "data/sample_qog_bas_ts_jan23.rds")
write_csv(qog, "data/sample_qog_bas_ts_jan23.csv")
write_dta(qog, "data/sample_qog_bas_ts_jan23.dta")
write_sav(qog, "data/sample_qog_bas_ts_jan23.sav")
write_xlsx(qog, "data/sample_qog_bas_ts_jan23.xlsx")

# Generate vertical line test figure

library(patchwork)

f_graph <- function(){
  ggplot() + 
    scale_x_continuous(limits = c(-.5, 1)) +
    scale_y_continuous(limits = c(-.5, 1)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    annotate("text", x = 0.03, y = .97, label = "y", hjust = "inward", vjust = -.1) +
    annotate("text", x = 1, y = 0.01, label = "x", hjust = "inward", vjust = -.1) +
    theme_minimal() +
    theme(panel.grid = element_blank(), 
          axis.text = element_blank()) +
    labs(x = "", y = "")
}

p_linear <- f_graph() + stat_function(fun = (\(x) 1 * x))
p_log <- f_graph() + stat_function(fun = (\(x) .5 + .3 * log(2 * x)))
p_cubic <- f_graph() + stat_function(fun = (\(x) 5 * x ^ 3))
p_const <- f_graph() + stat_function(fun = (\(x) .5))

p_vline <- f_graph() + geom_vline(xintercept = .25)

p_normal <- f_graph() + 
  stat_function(fun = (\(x) dnorm(x, mean = .25, sd = .4) - .08))

p_double_quad <- f_graph() +
  stat_function(fun = (\(x) x ^ 2)) +
  stat_function(fun = (\(x) -1 * x ^ 2))

# Draw a circle. 
# Source: Joran on StackOverflow (https://stackoverflow.com/a/6863490)
circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
dat <- circleFun(c(0, 0), 1.42, npoints = 1000)

p_semi_circle <- f_graph() +
  geom_path(data = dat, mapping = aes(x = x, y = y))

layout <- "
ABCD
EFGH
"

p_patchw <- (p_linear + p_cubic + p_vline  + p_log + p_const + 
    p_double_quad + p_normal + p_semi_circle) +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'A', tag_prefix = "(", tag_suffix = ")")

p_patchw

ggsave(plot = p_patchw, filename = "images/vertical_line_test.png",
       scale = .85, width = 8, height = 5)

