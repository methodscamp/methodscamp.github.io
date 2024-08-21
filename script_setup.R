# Process QOG data for Ch. 4 ----

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
  mutate(cname = case_when(
    cname == "Bolivia (Plurinational State of)" ~ "Bolivia",
    cname == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
    cname == "United States of America (the)" ~ "United States",
    TRUE ~ cname
  )) |> 
  select(cname, ccodealp, year, region, wdi_pop, vdem_polyarchy, vdem_corr,
         ht_colonial)
  
write_rds(qog, "data/sample_qog_bas_ts_jan23.rds")
write_csv(qog, "data/sample_qog_bas_ts_jan23.csv")
write_dta(qog, "data/sample_qog_bas_ts_jan23.dta")
write_sav(qog, "data/sample_qog_bas_ts_jan23.sav")
write_xlsx(qog, "data/sample_qog_bas_ts_jan23.xlsx")

# Generate vertical line test figure ----

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

# Generate class materials ----

library(stringr)

# identify and read Quarto files

files_part1 <- c("07_probability.qmd", "08_stats_sims.qmd")

l_lines_part1 <- lapply(files_part1, readLines)

# extract titles

titles_part1 <- sapply(
  l_lines_part1, function(x){x |> str_subset("^\\#\\s")} |> head(1)
) |> 
  str_remove("^#\\s")
  
# add header

f_add_top <- function(body, title){
  to_add <- c("---", 
              sprintf("title: 'Methods Camp - %s'", title),
              "date: 'Summer 2024'", 
              "editor: visual",
              "---", 
              "")
  print(to_add)
  c(to_add, body)
}

l_lines_part1_edit <- lapply(seq_along(files_part1), function(i){
  f_add_top(body = l_lines_part1[[i]], title = titles_part1[[i]])
})

# save Quarto files

dir.create("materials/methodscamp_part3/")

for (i in seq_along(l_lines_part1_edit)){
  writeLines(l_lines_part1_edit[[i]], 
             str_c("materials/methodscamp_part3/", files_part1[[i]]))
}

# copy images

imgs_part1 <- lapply(l_lines_part1, str_subset, "images/") |> 
  unlist() |> 
  str_extract("images/.*((jpg)|(png))") |> 
  na.omit()

imgs_folder <- "materials/methodscamp_part3/images"
dir.create(imgs_folder)
for (i in seq_along(imgs_part1)){
  file.copy(imgs_part1[[i]], imgs_folder)
}

# copy data files

data_part1 <- lapply(l_lines_part1, str_subset, "data/") |> 
  unlist() |> 
  str_extract("data/\\S*((rds)|(csv)|(xlsx)|(dta)|(sav)|(xls))") |> 
  na.omit()

data_folder <- "materials/methodscamp_part3/data"
dir.create(data_folder)
for (i in seq_along(data_part1)){
  file.copy(data_part1[[i]], data_folder)
}

# create .Rproj

text_rproj <- c("Version: 1.0", 
                "", 
                "RestoreWorkspace: Default",
                "SaveWorkspace: Default",
                "AlwaysSaveHistory: Default",
                "",
                "EnableCodeIndexing: Yes",
                "UseSpacesForTab: Yes",
                "NumSpacesForTab: 2",
                "Encoding: UTF-8",
                "",
                "RnwWeave: Sweave", 
                "LaTeX: pdfLaTeX")

writeLines(text_rproj, "materials/methodscamp_part3/methodscamp_part3.Rproj")

# create .zip file

setwd("materials")
zip(zipfile = "methodscamp_part3.zip", 
    files = "methodscamp_part3/")

unlink("methodscamp_part3/", recursive = T)

setwd("..")
