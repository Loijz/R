


rm(list = ls())
gc()


# load libraries -----------------------------------

library(data.table)
library(stringr)
library(ggplot2)
##install.packages(c("ggtext", "ggh4x", "shadowtext", "paletteer", "colorspace", "extrafont"))
library(shadowtext)
library(ggtext)
library(ggh4x)
library(paletteer)
library(colorspace)
library(extrafont)
library(dplyr)
font_import()
?extrafont
coffee_font <- "Candara"

# input data frame ----------------------------------- 

coffee <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-14/coffee_survey.csv')



head(coffee)
str(coffee)
summary(coffee$where_drink)
coffee$where_drink

coffee2 <- coffee %>%
  filter(!is.na(cups) & !is.na(age) & (gender == "Male" | gender == "Female"))


coffee2$age |> unique()
coffee2$cups |> unique()
coffee2$gender |> unique()


coffee_cups = coffee2[, by = .(age, cups, gender), .N]
summary(coffee_cups)

coffee_cups$age = coffee_cups$age |> factor(levels = c(
  "<18 years old",
  "18-24 years old",
  "25-34 years old",
  "35-44 years old",
  "45-54 years old",
  "55-64 years old",
  ">65 years old"
))
summary(coffee_cups)

coffee_cups$cups = coffee_cups$cups |> factor(levels = c(
  "Less than 1",
  "1",
  "2",
  "3",
  "4",
  "More than 4"
))

coffee_cups$gender = coffee_cups$gender |> factor(levels = c(
  "Male",
  "Female"
))

summary(coffee_cups)



coffeeplot2 = coffee_cups|>
  ggplot(aes(N, gender, fill = cups)) +
  
  geom_col(
    position = "fill"
  ) +
  
scale_x_continuous(
    expand = c(0, 0),
    labels = scales::percent,
    breaks = c(.0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1)
  ) +
  
  scale_fill_manual(
    values = c('#ECCE60', '#CE8964', '#AE501D', '#832232', '#54004B', '#0B0033')
  ) +
  
  theme_minimal(base_family = coffee_font) +
  
  theme(
    legend.position = "top",
    legend.title.position = "top",
    
    strip.text = element_text(face = "bold"),
    
    axis.title.y = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10)),
    
    axis.text = element_text(color = "black"),
    axis.text.y = element_text(color = "black"), 
    axis.text.x = element_text(color = "black"),
    
    panel.spacing = unit(1, "lines"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = .35, linetype = "dotted", lineend = "round", color = "#000000"),
    
    plot.title    = element_text(size = 26, family = coffee_font, face = "bold"),
    plot.subtitle = element_markdown(size = 9, family = coffee_font, margin = margin(b = 15)),
    plot.caption  = element_markdown(size = 7, family = coffee_font, margin = margin(t = 15)),
    
    plot.title.position = "plot",
    plot.caption.position = "plot",
    
    plot.background = element_rect(fill = "#80713B", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  
  labs(
    x = "Frequency of daily cups",
    fill = "How much coffee do you drink every day?",
    
    title = "The Great American Coffee Taste Test",
    subtitle = paste0(
      "Is there a gender difference between daily cups drunk?"
    ),
    
    caption = glue::glue(
      "<span style='float:left;'>Source: <b>Great American Coffee Taste Test</b></span>
       <span style='float:right;'>Graphic: <b>Jarkko Schaad</b></span>"
      )
  )

coffeeplot2

ggsave(
  plot = coffeeplot2, filename = "tidytuesday212024.png",
  width = 10, height = 8, units = "in", dpi = 600
)




