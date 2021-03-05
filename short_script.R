library(tidyverse)
library(cowplot)

dat <- read_csv("https://cdn1.sph.harvard.edu/wp-content/uploads/sites/1268/1268/20/nhefs.csv")

dat2 <- dat %>%
  select(death, age, h = ht, w = wt71) %>%
  mutate(bmi = w/(h/100)^2) %>%
  mutate(bmicat = case_when(
    bmi <= 18           ~ "Under Weight",
    bmi >18 & bmi <= 25 ~ "Normal",
    bmi >25 & bmi <= 30 ~ "Over Weight",
    bmi >30             ~ "Obese"
  )) %>%
  mutate(bmicat = factor(bmicat, c("Under Weight","Normal","Over Weight", "Obese"))) %>%
  mutate(agecat = case_when(
    age <= 45             ~ "-45",
    age >= 46 & age <= 55 ~ "46-55",
    age >= 56             ~ "56-"
  ))

dat2 %>%
  count(death, bmicat, agecat) %>%
  pivot_wider(id_cols = c(bmicat, agecat), names_from = death, values_from = n) %>%
  arrange(agecat)

model <- glm("death ~ agecat + bmicat", data = dat2, family = binomial)

coeffs <- broom::tidy(model) %>%
  filter(term != "(Intercept)") %>%
  mutate(or = exp(estimate),
         ub = exp(estimate + 1.96*std.error),
         lb = exp(estimate - 1.96*std.error))

gdat <- tribble(
  ~position,~text_size ,~vn            , ~term,
  1        ,15         ,"Age, years"    , NA,
  2        ,12         ,"    46-55"      , "agecat46-55",
  3        ,12         ,"    56-"        , "agecat56-",
  4        ,15         ,"BMI category" , NA,
  5        ,12         ,"    Normal"     , "bmicatNormal",
  6        ,12         ,"    Over Weight", "bmicatOver Weight",
  7        ,12         ,"    Obese"      , "bmicatObese"
) %>%
  left_join(coeffs, by = "term")

text_size <- rev(gdat$text_size)

#plot------

gg <- ggplot(gdat) +
  geom_point(
    aes(x = or,
        y = reorder(vn,rev(position)))
  ) +
  geom_errorbar(
    aes(xmin = lb,
        xmax = ub,
        y = reorder(vn,rev(position)),
        ),
    width = 0.25
  ) +
  geom_vline(xintercept = 1) +
  labs(x = "Odds Ratio", y = NULL) +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(hjust = 0, face="plain", size = text_size),
        axis.text.x = element_text(size = 12))

#table -----------
tdat <- gdat %>%
  mutate(
    f_or = format(or,digits=1),
    f_lb = format(lb,digits=1),
    f_ub = format(ub,digits=1)
  ) %>%
  mutate(res = if_else(
    is.na(or),
    str_glue(" "),
    str_glue("{f_or}({f_lb}, {f_ub})"))) %>%
  select(position, res, text_size)

gtable <- ggplot(tdat) +
  geom_text(aes(x = 1, y = reorder(position,rev(position)), label = res)) +
  theme_classic() +
  theme(
    axis.text = element_text(color = "white"),
    axis.title = element_text(color="white"),
    axis.ticks = element_line(color="white"),
    axis.line = element_line(color = "white"),
    axis.text.x = element_text(size = 12)
  )


plot_grid(gg,gtable, rel_widths = c(3,1), labels = c("Variable", "OR(95%CI)"), vjust = 1.2)
