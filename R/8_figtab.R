#-----------------------------------------------------------------------------#
#                       8. Figures and tables generation                      #
#-----------------------------------------------------------------------------#

library("sf")
library("tmap")
library("tidyverse")
library("eurostat")
library("ggpubr")
library("cowplot")
library("readxl")
library("biscale")

# 0. Utils for computing trends
linear_trend <- function(df, outcome, spunit){
  mod <- lm(as.formula(paste0(outcome, "~year")), data = df)
  data.frame(spunit = as.character(df[1,spunit]),
             int = coef(mod)[1],
             coef = coef(mod)[2],
             lower = confint(mod)[2,1],
             upper = confint(mod)[2,2],
             pval = summary(mod)$coefficients[2,4])
}

# 1. TS main figure ----
pm25_region <- read_csv("data/processed/pm25_region.csv")
FWI_region <- read_csv("data/processed/FWI_region.csv")
pm25_trend <- split(pm25_region, f = pm25_region$region) |>
  map_df(linear_trend, outcome = "pm25_pop", spunit = "region")
FWI_trend <- split(FWI_region, f = FWI_region$region) |>
  map_df(linear_trend, outcome = "FWI_pop", spunit = "region")
p1 <- ggplot() +
  geom_abline(data = pm25_trend, aes(intercept = int, slope = coef, col=spunit),
              lty = 2, show.legend = FALSE) +
  geom_line(data = pm25_region, aes(x=year, y=pm25_pop, col=region, group=region),
            alpha = 0.7, lwd = 0.5) +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#009988")) +
  scale_x_continuous(breaks = seq(2003, 2022, 2), minor_breaks = seq(2003, 2022, 1)) +
  theme_bw() +
  xlab("") +
  ylab(expression(Annual~average~`wildfire-PM`[2.5]~(mu*g/m^3))) +
  labs(colour = "") +
  theme(legend.position = "none")
p2 <- ggplot() +
  geom_abline(data = FWI_trend, aes(intercept = int, slope = coef, col=spunit),
              lty = 2, show.legend = FALSE) +
  geom_line(data = FWI_region, aes(x=year, y=FWI_pop, col=region, group=region),
            alpha = 0.7, lwd = 0.5) +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#009988")) +
  scale_x_continuous(breaks = seq(1980, 2022, 4), minor_breaks = seq(1982, 2022, 4)) +
  theme_bw() +
  xlab("") +
  ylab("Annual average FWI") +
  labs(colour = "") +
  theme(legend.position = "bottom",
        legend.margin=margin(-30,0,-20,0),
        legend.box.margin=margin(-30,0,-20,0))
p3 <- as_ggplot(get_legend(p2))
p2 <- p2 + theme(legend.position = "none")
pall <- ggarrange(ggarrange(p1, p2, nrow = 1), p3, nrow = 2, heights = c(10, 1)) +
  bgcolor("white")
# ggsave("figures/regiontrend_popw.png", pall,  width = 10, height = 5)
# Export trends
trendtab <- rbind(mutate(pm25_trend, metric = "PM2.5"),
                       mutate(FWI_trend, metric = "FWI")) |>
  rename(region = spunit, pvalue = pval) |>
  mutate(trend = paste0(round(coef, 3), " (", round(lower, 3), ", ", round(upper,3), ")"),
         pvalue = round(pvalue, 2)) |>
  select(metric, region, trend, pvalue) |>
  arrange(desc(metric), region)
# write_csv(trendtab, "figures/regiontrend_popw.csv")
rm("FWI_region", "FWI_trend", "pm25_region", "pm25_trend",
   "p1", "p2", "p3", "pall", "trendtab")

# 2. TS sensitivity figure ----
pm25_region <- read_csv("data/processed/pm25_region.csv")
FWI_region <- read_csv("data/processed/FWI_region.csv")
pm25_trend <- split(pm25_region, f = pm25_region$region) |>
  map_df(linear_trend, outcome = "pm25_spatial", spunit = "region")
FWI_trend <- split(FWI_region, f = FWI_region$region) |>
  map_df(linear_trend, outcome = "FWI_spatial", spunit = "region")
p1 <- ggplot() +
  geom_abline(data = pm25_trend, aes(intercept = int, slope = coef, col=spunit),
              lty = 2, show.legend = FALSE) +
  geom_line(data = pm25_region, aes(x=year, y=pm25_spatial, col=region, group=region),
            alpha = 0.7, lwd = 0.5) +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#009988")) +
  scale_x_continuous(breaks = seq(2003, 2022, 2), minor_breaks = seq(2003, 2022, 1)) +
  theme_bw() +
  xlab("") +
  ylab(expression(Annual~average~`wildfire-PM`[2.5]~(mu*g/m^3))) +
  labs(colour = "") +
  theme(legend.position = "none")
p2 <- ggplot() +
  geom_abline(data = FWI_trend, aes(intercept = int, slope = coef, col=spunit),
              lty = 2, show.legend = FALSE) +
  geom_line(data = FWI_region, aes(x=year, y=FWI_spatial, col=region, group=region),
            alpha = 0.7, lwd = 0.5) +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#009988")) +
  scale_x_continuous(breaks = seq(1980, 2022, 4), minor_breaks = seq(1982, 2022, 4)) +
  theme_bw() +
  xlab("") +
  ylab("Annual average FWI") +
  labs(colour = "") +
  theme(legend.position = "bottom",
        legend.margin=margin(-30,0,-20,0),
        legend.box.margin=margin(-30,0,-20,0))
p3 <- as_ggplot(get_legend(p2))
p2 <- p2 + theme(legend.position = "none")
pall <- ggarrange(ggarrange(p1, p2, nrow = 1), p3, nrow = 2, heights = c(10, 1)) +
  bgcolor("white")
# ggsave("figures/regiontrend_spatial.png", pall,  width = 10, height = 5)
trendtab <- rbind(mutate(pm25_trend, metric = "PM2.5"),
                  mutate(FWI_trend, metric = "FWI")) |>
  rename(region = spunit, pvalue = pval) |>
  mutate(trend = paste0(round(coef, 3), " (", round(lower, 3), ", ", round(upper,3), ")"),
         pvalue = round(pvalue, 2)) |>
  select(metric, region, trend, pvalue) |>
  arrange(desc(metric), region)
# write_csv(trendtab, "figures/regiontrend_spat.csv")
rm("FWI_region", "FWI_trend", "pm25_region", "pm25_trend", "trendtab",
   "p1", "p2", "p3", "pall")


# 3. Trends by country ----

# Compute trends
pm25_country <- read_csv("data/processed/pm25_country.csv")
FWI_country <- read_csv("data/processed/FWI_country.csv")
pm25_trendpop <- split(pm25_country, f = pm25_country$NUTS_0) |>
  map_df(linear_trend, outcome = "pm25_spatial", spunit = "NUTS_0")
FWI_trendpop <- split(FWI_country, f = FWI_country$NUTS_0) |>
  map_df(linear_trend, outcome = "FWI_spatial", spunit = "NUTS_0")

# Add regions and complete names
euroregions <- read_excel("data/raw/regions/regions.xlsx", skip = 1) |>
  select(1, 4, 9)
euroregions <- euroregions[complete.cases(euroregions),]
names(euroregions) <- c("name", "NUTS_0", "region")
# Change cyprus to Southern Europe
euroregions$region[euroregions$NUTS_0=="CY"] <- "Southern Europe"
pm25_trendpop <- left_join(pm25_trendpop, euroregions, by = c("spunit" = "NUTS_0"))
FWI_trendpop <- left_join(FWI_trendpop, euroregions, by = c("spunit" = "NUTS_0"))

# Prepare for plotting
pm25_trendpop$Pvalue <- case_when(
  pm25_trendpop$pval > 0.2 ~ "> 0.2",
  pm25_trendpop$pval > 0.05 ~ "0.05 to 0.2",
  pm25_trendpop$pval <= 0.05 ~ "< 0.05")
pm25_trendpop$Pvalue <- fct_relevel(pm25_trendpop$Pvalue,
                                          c("> 0.2","0.05 to 0.2","< 0.05"))
FWI_trendpop$Pvalue <- case_when(
  FWI_trendpop$pval > 0.2 ~ "> 0.2",
  FWI_trendpop$pval > 0.05 ~ "0.05 to 0.2",
  FWI_trendpop$pval <= 0.05 ~ "< 0.05")
FWI_trendpop$Pvalue <- fct_relevel(FWI_trendpop$Pvalue,
                                    c("> 0.2","0.05 to 0.2","< 0.05"))
# Plotting
p1 <- ggdotchart(pm25_trendpop, x = "name", y = "coef",
                 color = "region",
                 dot.size = "Pvalue",
                 sorting = "descending",
                 add = "segments",
                 add.params = list(color = "lightgray", size = 2),
                 font.label = list(color = "white", size = 9,
                                   vjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey20") +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#009988")) +
  scale_size_manual(values = c(1,3,5)) +
  labs(color = "Region") +
  xlab("") +
  ylab(expression(Linear~trend~"in"~`wildfire-PM`[2.5])) +
  theme_bw() +
  theme(legend.position = "bottom", legend.box="vertical",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(size = 10),
        legend.spacing.y = unit(0.5, 'cm'),
        legend.margin=margin(-10,0.1,0.1,0.1),
        legend.box.margin=margin(-10,0.1,0.1,0.1))
p2 <- ggdotchart(FWI_trendpop, x = "name", y = "coef",
                 color = "region",
                 dot.size = "Pvalue",
                 sorting = "descending",
                 add = "segments",
                 add.params = list(color = "lightgray", size = 2),
                 font.label = list(color = "white", size = 9,
                                   vjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey20") +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#009988")) +
  scale_size_manual(values = c(1,3,5)) +
  labs(color = "Region") +
  xlab("") +
  ylab("Linear trend in Fire Weather Index") +
  theme_bw() +
  theme(legend.position = "bottom", legend.box="vertical",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(size = 10),
        legend.spacing.y = unit(0.5, 'cm'),
        legend.margin=margin(-10,0.1,0.1,0.1),
        legend.box.margin=margin(-10,0.1,0.1,0.1))
pall <- ggarrange(p1 + theme(legend.position = "none"), p2,
                  nrow = 2, heights = c(1, 1.1))
# ggsave("figures/countrytrend_popw.png", pall,  width = 8, height = 7)
rm("ccodes", "FWI_country", "FWI_trendpop", "p1", "p2", "pall",
   "pm25_country", "pm25_trendpop", "euroregions")

# 4. Bivariate choropleth ----
# Data prep
pm25_nuts2 <- read_csv("data/processed/pm25_nuts2.csv")
FWI_nuts2 <- read_csv("data/processed/FWI_nuts2.csv") |>
  filter(year %in% 2003:2022)
alldata <- full_join(pm25_nuts2, FWI_nuts2, by = c("NUTS_2", "year"))
alldata <- group_by(alldata, NUTS_2) |>
  summarise(pm25 = mean(pm25_pop),
            FWI = mean(FWI_pop))
nuts2 <- read_sf("data/raw/boundaries/NUTS_RG_01M_2021_3035.geojson") |>
  dplyr::filter(NUTS_ID %in% alldata$NUTS_2)  |>
  rename(NUTS_2 = NUTS_ID) |>
  dplyr::select(NUTS_2)
nuts2 <- full_join(nuts2, alldata, by = "NUTS_2")
nuts0 <- st_read("data/raw/boundaries/NUTS_RG_01M_2021_3035.geojson", quiet=TRUE) %>%
  filter(LEVL_CODE == 0 & NUTS_ID %in% unique(substr(nuts2$NUTS_2, 1, 2))) %>%
  st_as_sf()
nuts0 <- st_crop(nuts0, nuts2)
# Map
mapbivar <- bi_class(nuts2, pm25, FWI, style = "quantile", dim = 3) %>%
  mutate(bi_class = ifelse(str_detect(bi_class, "NA"), NA, bi_class))
bilegend <- bi_legend(pal = "GrPink",
                     dim = 3,
                     xlab = "Higher PM2.5",
                     ylab = "Higher FWI",
                     size = 9) +
  theme(plot.background = element_rect(fill='transparent', color=NA))
p1 <- ggplot() +
  geom_sf(data = mapbivar, aes(fill = bi_class),
          colour = NA, size = .1, show.legend = FALSE) +
  geom_sf(data = nuts2, color = "black", fill = NA, lwd = 0.02) +
  geom_sf(data = nuts0, color = "black", fill = NA, lwd = 0.12) +
  bi_scale_fill(pal = "GrPink", dim = 3, na.value = "white") +
  bi_theme()
p2 <- ggdraw() +
  draw_plot(p1, 0, 0, 1, 1) +
  draw_plot(bilegend, 0.65, 0.65, 0.3, 0.3)
# save_plot("figures/bimap_popw.png", p2, base_height = 5, base_width = 5)
rm("p1", "p2", "alldata", "bilegend", "FWI_nuts2",
   "mapbivar", "nuts0", "nuts2", "pm25_nuts2")

# 5. Inequalities ----
# Data prep
pm25_nuts2 <- read_csv("data/processed/pm25_nuts2.csv")
FWI_nuts2 <- read_csv("data/processed/FWI_nuts2.csv") |>
  filter(year %in% 2003:2022)
pm25_nuts2 <- full_join(pm25_nuts2, FWI_nuts2, by = c("NUTS_2", "year"))
pm25_nuts2 <- group_by(pm25_nuts2, NUTS_2) |>
  summarise(pm25 = mean(pm25_pop),
            FWI = mean(FWI_pop))
ineq <- read_excel("data/raw/deprivation/material_social_deprivation_NUTS2.xlsx") # Incomplete survey data
pm25_nuts2 <- inner_join(pm25_nuts2, ineq, by = c("NUTS_2" = "NUTS2_ID")) |>
  mutate(Classification = fct_relevel(Classification, c("Low", "Medium", "High")))
pm25means <- data.frame(Classification = c("Low", "Medium", "High"),
                        mean = tapply(pm25_nuts2$pm25, pm25_nuts2$Classification, mean),
                        sd = tapply(pm25_nuts2$pm25, pm25_nuts2$Classification, sd)) |>
  mutate(lab = paste0("Mean (SD): ", round(mean, 2), " (", round(sd, 2), ")"))
FWImeans <- data.frame(Classification = c("Low", "Medium", "High"),
                        mean = tapply(pm25_nuts2$FWI, pm25_nuts2$Classification, mean),
                        sd = tapply(pm25_nuts2$FWI, pm25_nuts2$Classification, sd)) |>
  mutate(lab = paste0("Mean (SD): ", round(mean, 2), " (", round(sd, 2), ")"))

# Plot
p1 <- ggboxplot(pm25_nuts2, x = "Classification", y = "pm25",
          color = "Classification", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
          add = "jitter") +
  geom_text(data = pm25means, aes(x = Classification,  y = 0.01, label = lab)) +
  theme_bw() + theme(legend.position = "none") +
  xlab("Deprivation") +
  ylab(expression(Average~`wildfire-PM`[2.5]~(mu*g/m^3)))
p2 <- ggboxplot(pm25_nuts2, x = "Classification", y = "FWI",
                color = "Classification", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                add = "jitter") +
  geom_text(data = FWImeans, aes(x = Classification,  y = 0.1, label = lab)) +
  theme_bw() + theme(legend.position = "none") +
  xlab("Deprivation") +
  ylab("Average FWI")
pall <- ggarrange(p1, p2, nrow = 1)
# ggsave("figures/inequalities_nuts2.png", pall,  width = 12, height = 5)
rm("p1", "p2", "pall", "alldata", "FWImeans",
   "pm25means", "pm25_nuts2", "FWI_nuts2", "ineq")

# 6. Yearly death counts in Europe ----
attreuro_main <- read_csv("data/processed/attributable_euro.csv") |>
  mutate(attr_main = paste0(round(attr,1), " (", round(attrlower),
                            ", ", round(attrupper,1), ")")) |>
  select(year, attr_main, Ncountries)
attreuro_sens <- read_csv("data/processed/attributable_euro_sens.csv") |>
  mutate(attr_sens = paste0(round(attr,1), " (", round(attrlower),
                            ", ", round(attrupper,1), ")")) |>
  select(year, attr_sens, Ncountries)
attrboth <- full_join(attreuro_main, attreuro_sens, by = c("year", "Ncountries")) |>
  select(year, attr_main, attr_sens, Ncountries)
# write_csv(attrboth, "figures/attributable_euro.csv")

# 7. Top 20 death counts 2003-2022 by NUTS 2 ----
nuts2 <- read_sf("data/raw/boundaries/NUTS_RG_01M_2021_3035.geojson")
attnuts <- read_csv("data/processed/attributable_nuts.csv") |>
  arrange(-attr) |>
  mutate(attributable = paste0(round(attr,1), " (", round(attrlower),
                               ", ", round(attrupper,1), ")"))
attnuts <- left_join(attnuts, st_drop_geometry(select(nuts2, NUTS_ID, NAME_LATN)),
          by = c("NUTS_mort"= "NUTS_ID")) |>
  mutate(country = case_when(
    NUTS_0 == "EL" ~ "Greece",
    NUTS_0 == "PT" ~ "Portugal",
    NUTS_0 == "IT" ~ "Italy",
    NUTS_0 == "ES" ~ "Spain")) |>
  rename(nuts = NAME_LATN) |>
  select(year, nuts, country, attributable) |>
  mutate(nuts = gsub("(PT)", "", nuts, fixed = TRUE))
attnuts <- attnuts[1:20,]
# write_csv(attnuts, "figures/attributable_nuts.csv")
