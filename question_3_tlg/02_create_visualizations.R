################################
########## Question 3 ##########
################################


library(pharmaverseadam)
library(dplyr)
library(ggplot2)
library(binom)


# load adverse events and subject level dataset from pharmaverseadam
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

# filter to only treatment emergent adverse events
# so only events that started after treatment began
adae_te <- adae %>% filter(TRTEMFL == "Y")


# stacked bar plot

colrs <- c("MILD" = "lightblue", "MODERATE" = "#E69F00", "SEVERE" = "#D55E00")

severity_plot <- adae_te %>%
  filter(!is.na(AESEV)) %>%
  mutate(AESEV = factor(AESEV, levels = c("SEVERE", "MODERATE", "MILD"))) %>%
  ggplot(aes(x = ACTARM, fill = AESEV)) +
  geom_bar(position = "stack", color = "black", linewidth = 0.2) +
  labs(
    title = "AE Severity Distribution by Treatment Arm",
    subtitle = "Treatment Emergent Adverse Event Severity",
    x = "Treatment Arm",
    y = "Count of AEs",
    fill = "Severity"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(hjust = 0.5, size = 9),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "right",

    panel.grid.minor = element_blank()
  ) +
  scale_fill_manual(values = colrs)

ggsave("severity_distribution.png", severity_plot, width = 10,
       height = 6, dpi = 300)


# calculate top 10 adverse events with ci
# get one row per subject per adverse event
# calculate incidence
# use exact method which is standard for clinical trial

n_total <- n_distinct(adsl$USUBJID)
top10_ae <- adae_te %>%
  distinct(USUBJID, AETERM) %>%
  count(AETERM, name = "n_subjects") %>%
  arrange(desc(n_subjects)) %>%
  head(10) %>%
  rowwise() %>%
  mutate(
    incidence = n_subjects / n_total * 100,
    ci = list(binom.confint(n_subjects, n_total, methods = "exact")),
    ci_lower = ci$lower * 100,
    ci_upper = ci$upper * 100
  ) %>%
  ungroup() %>%
  select(AETERM, n_subjects, incidence, ci_lower, ci_upper) %>%
  mutate(
    incidence_ci = sprintf("%.1f%% (%.1f%% - %.1f%%)", incidence,
                           ci_lower, ci_upper)
  )


# box plot

top10_plot <- top10_ae %>%
  arrange(AETERM, incidence) %>%
  mutate(AETERM = reorder(AETERM, incidence)) %>%
  ggplot(aes(x = incidence, y = AETERM)) +
  geom_point(size = 3, color = "#0072B2") +
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper), width = 0.2,
                color = "#0072B2", linewidth = 0.8) +
  labs(
    title = "Top 10 Most Frequent Adverse Events",
    subtitle = "n = 306 subjects; Exact Method (Clopper-Pearson CI)",
    x = "Percentage of Patients (%)",
    y = NULL
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(margin = margin(t = 10)),
    panel.grid.minor = element_blank(),
  ) +
  scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, 5))

ggsave("top10_ae_incidence.png", top10_plot, width = 10, height = 6, dpi = 300)
