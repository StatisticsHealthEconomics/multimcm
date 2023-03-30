
# plot multiple distributions on the same survival plot

# all pairs of same distributions

library(ggplot2)
library(ggthemes)

distns <- c(
  "exp",
  "weibull",
  "gompertz",
  "loglogistic",
  "lognormal")

bmcm_out <- list()
plot_dat <- list()

for (i in distns) {
  bmcm_out[[i]] <- readRDS(glue::glue("data/bmcm_stan_{i}_{i}.Rds"))
  plot_dat[[i]] <- prep_S_joint_data(bmcm_out[[i]])
}


drug_names <- c("1" = 'Ipilimumab', "2" = 'Nivolumab', "3" = 'Ipilimumab + Nivolumab',
                "background" = 'Cured', "uncured" = 'Uncured')

ggplot(plot_dat[[1]], aes(x = time, y = mean, group = type_tx, linetype = Tx, colour = distns[1])) +
  geom_line(size = 1.1) +
  facet_grid( ~ endpoint,
              labeller = as_labeller(c("1" = "OS", "2" = "PFS"))) +
  geom_line(data = plot_dat[[2]], aes(x = time, y = mean, group = type_tx, linetype = Tx, colour = distns[2]), size = 1.1, inherit.aes = FALSE) +
  geom_line(data = plot_dat[[3]], aes(x = time, y = mean, group = type_tx, linetype = Tx, colour = distns[3]), size = 1.1, inherit.aes = FALSE) +
  geom_line(data = plot_dat[[4]], aes(x = time, y = mean, group = type_tx, linetype = Tx, colour = distns[4]), size = 1.1, inherit.aes = FALSE) +
  geom_line(data = plot_dat[[5]], aes(x = time, y = mean, group = type_tx, linetype = Tx, colour = distns[5]), size = 1.1, inherit.aes = FALSE) +
  geom_line(data = filter(plot_dat[[5]], Tx == "background"), aes(x = time, y = mean, group = type_tx), size = 1.1, inherit.aes = FALSE) +
  ylab("Survival") +
  xlab("Time (months)") +
  ylim(0, 1) +
  xlim(0, 70) +
  theme_bw() +
  labs(color = 'Distribution', linetype = "Treatment") +
  # scale_colour_calc()
  # scale_colour_hc() +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = drug_names, values=c("solid", "dashed", "twodash", "solid", "dotted"))

ggsave(filename = here::here("plots", "plot_S_overlay.png"),
       units = "in", width = 12, height = 6, dpi = 300)
