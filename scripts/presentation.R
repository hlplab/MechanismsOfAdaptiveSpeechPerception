# Run after running everything up to end of Section 5
exposure.data = d.AA.exposure
test.data = d.AA.test
categories = categories.AA
fig.width = 4
fig.height = 4

p <-
  exposure.data %>%
  mutate(Condition = paste0("Exposure: ", Condition)) %>%
  ggplot(aes(x = VOT, y = f0_Mel, color = Item.Category)) +
  geom_point(alpha = 0.5, shape = shapes.category[1]) +
  geom_rug(
    data = . %>%
      group_by(Condition, Item.Category) %>%
      summarise(VOT = mean(VOT), f0_Mel = mean(f0_Mel)), show.legend = FALSE) +
  scale_x_continuous(expression("VOT (ms)"), expand = expansion(mult = .1, add = 0)) +
  scale_y_continuous(expression("f0 (Mel)"), expand = expansion(mult = .1, add = 0), breaks=seq(0, 500, by = 100)) +
  coord_cartesian(ylim = c(0, 500)) +
  scale_color_manual("Category", breaks = categories, values = colors.category) +
  scale_fill_manual("Category", breaks = categories, values = colors.category) +
  facet_wrap(~ Condition, ncol = 1) +
  myGplot.defaults(base_size = phonetic.base.size, set_theme = F) +
  ggh4x::force_panelsizes(cols = phonetic.panel.width, rows = phonetic.panel.height) +
  theme(legend.position = "top")
ggsave(p, filename = "figures/presentation/AA-exposure.png", width = fig.width, height = fig.height)

ggsave(
  p +
  stat_ellipse(
    mapping = aes(fill = Item.Category, color = NULL),
    level = .95, alpha = 0.3, geom = "polygon"),
  filename = "figures/presentation/AA-exposure-representations.png", width = fig.width, height = fig.height)

limits <- get_plot_limits(p.AA.exposure)
breaks <- get_plot_breaks(p.AA.exposure)

p <-
  test.data %>%
  mutate(Condition = paste("Test:", Condition)) %>%
  ggplot(aes(x = VOT, y = f0_Mel, label = as.numeric(factor(ItemID)))) +
  geom_point(aes(color = Item.Intended_category), alpha = 0.5, shape = shapes.category[1]) +
  scale_x_continuous(
    expression("VOT (ms)"),
    expand = expansion(mult = .1, add = 0),
    breaks = c(breaks$xbreaks),
    limits = c(limits$xmin, limits$xmax)) +
  scale_y_continuous(
    expression("f0 (Mel)"),
    breaks = c(breaks$ybreaks),
    expand = expansion(mult = .1, add = 0),
    limits = c(limits$ymin, limits$ymax)) +
  scale_color_manual("Category", values = colors.category) +
  scale_fill_manual("Category", values = colors.category, guide = "none") +
  coord_cartesian(xlim = c(limits$xmin, limits$xmax), ylim = c(limits$ymin, limits$ymax), expand = FALSE) +
  facet_wrap(~ Condition, ncol = 1) +
  myGplot.defaults(base_size = phonetic.base.size, set_theme = F) +
  ggh4x::force_panelsizes(cols = phonetic.panel.width, rows = phonetic.panel.height) +
  theme(legend.position = "none")
ggsave(p, filename = "figures/presentation/AA-test.png", width = fig.width, height = fig.height)

ggsave(p +
  stat_ellipse(
    data = exposure.data %>%
      mutate(Condition = paste("Test:", Condition)),
    mapping = aes(fill = Item.Category),
    level = .95, alpha = 0.3, geom = "polygon"),
  filename = "figures/presentation/AA-test-representations.png", width = fig.width, height = fig.height)

## Normalization
d.diff <-
  exposure.data %>%
  group_by(Condition) %>%
  summarise(VOT = mean(VOT), f0_Mel = mean(f0_Mel)) %>%
  summarise(across(c("VOT", "f0_Mel"), ~ last(.x) - first(.x)))
p <-
  exposure.data %>%
  mutate(Condition = paste0("Exposure: ", Condition)) %>%
  ggplot(aes(x = VOT, y = f0_Mel)) +
  geom_point(alpha = 0.5, shape = shapes.category[1], color = "purple") +
  geom_rug(
    data = . %>%
      group_by(Condition) %>%
      summarise(VOT = mean(VOT), f0_Mel = mean(f0_Mel)),
    show.legend = FALSE) +
  scale_x_continuous(expression("VOT (ms)"), expand = expansion(mult = .1, add = 0)) +
  scale_y_continuous(expression("f0 (Mel)"), expand = expansion(mult = .1, add = 0), breaks=seq(0, 500, by = 100)) +
  coord_cartesian(ylim = c(0, 500)) +
  scale_color_manual("Category", breaks = categories, values = colors.category) +
  scale_fill_manual("Category", breaks = categories, values = colors.category) +
  facet_wrap(~ Condition, ncol = 1) +
  myGplot.defaults(base_size = phonetic.base.size, set_theme = F) +
  ggh4x::force_panelsizes(cols = phonetic.panel.width, rows = phonetic.panel.height) +
  theme(legend.position = "top")
ggsave(p, filename = "figures/presentation/AA-exposure-normalization.png", width = fig.width, height = fig.height)

test.data.normalization <-
  test.data %>%
  crossing(state = c("before", "after")) %>%
  mutate(
    VOT = ifelse(Condition == "L1-accented", VOT, ifelse(state == "before", VOT, VOT - d.diff$VOT)),
    f0_Mel = ifelse(Condition == "L1-accented", f0_Mel, ifelse(state == "before", f0_Mel, f0_Mel - d.diff$f0_Mel)),
    Condition = paste("Test:", Condition))

p <-
  test.data.normalization %>%
  filter(state == "before") %>%
  ggplot(aes(x = VOT, y = f0_Mel, label = as.numeric(factor(ItemID)))) +
  geom_point(color = "purple", alpha = 0.5, shape = shapes.category[1]) +
  scale_x_continuous(
    expression("VOT (ms)"),
    expand = expansion(mult = .1, add = 0),
    breaks = c(breaks$xbreaks),
    limits = c(limits$xmin, limits$xmax)) +
  scale_y_continuous(
    expression("f0 (Mel)"),
    breaks = c(breaks$ybreaks),
    expand = expansion(mult = .1, add = 0),
    limits = c(limits$ymin, limits$ymax)) +
  scale_color_manual("Category", values = colors.category) +
  scale_fill_manual("Category", values = colors.category, guide = "none") +
  coord_cartesian(xlim = c(limits$xmin, limits$xmax), ylim = c(limits$ymin, limits$ymax), expand = FALSE) +
  facet_wrap(~ Condition, ncol = 1) +
  myGplot.defaults(base_size = phonetic.base.size, set_theme = F) +
  ggh4x::force_panelsizes(cols = phonetic.panel.width, rows = phonetic.panel.height) +
  theme(legend.position = "none")
ggsave(p,
       filename = "figures/presentation/AA-test-normalization-before.png",
       width = fig.width, height = fig.height)
ggsave(p %+%
         (test.data.normalization %>%
            filter(state == "after")),
       filename = "figures/presentation/AA-test-normalization-after.png",
       width = fig.width, height = fig.height)
ggsave(p %+%
         (test.data.normalization %>%
            filter(state == "after")) +
         stat_ellipse(
           data = exposure.data %>%
             filter(Condition == "L1-accented") %>%
             select(-Condition) %>%
             crossing(Condition = c("L1-accented", "L2-accented")) %>%
           mutate(Condition = paste("Test:", Condition)),
           mapping = aes(fill = Item.Category),
           level = .95, alpha = 0.3, geom = "polygon"),
       filename = "figures/presentation/AA-test-normalization-after-ellipse.png",
       width = fig.width, height = fig.height)
ggsave(test.data.normalization %>%
            filter(state == "after") %>%
         ggplot(aes(x = VOT, y = f0_Mel, label = as.numeric(factor(ItemID)))) +
         geom_point(aes(color = Item.Intended_category), alpha = 0.5, shape = shapes.category[1]) +
         scale_x_continuous(
           expression("VOT (ms)"),
           expand = expansion(mult = .1, add = 0),
           breaks = c(breaks$xbreaks),
           limits = c(limits$xmin, limits$xmax)) +
         scale_y_continuous(
           expression("f0 (Mel)"),
           breaks = c(breaks$ybreaks),
           expand = expansion(mult = .1, add = 0),
           limits = c(limits$ymin, limits$ymax)) +
         scale_color_manual("Category", values = colors.category) +
         scale_fill_manual("Category", values = colors.category, guide = "none") +
         coord_cartesian(xlim = c(limits$xmin, limits$xmax), ylim = c(limits$ymin, limits$ymax), expand = FALSE) +
         facet_wrap(~ Condition, ncol = 1) +
         myGplot.defaults(base_size = phonetic.base.size, set_theme = F) +
         ggh4x::force_panelsizes(cols = phonetic.panel.width, rows = phonetic.panel.height) +
         theme(legend.position = "none") +
         stat_ellipse(
           data = exposure.data %>%
             filter(Condition == "L1-accented") %>%
             select(-Condition) %>%
             crossing(Condition = c("L1-accented", "L2-accented")) %>%
             mutate(Condition = paste("Test:", Condition)),
           mapping = aes(fill = Item.Category),
           level = .95, alpha = 0.3, geom = "polygon"),
       filename = "figures/presentation/AA-test-normalization-after-categories.png",
       width = fig.width, height = fig.height)
animate(p %+% test.data.normalization + transition_states(states = state), renderer = gifski_renderer(file = "figures/presentation/AA-test-normalization.gif"))
# animate(p, renderer = ffmpeg_renderer("figures/presentation/AA-test-normalization.mpeg"))


## decision-making
m <-
  d.AA.exposure %>%
  nest(data = -c(Condition, Subject)) %>%
  crossing(
    posterior.lapse_rate = best_performing_parameters.bias$par[1],
    beta_pi = best_performing_parameters.bias$par[2]) %>%
  crossing(
    m.ia.VOT_f0.AA %>%
      filter(prior_kappa == max(prior_kappa), prior_nu == max(prior_nu)) %>%
      nest(prior = everything())) %>%
  mutate(prior = map2(posterior.lapse_rate, prior, ~ .y %>% mutate(lapse_rate = .x))) %>%
  group_by(Condition, Subject, posterior.lapse_rate, beta_pi) %>%
  group_modify(
    ~ update_NIW_response_bias_incrementally(
      prior = .x$prior[[1]],
      beta = .x$beta_pi,
      exposure = .x$data[[1]] %>% sample_frac(1, replace = F),
      exposure.category = "Item.Category",
      exposure.cues = c("VOT", "f0_Mel"),
      decision_rule = "proportional",
      noise_treatment = "marginalize",
      lapse_treatment = "marginalize", keep.exposure_data = T),
    .keep = TRUE, verbose = T) %>%
  mutate_at(vars(starts_with("prior_")), ~ factor(.x)) %>%
  mutate_at(vars(starts_with("prior_")), fct_rev) %>%
  ungroup()

p <-
  m %>% filter(observationID == 120) %>%
  select(-c(category, response, Item.Intended_category)) %>%
  unnest(posterior) %>%
  ggplot(aes(x = category, y = lapse_bias, fill = category)) +
  geom_bar(stat = "identity") +
  scale_x_discrete("Category") +
  scale_y_continuous("Response bias") +
  scale_color_manual("Category", breaks = categories, values = colors.category) +
  scale_fill_manual("Category", breaks = categories, values = colors.category) +
  facet_wrap(~ Condition, ncol = 1) +
  myGplot.defaults(base_size = phonetic.base.size, set_theme = F) +
  ggh4x::force_panelsizes(cols = phonetic.panel.width, rows = phonetic.panel.height) +
  theme(legend.position = "none")
ggsave(p, filename = "figures/presentation/AA-exposure-response-bias.png", width = fig.width, height = fig.height)

