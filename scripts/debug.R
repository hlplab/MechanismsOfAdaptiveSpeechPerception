# cue-level priors: prior_marginal_VOT_f0_stats = c(32,240)
# cue mean calculated from category-level priors -- m.io.VOT_f0.AA = c(39, 242)



# Run 1: exposure data is off by +3ms in VOT and +3Mel for f0 for /d/

temp.d.model.both <- get_parameters_phonetic_contrast.AA(m.io.VOT_f0.AA, 0.7, 0.5)
temp.d.model.native <- temp.d.model.both %>%
  filter(speech == "native") %>%
  droplevels()
temp.d.model.nonnative <- temp.d.model.both %>%
  filter(speech == "nonnative") %>%
  droplevels()

exposure.tokens.L1.n = 30
exposure.tokens.L2.n = 30
experimenter.variability_reduction = 1
# Create basic tibble
temp.d.AA.exposure <- tibble(
  Phase = "exposure",
  ItemID = as.character(1:(exposure.tokens.L1.n + exposure.tokens.L2.n)),
  Item.Type = factor(c(rep("L1-accented", exposure.tokens.L1.n), rep("L2-accented", exposure.tokens.L2.n)))
) %>% 
  # Add all unique design combinations
  crossing(
    Item.Category = factor(m.io.VOT_f0.AA$category)) %>%
  # Get cue value of item: if L2-accented then use L2 category parameters. If not sample from L1-accented distribution
  mutate(
    Condition = factor(ifelse(
      (Item.Category ==  m.io.VOT_f0.AA$category[[2]] & Item.Type == "L1-accented") | (Item.Category ==  m.io.VOT_f0.AA$category[[1]] & Item.Type == "L2-accented"),
      "L2-accented",
      "L1-accented")),
    x = map2(
      Item.Type,
      Item.Category,
      ~ case_when(
        .x == "L2-accented" & .y == m.io.VOT_f0.AA$category[[1]] ~ rmvnorm(1, temp.d.model.nonnative$mu[[1]], temp.d.model.nonnative$Sigma[[1]] / experimenter.variability_reduction),
        .x == "L2-accented" & .y == m.io.VOT_f0.AA$category[[2]] ~ rmvnorm(1, temp.d.model.nonnative$mu[[2]], temp.d.model.nonnative$Sigma[[2]] / experimenter.variability_reduction),
        .x == "L1-accented" & .y == m.io.VOT_f0.AA$category[[1]] ~ rmvnorm(1, temp.d.model.native$mu[[1]], temp.d.model.native$Sigma[[1]] / experimenter.variability_reduction),
        .x == "L1-accented" & .y == m.io.VOT_f0.AA$category[[2]] ~ rmvnorm(1, temp.d.model.native$mu[[2]], temp.d.model.native$Sigma[[2]] / experimenter.variability_reduction),
        T ~ NA_real_))) %>%
  # mutate(VOT = map(x, ~ .x[1]) %>% unlist(), f0_Mel = map(x, ~ .x[2]) %>% unlist()) %>%
  mutate(VOT = unlist(map(x, ~max(min_VOT, .x[1]))), # cue 1
         f0_Mel = unlist(map(x, ~max(min_f0_Mel, .x[2]))), # cue 2 %>%  
         x = map2(VOT, f0_Mel, ~ c(.x, .y)))

temp.d.AA.exposure %<>%
  add_subjects_to_exposure(n.subject = n.subject)


temp.d.AA.test <- make_accent_adaptation_test_design(temp.d.AA.exposure, experimenter.ideal_observer = m.io.VOT_f0.AA, test.n_block = n.test_block, n_test = n.test.token, category_dist_ratio1 = 0.7, category_dist_ratio2 = 0.5) 

p.AA.exposure <- temp.d.AA.exposure %>%
  mutate(Condition = paste0("Exposure: ", Condition)) %>%
  ggplot(aes(x = VOT, y = f0_Mel, color = Item.Category)) +
  geom_point(alpha = 0.5) +
  # stat_ellipse(aes(fill = Item.Category), level = .95, alpha = 0.3, geom = "polygon") +
  geom_rug(data = . %>%
             group_by(Condition, Item.Category) %>%
             summarise(VOT = mean(VOT), f0_Mel = mean(f0_Mel)), show.legend = FALSE) +
  scale_x_continuous(expression("VOT (ms)"), expand = expansion(mult = .1, add = 0), breaks=seq(0, 120, by = 30)) +
  scale_y_continuous(expression("f0 (Mel)"), expand = expansion(mult = .1, add = 0), breaks=seq(0, 500, by = 100)) +
  coord_cartesian(ylim = c(0, 500), xlim = c(0,120)) + 
  scale_color_manual("Category", breaks = categories.AA, values = colors.category) +
  scale_fill_manual("Category", breaks = categories.AA, values = colors.category) +
  facet_grid(~ Condition)
p.AA.test <- temp.d.AA.test %>%
  mutate(Speech = ifelse(grepl("L1", Condition), "L1-accented", "L2-accented")) %>%
  filter(Speech == "L2-accented") %>%
  mutate(Speech = paste0("Test: ", Speech)) %>%
  ggplot(aes(x = VOT, y = f0_Mel, label = as.numeric(factor(ItemID)))) + 
  geom_point(aes(color = Item.Intended_category), alpha = 0.5) +
  stat_ellipse(data = temp.d.AA.exposure %>% filter(Condition == "L2-accented"), aes(fill = Item.Category), level = .95, alpha = 0.3, geom = "polygon") +
  scale_x_continuous(expression("VOT (ms)"), 
                     expand = expansion(mult = .1, add = 0), 
                     breaks=seq(0, 120, by = 30), limits = c(limits$xmin, limits$xmax)) +
  scale_y_continuous(expression("f0 (Mel)"), 
                     expand = expansion(mult = .1, add = 0),
                     breaks=seq(0, 500, by = 100)
  ) +
  scale_color_manual("Category", values = colors.category) +
  scale_fill_manual("Category", values = colors.category, guide = "none") +
  coord_cartesian(ylim = c(0, 500), xlim = c(0,120)) + 
  # coord_cartesian(xlim = c(limits$xmin, limits$xmax), ylim = c(limits$ymin, limits$ymax)) +
  facet_grid(~ Speech)

temp.d.AA.test %<>%
  add_subjects_to_test(n.subject = n.subject)


prow = plot_grid(p.AA.exposure + theme(legend.position="none"),
                 p.AA.test + theme(legend.position="none"),
                 labels = c('A)', 'B)'), nrow = 1, rel_widths = c(2,1))

# extract a legend that is laid out horizontally
legend_prow <- get_legend(
  p.AA.exposure + 
    guides(color = guide_legend(title.position = "left", nrow = 1))
)

plot_grid(legend_prow, prow, ncol = 1, rel_heights = c(.1, 1))

# check whether the generated exposure data has the same category parameters as those priors pulled for corpus
temp = temp.d.AA.exposure %>%
  group_by(Condition, Subject, Item.Category) %>%
  summarise(
    x_N = length(x),
    x_mean = list(colMeans(reduce(x, rbind))),
    x_cov = list(cov(reduce(x, rbind))))



# debug
#temp.temp.mu_0 <- prior_marginal_VOT_f0_stats$x_mean[[1]]

temp.prior_marginal_VOT_f0_stats <- prior_marginal_VOT_f0_stats

#option 1
target.prior.mean <- (temp$x_mean[[1]] + temp$x_mean[[2]])/2 # forcing the prior mean to match the actual cue mean from the exposure data
#option 2
target.prior.mean <- (m.io.VOT_f0.AA$mu[[1]] + m.io.VOT_f0.AA$mu[[2]])/2 # forcing the prior mean to match the actual cue mean calculated from category priors (m.io.VOT_f0.AA)



temp.prior_marginal_VOT_f0_stats$x_mean[[1]] = c("VOT" = target.prior.mean[1], "f0" = target.prior.mean[2]) 


temp.mu_0 = temp.prior_marginal_VOT_f0_stats$x_mean[[1]]
temp.exposure.normalization <- temp.d.AA.exposure %>%
  group_by(Condition, Subject) %>%
  summarise(
    x_N = length(x),
    x_mean = list(colMeans(reduce(x, rbind))),
    x_cov = list(cov(reduce(x, rbind))))


temp.d.AA.normalization <- temp.d.AA.exposure %>%
  nest(data = -c(Condition, Subject)) %>%
  crossing(
    normalization = factor(c("no normalization", "centered based\non exposure"), levels = c("no normalization", "centered based\non exposure")),
    prior_kappa.normalization = 4^(1:5),
    # prior_nu.normalization = 4^(1:5),
    m.ia.VOT_f0.AA %>%
      filter(prior_kappa == max(prior_kappa) & prior_nu == max(prior_nu)) %>%
      nest(prior = everything())) %>%
  group_by(Condition, Subject, prior_kappa.normalization) %>%
  mutate(posterior = prior) %>%
  crossing(temp.d.AA.test %>% distinct(x)) %>%
  # Normalize test tokens
  mutate(
    # Get inferred mean
    mu_inferred = pmap(
      .l = list(Condition, Subject, prior_kappa.normalization),
      .f = function(currentCondition, currentSubject, currentKappa) {
        x_N <- temp.exposure.normalization[temp.exposure.normalization$Condition == currentCondition & temp.exposure.normalization$Subject == currentSubject, "x_N"][[1]]
        x_bar <- temp.exposure.normalization[temp.exposure.normalization$Condition == currentCondition & temp.exposure.normalization$Subject == currentSubject, "x_mean"][[1]][[1]]
        
        mu <- 1 / (currentKappa + x_N) * (currentKappa * temp.mu_0 + x_N * x_bar)
        return(mu)
      }),
    x = ifelse(normalization == "no normalization", x, map2(x, mu_inferred, ~ .x - (.y - temp.mu_0)))
  ) %>%
  nest(x = c(x))


temp.d.AA.normalization %<>%
group_by(Condition, Subject, prior_kappa.normalization, normalization) %>%
  add_categorization_functions() %>%
  add_categorization() %>%
  distinct() %>%
  ungroup() %>%
  mutate(
    prior_kappa.normalization = factor(as.character(prior_kappa.normalization), levels = as.character(rev(sort(unique(prior_kappa.normalization))))))


# get ItemID for normalized cue values so that the intended_category is known
pair.ItemID.observationID <- temp.d.AA.normalization %>%
  select(observationID, x, category) %>%
  left_join(., temp.d.AA.test %>% select(x, Item.Intended_category, ItemID), by = "x") %>%
  ungroup() %>%
  filter(!is.na(ItemID)) %>%
  distinct(observationID, ItemID)

temp.d.AA.normalization %<>%
  left_join(pair.ItemID.observationID) %>%
  left_join(temp.d.AA.test %>% select(Item.Intended_category, ItemID), by = "ItemID") %>%
  distinct()

# plot distribution of test tokens after normalization
temp.d.AA.normalization %>%
  filter(category == Item.Intended_category) %>%
  filter(normalization == "centered based\non exposure") %>% # only show the scenarios when normalization is applied
  filter(prior_kappa.normalization %in% c(4, 64, 1024)) %>%
  mutate(Speech = ifelse(grepl("L1", Condition), "L1-accented", "L2-accented")) %>%
  mutate(Speech = paste0("Exposure: ", Speech)) %>%
  mutate(VOT = map(x, ~ .x[1]) %>% unlist(), f0_Mel = map(x, ~ .x[2]) %>% unlist()) %>%
  ggplot(aes(x = VOT, y = f0_Mel, label = as.numeric(factor(ItemID)))) +
  geom_point(aes(color = Item.Intended_category), alpha = 0.5) +
  geom_rug(inherit.aes = FALSE, data = . %>%
             group_by(Speech, normalization, Item.Intended_category, prior_kappa.normalization) %>%
             summarise(VOT = mean(VOT), f0_Mel = mean(f0_Mel)),
           mapping = aes(x = VOT, y = f0_Mel, color = Item.Intended_category),
           sides = "bl") +
  scale_x_continuous(expression("VOT (ms)"), expand = expansion(mult = .1, add = 0), breaks=seq(0, 120, by = 30)) +
  scale_y_continuous(expression("f0 (Mel)"), expand = expansion(mult = .1, add = 0), breaks=seq(0, 500, by = 100)) +
  coord_cartesian(ylim = c(0, 500), xlim = c(0,120)) +
  scale_color_manual("Category", values = colors.category) +
  scale_shape_manual("Category", values = shapes.category) +
  facet_grid(
    prior_kappa.normalization ~ Speech,
    labeller = label_bquote(
      rows = ~kappa[0] == .(as.character(prior_kappa.normalization)))) +
  theme(legend.position = "top")

# plot normalization predictions
temp.d.AA.normalization %>%
  filter(category == Item.Intended_category) %>% # show the response for intended category
  filter(normalization == "centered based\non exposure") %>%
  # filter(prior_kappa.normalization == 1024) %>%
  ggplot(aes(x = Condition, y = response, fill = Item.Intended_category, alpha = Condition)) +
  stat_summary(fun = mean, 
               geom="bar", position = position_dodge(0.6),
               width = 0.6) +
  stat_summary(aes(color = Item.Intended_category),fun.data = mean_cl_boot, geom = "errorbar", position = position_dodge(0.6), width = 0.2) +
  coord_cartesian(ylim =  c(0,1)) + 
  scale_color_manual("Category", values = colors.category) + 
  scale_fill_manual("Category", values = colors.category) + 
  scale_alpha_discrete(range = c(0.2, 1), guide = "none") +
  xlab("Exposure condition") + 
  ylab("Predicted categorization accuracy") + 
  scale_x_discrete(labels= c("L1-\naccented", "L2-\naccented")) + 
  geom_text(inherit.aes = FALSE, data = . %>%
              group_by(Condition, normalization, prior_kappa.normalization) %>%
              summarise(mAcc = round(mean(response), digits = 2)), aes(label = mAcc, x = Condition, y = 1), size = geom_text.size) +
  facet_grid(
    . ~ prior_kappa.normalization,
    labeller = label_bquote(
      cols = ~kappa[0] == .(as.character(prior_kappa.normalization)))) + 
  theme(legend.position = "top", panel.grid.major.x = element_blank())
