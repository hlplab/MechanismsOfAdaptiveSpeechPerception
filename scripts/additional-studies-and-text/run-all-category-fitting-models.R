min.observation.n <- 0

d.chodroff_wilson <- read_csv("./data/Chodroff-Wilson-2018/all_observations_with_non-missing_vot_cog_f0.csv") %>%
  rename(category = stop, VOT = vot, f0 = usef0, Talker = subj, Word = word, Vowel = vowel) %>%
  mutate(
    category = 
      plyr::mapvalues(
        category,
        c("B", "D", "G", "P", "T", "K"),
        c("/b/", "/d/", "/g/", "/p/", "/t/", "/k/")),
    gender = factor(
      plyr::mapvalues(
        gender, 
        c("F", "M"),
        c("female", "male")),
      levels = c("male", "female")),
    poa = factor(
      plyr::mapvalues(
        poa, 
        c("lab", "cor", "dor"),
        c("/b/-/p/", "/d/-/t/", "/g/-/k/")),
      levels = c("/b/-/p/", "/d/-/t/", "/g/-/k/")),
    voicing = factor(
      ifelse(category %in% c("/b/", "/d/", "/g/"), "yes", "no"),
      levels = c("yes", "no"))) %>%
  # get semitones based on formula Zach provided from Dmitrieva et al. 
  group_by(Talker) %>%
  mutate(
    f0_Mel = phonR::normMel(f0),
    f0_semitones = 12 * log(f0 / mean(f0)) / log(2)) %>%
  # filter down to subjects and stops with at least min observations
  group_by(Talker, poa, voicing) %>%
  mutate(exclude = ifelse(length(Talker) < min.observation.n, T, F)) %>%
  # only include voicing minimal pairs for which the subject meets the inclusion criteria for 
  # both the voiced and voiceless category.
  group_by(Talker, poa) %>%
  mutate(exclude = ifelse(any(exclude), T, F)) %>%
  ungroup() %>%
  filter(!exclude) %>%
  mutate(across(c(Talker, gender, category), factor)) %>%
  select(Talker, Word, Vowel, gender, category, poa, voicing, VOT, f0, f0_Mel, f0_semitones, label)

d.chodroff_wilson %<>%
  # Keep only subjects with at last n.min observations for each stop
  group_by(Talker, category) %>%
  mutate(n = length(category)) %>%
  group_by(Talker) %>%
  mutate(n = ifelse(any(is.na(n)), 0, min(n))) %>%
  filter(n > min.observation.n)

# If one was to run this as one model for all stops, the following code would work. However, this would assume 
# that the correlation between VOT and f0 is constant across all stops. We therefore fit separate models to 
# each stop.
#
# bf_vot =
#   bf(vot ~ -1 + poa : voicing + (-1 + poa : voicing | p1 | subj) + (-1 + poa : voicing | p2 | word)) +
#   lf(sigma ~ -1 + poa : voicing + (-1 + poa : voicing | q1 | subj) + (-1 + poa : voicing | q2 | word)) + gaussian()
# 
# bf_f0 =
#   bf(f0_Mel ~ -1 + poa : voicing + (-1 + poa : voicing | p1 | subj) + (-1 + poa : voicing | p2 | word)) +
#   lf(sigma ~ -1 + poa : voicing + (-1 + poa : voicing | q1 | subj) + (-1 + poa : voicing | q2 | word)) + gaussian()

# Word and Vowel are confounded with category identity.
bf_vot =
  bf(VOT ~ 1 + (1 | p1 | Talker)) +
  lf(sigma ~ 1 + (1 | q1 | Talker)) + gaussian()

bf_f0 =
  bf(f0_Mel ~ 1 + (1 | p1 | Talker)) +
  lf(sigma ~ 1 + (1 | q1 | Talker)) + gaussian()


# Helpful to get general structure of priors for this type of model:
# 
# get_prior(
#   formula = bf_vot + bf_f0,
#   data = d.chodroff_wilson)

my.priors.bivariate = c(
  # No intercept priors since model is reparameterized to have no intercepts
  #  set_prior("student_t(3, 0, 2.5)", class = "Intercept", resp = c("vot","f0Mel")),
  # weakly regularizing prior for fixed effect coefficiencts for sigma.
  set_prior("student_t(3, 0, 2.5)", class = "Intercept", resp = c("VOT","f0Mel"), dpar = "sigma"),
  # weakly regularizing prior for fixed effect coefficiencts for mu.
  #  set_prior("student_t(3, 0, 2.5)", class = "b", resp = c("vot","f0Mel")),
  # weakly regularizing prior for fixed effect coefficiencts for sigma.
  #  set_prior("student_t(3, 0, 2.5)", class = "b", resp = c("vot","f0Mel"), dpar = "sigma"),
  set_prior("cauchy(0, 5)", class = "sd", resp = c("VOT", "f0Mel")),
  set_prior("cauchy(0, 5)", class = "sd", resp = c("VOT", "f0Mel"), dpar = "sigma"),
  set_prior("cauchy(0, 5)", class = "sd", resp = c("VOT", "f0Mel"), group = c("Talker")),
  set_prior("cauchy(0, 5)", class = "sd", resp = c("VOT", "f0Mel"), group = c("Talker"), dpar = "sigma")
)

for (s in unique(d.chodroff_wilson$label)) {
  m <- brm(
    formula = bf_vot + bf_f0,
    data = d.chodroff_wilson %>%
      filter(label == s),
    prior = my.priors.bivariate,
    chains = 4, cores = 4,
    warmup = 3000,
    iter = 4500,
    control = list(adapt_delta = .995, max_treedepth = 20),
    backend = "cmdstanr",
    file = paste0("./models/production-syllable_initial_stop-VOT_f0Mel-bivariate_normal-simple_effects-", s))
}
