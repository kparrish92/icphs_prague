
source(here::here("scripts", "00_libs.R"))
source(here("scripts", "01_helpers.R"))

### Means and sds reported from Chodroff & Wilson (2017)

set.seed(15)

sampling_p = data.frame(k = rnorm(n = 1000, mean = 51, sd = 9)) %>%
  mutate(df = "sampling")

sampling_t = data.frame(k = rnorm(n = 1000, mean = 61, sd = 9)) %>%
  mutate(df = "sampling")

sampling_k = data.frame(k = rnorm(n = 1000, mean = 56, sd = 8)) %>%
  mutate(df = "sampling")

sampling_b = data.frame(k = rnorm(n = 1000, mean = 8, sd = 2)) %>%
  mutate(df = "sampling")

sampling_d = data.frame(k = rnorm(n = 1000, mean = 14, sd = 3)) %>%
  mutate(df = "sampling")

sampling_g = data.frame(k = rnorm(n = 1000, mean = 17, sd = 3)) %>%
  mutate(df = "sampling")

### A simulated dataset was generated for each for each segment using the
### rnorm function in R

p_data = run_samples(sampling_p, "p")
t_data = run_samples(sampling_t, "t")
k_data = run_samples(sampling_k, "k")
b_data = run_samples(sampling_b, "b")
d_data = run_samples(sampling_d, "d")
g_data = run_samples(sampling_g, "g")


full_df = rbind(p_data, t_data, k_data, b_data, d_data, g_data) %>%
  rename("tost" = V1,
         "t_test" = V2,
         "mean_actual" = V3,
         "mean_sample" = V4,
         "sd_actual" = V5,
         "sd_sample" = V6,
         "n_actual" = V7,
         "n_sample" = V8,
         "iteration" = V9) %>%
  mutate(sig_tost = case_when(
    tost < .05 ~ 1,
    tost > .05 ~ 0,
  )) %>%
  mutate(sig_ttest = case_when(
    t_test < .05 ~ 1,
    t_test > .05 ~ 0,
  )) %>%
  mutate(tost = round(as.numeric(tost), digits = 3)) %>%
  mutate(t_test = round(as.numeric(tost), digits = 3)) %>%
  mutate(mean_difference = round(as.numeric(mean_actual) -
                                   as.numeric(mean_sample), digits = 3)) %>%
  mutate(participant = rep(1:10, each = 100, 30))


full_df %>%
  group_by(participant, segment) %>%
  summarize(n = n())

full_df %>%
  write.csv(here("data", "full_df.csv"))
