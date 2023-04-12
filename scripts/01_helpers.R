

sample_function = function(sampling, number_stim)
{
  power_df = matrix(nrow = 1000, ncol= 9)


  for (i in 1:1000) {

    sample_df = sampling %>%
      sample_n(number_stim) %>%
      mutate(df = "sample")

    data_tost = rbind(sampling, sample_df)

    t_e = TOSTER::dataTOSTtwo(
      data = data_tost,
      deps = "k",
      group = "df",
      low_eqbound_d = -0.4,
      high_eqbound_d = 0.4,
      desc = TRUE,
      plots = FALSE)

    tost = t_e$tost$asDF
    desc = t_e$desc$asDF

    power_df[i, 1] = min(tost$`p[1]`, tost$`p[2]`) # p value tost
    power_df[i, 2] = tost$`p[0]`# p value t-test
    power_df[i, 3] = desc$`m[1]`# mean 1 (actual)
    power_df[i, 4] = desc$`m[2]`# mean 2 (sample)
    power_df[i, 5] = desc$`sd[1]`# sd 1 (actual)
    power_df[i, 6] = desc$`sd[2]`# sd 2 (sample)
    power_df[i, 7] = desc$`n[1]`# n1 (actual)
    power_df[i, 8] = desc$`n[2]`# n2 (sample)
    power_df[i, 9] = i # n2 (sample)


  }
  return(power_df)
}


run_samples = function(sampling_df, phoneme)
{
five = sample_function(sampling_df, 6) %>%
  as.data.frame()

eight = sample_function(sampling_k, 10) %>%
  as.data.frame()

ten = sample_function(sampling_k ,12) %>%
  as.data.frame()

twelve = sample_function(sampling_k, 15) %>%
  as.data.frame()

fifteen = sample_function(sampling_k, 18) %>%
  as.data.frame()

final_df = rbind(five, eight, ten, twelve, fifteen) %>%
  mutate(segment = phoneme)

return(final_df)
}
