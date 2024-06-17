get_deltas <- function(df){
  df |>
    mutate(
      dcveg = (cveg_final - cveg_init)/cveg_init,
      dcroot = (croot_final - croot_init)/croot_init,
      # dgpp = (gpp_final - gpp_init)/gpp_init,
      dnpp = (npp_final - npp_init)/npp_init
    ) |>
    mutate(
      # l_gpp_npp = dgpp / dnpp,
      l_npp_cveg = dcveg / dnpp,
      l_croot_cveg = dcroot / dcveg
    )
}
