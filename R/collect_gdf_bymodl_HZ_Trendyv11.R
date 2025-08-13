collect_gdf_bymodl <- function(
    modl, dir,
    filn_csoil_init, filn_csoil_final,
    filn_cveg_init, filn_cveg_final,
    filn_cleaf_init, filn_cleaf_final,
    filn_croot_init, filn_croot_final,
    filn_cwood_init, filn_cwood_final,
    filn_npp_init, filn_npp_final,
    filn_csoil_change, filn_cveg_change,
    filn_gpp_init, filn_gpp_final,
    filn_rh_init, filn_rh_final,
    filn_cLitter_init, filn_cLitter_final,
    filn_landCoverFrac
){

  rlang::inform(paste("Collecting outputs for", modl))

  ## first get cSoil
  gdf <- read_nc_onefile(paste0(dir, "/data/", filn_csoil_init, ".nc"), ignore_time = TRUE, varnam = "cSoil") %>%
    nc_to_df(varnam = "cSoil") %>%
    tidyr::drop_na(myvar) %>%
    dplyr::rename(csoil_init = myvar) %>%
    left_join(
      read_nc_onefile(paste0(dir, "/data/", filn_csoil_final, ".nc"), ignore_time = TRUE, varnam = "cSoil") %>%
        nc_to_df(varnam = "cSoil") %>%
        tidyr::drop_na(myvar) %>%
        dplyr::rename(csoil_final = myvar),
      by = c("lon", "lat")
    ) %>%

    ## cVeg
    left_join(
      read_nc_onefile(paste0(dir, "/data/", filn_cveg_init, ".nc"), ignore_time = TRUE, varnam = "cVeg") %>%
        nc_to_df(varnam = "cVeg") %>%
        tidyr::drop_na(myvar) %>%
        dplyr::rename(cveg_init = myvar),
      by = c("lon", "lat")
    ) %>%
    left_join(
      read_nc_onefile(paste0(dir, "/data/", filn_cveg_final, ".nc"), ignore_time = TRUE, varnam = "cVeg") %>%
        nc_to_df(varnam = "cVeg") %>%
        tidyr::drop_na(myvar) %>%
        dplyr::rename(cveg_final = myvar),
      by = c("lon", "lat")
    ) %>%

    ## cLeaf
    left_join(
      read_nc_onefile(paste0(dir, "/data/", filn_cleaf_init, ".nc"), ignore_time = TRUE, varnam = "cLeaf") %>%
        nc_to_df(varnam = "cLeaf") %>%
        tidyr::drop_na(myvar) %>%
        dplyr::rename(cleaf_init = myvar),
      by = c("lon", "lat")
    ) %>%
    left_join(
      read_nc_onefile(paste0(dir, "/data/", filn_cleaf_final, ".nc"), ignore_time = TRUE, varnam = "cLeaf") %>%
        nc_to_df(varnam = "cLeaf") %>%
        tidyr::drop_na(myvar) %>%
        dplyr::rename(cleaf_final = myvar),
      by = c("lon", "lat")
    ) %>%

    ## cRoot
    left_join(
      read_nc_onefile(paste0(dir, "/data/", filn_croot_init, ".nc"), ignore_time = TRUE, varnam = "cRoot") %>%
        nc_to_df(varnam = "cRoot") %>%
        tidyr::drop_na(myvar) %>%
        dplyr::rename(croot_init = myvar),
      by = c("lon", "lat")
    ) %>%
    left_join(
      read_nc_onefile(paste0(dir, "/data/", filn_croot_final, ".nc"), ignore_time = TRUE, varnam = "cRoot") %>%
        nc_to_df(varnam = "cRoot") %>%
        tidyr::drop_na(myvar) %>%
        dplyr::rename(croot_final = myvar),
      by = c("lon", "lat")
    ) %>%

    ## cWood
    left_join(
      read_nc_onefile(paste0(dir, "/data/", filn_cwood_init, ".nc"), ignore_time = TRUE, varnam = "cWood") %>%
        nc_to_df(varnam = "cWood") %>%
        tidyr::drop_na(myvar) %>%
        dplyr::rename(cwood_init = myvar),
      by = c("lon", "lat")
    ) %>%
    left_join(
      read_nc_onefile(paste0(dir, "/data/", filn_cwood_final, ".nc"), ignore_time = TRUE, varnam = "cWood") %>%
        nc_to_df(varnam = "cWood") %>%
        tidyr::drop_na(myvar) %>%
        dplyr::rename(cwood_final = myvar),
      by = c("lon", "lat")
    ) %>%

    ## NPP
    left_join(
      read_nc_onefile(paste0(dir, "/data/", filn_npp_init, ".nc"), ignore_time = TRUE, varnam = "npp") %>%
        nc_to_df(varnam = "npp") %>%
        tidyr::drop_na(myvar) %>%
        dplyr::rename(npp_init = myvar) %>%
        mutate(npp_init = npp_init * 31536000),
      by = c("lon", "lat")
    ) %>%
    left_join(
      read_nc_onefile(paste0(dir, "/data/", filn_npp_final, ".nc"), ignore_time = TRUE, varnam = "npp") %>%
        nc_to_df(varnam = "npp") %>%
        tidyr::drop_na(myvar) %>%
        dplyr::rename(npp_final = myvar) %>%
        mutate(npp_final = npp_final * 31536000),
      by = c("lon", "lat")
    ) %>%

    ## soil C change during the last decade
    left_join(
      read_nc_onefile(paste0(dir, "/data/", filn_csoil_change, ".nc"), ignore_time = TRUE, varnam = "cSoil") %>%
        nc_to_df(varnam = "cSoil") %>%
        tidyr::drop_na(myvar) %>%
        dplyr::rename(csoil_change = myvar),
      by = c("lon", "lat")
    ) %>%

    ## Veg C change during the last decade
    left_join(
      read_nc_onefile(paste0(dir, "/data/", filn_cveg_change, ".nc"), ignore_time = TRUE, varnam = "cVeg") %>%
        nc_to_df(varnam = "cVeg") %>%
        tidyr::drop_na(myvar) %>%
        dplyr::rename(cveg_change = myvar),
      by = c("lon", "lat")
    ) %>%
    ### GPP
    left_join(
      read_nc_onefile(paste0(dir, "/data/", filn_gpp_init, ".nc"), ignore_time = TRUE, varnam = "gpp") %>%
        nc_to_df(varnam = "gpp") %>%
        tidyr::drop_na(myvar) %>%
        dplyr::rename(gpp_init = myvar) %>%
        mutate(gpp_init = gpp_init*31536000),
      by = c("lon", "lat")
    ) %>%
    left_join(
      read_nc_onefile(paste0(dir, "/data/", filn_gpp_final, ".nc"), ignore_time = TRUE, varnam = "gpp") %>%
        nc_to_df(varnam = "gpp") %>%
        tidyr::drop_na(myvar) %>%
        dplyr::rename(gpp_final = myvar) %>%
        mutate(gpp_final = gpp_final*31536000),
      by = c("lon", "lat")
    ) %>%
    ### heterotrophic respiration
    left_join(
      read_nc_onefile(paste0(dir, "/data/", filn_rh_init, ".nc"), ignore_time = TRUE, varnam = "rh") %>%
        nc_to_df(varnam = "rh") %>%
        tidyr::drop_na(myvar) %>%
        dplyr::rename(rh_init = myvar) %>%
        mutate(rh_init = rh_init*31536000),
      by = c("lon", "lat")
    ) %>%
    left_join(
      read_nc_onefile(paste0(dir, "/data/", filn_rh_final, ".nc"), ignore_time = TRUE, varnam = "rh") %>%
        nc_to_df(varnam = "rh") %>%
        tidyr::drop_na(myvar) %>%
        dplyr::rename(rh_final = myvar) %>%
        mutate(rh_final = rh_final*31536000),
      by = c("lon", "lat")
    ) %>%
    ### cLitter
    left_join(
      read_nc_onefile(paste0(dir, "/data/", filn_cLitter_init, ".nc"), ignore_time = TRUE, varnam = "cLitter") %>%
        nc_to_df(varnam = "cLitter") %>%
        tidyr::drop_na(myvar) %>%
        dplyr::rename(cLitter_init = myvar) ,
      by = c("lon", "lat")
    ) %>%
    left_join(
      read_nc_onefile(paste0(dir, "/data/", filn_cLitter_final, ".nc"), ignore_time = TRUE, varnam = "cLitter") %>%
        nc_to_df(varnam = "cLitter") %>%
        tidyr::drop_na(myvar) %>%
        dplyr::rename(cLitter_final = myvar),

      by = c("lon", "lat")
    ) %>%
    left_join(
      read_nc_onefile(paste0(dir, "/data/", filn_landCoverFrac, ".nc"), ignore_time = TRUE, varnam = "landCoverFrac") %>%
        nc_to_df(varnam = "landCoverFrac") %>%
        tidyr::drop_na(myvar) %>%
        dplyr::rename(landCoverFrac = myvar),

      by = c("lon", "lat")
    ) %>%
    ## add model name as column
    mutate(modl = modl)

  return(gdf)
}
