collect_gdf_bymodl <- function(modl, dir,
                               filn_gpp_init, filn_gpp_final,
                               filn_npp_init, filn_npp_final,
                               filn_cVeg_init, filn_cVeg_final,
                               filn_cRoot_init, filn_cRoot_final
                               # filn_cLeaf_init, filn_cLeaf_final,
                               # filn_cWood_init, filn_cWood_final
                               # filn_cSoil_init, filn_cSoil_final,
                               # filn_cSoil_change, filn_cVeg_change
                               ){

  rlang::inform(paste("Collecting outputs for", modl))

  ## first get cVeg
  gdf <- read_nc_onefile(paste0(dir, "/processed/", filn_cVeg_init, ".nc"), ignore_time = TRUE, varnam = "cVeg") |>
    nc_to_df(varnam = "cVeg") |>
    tidyr::drop_na() |>
    dplyr::rename(cveg_init = "cVeg") |>
    left_join(
      read_nc_onefile(paste0(dir, "/processed/", filn_cVeg_final, ".nc"), ignore_time = TRUE, varnam = "cVeg") |>
        nc_to_df(varnam = "cVeg") |>
        tidyr::drop_na() |>
        dplyr::rename(cveg_final = "cVeg"),
      by = c("lon", "lat")
    ) |>

    ## cRoot
    left_join(
      read_nc_onefile(paste0(dir, "/processed/", filn_cRoot_init, ".nc"), ignore_time = TRUE, varnam = "cRoot") |>
        nc_to_df(varnam = "cRoot") |>
        tidyr::drop_na() |>
        dplyr::rename(croot_init = "cRoot"),
      by = c("lon", "lat")
    ) |>
    left_join(
      read_nc_onefile(paste0(dir, "/processed/", filn_cRoot_final, ".nc"), ignore_time = TRUE, varnam = "cRoot") |>
        nc_to_df(varnam = "cRoot") |>
        tidyr::drop_na() |>
        dplyr::rename(croot_final = "cRoot"),
      by = c("lon", "lat")
    ) |>

    # ## GPP
    # left_join(
    #   read_nc_onefile(paste0(dir, "/processed/", filn_gpp_init, ".nc"), ignore_time = TRUE, varnam = "gpp") |>
    #     nc_to_df(varnam = "gpp") |>
    #     tidyr::drop_na() |>
    #     dplyr::rename(gpp_init = "gpp") |>
    #     mutate(gpp_init = gpp_init),
    #   by = c("lon", "lat")
    # ) |>
    # left_join(
    #   read_nc_onefile(paste0(dir, "/processed/", filn_gpp_final, ".nc"), ignore_time = TRUE, varnam = "gpp") |>
    #     nc_to_df(varnam = "gpp") |>
    #     tidyr::drop_na() |>
    #     dplyr::rename(gpp_final = "gpp") |>
    #     mutate(gpp_final = gpp_final),
    #   by = c("lon", "lat")
    # ) |>

    ## NPP
    left_join(
      read_nc_onefile(paste0(dir, "/processed/", filn_npp_init, ".nc"), ignore_time = TRUE, varnam = "npp") |>
        nc_to_df(varnam = "npp") |>
        tidyr::drop_na() |>
        dplyr::rename(npp_init = "npp") |>
        mutate(npp_init = npp_init),
      by = c("lon", "lat")
    ) |>
    left_join(
      read_nc_onefile(paste0(dir, "/processed/", filn_npp_final, ".nc"), ignore_time = TRUE, varnam = "npp") |>
        nc_to_df(varnam = "npp") |>
        tidyr::drop_na() |>
        dplyr::rename(npp_final = "npp") |>
        mutate(npp_final = npp_final),
      by = c("lon", "lat")
    ) |>

    # ## soil C change during the last decade
    # left_join(
    #   read_nc_onefile(paste0(dir, "/processed/", filn_cSoil_change, ".nc"), ignore_time = TRUE, varnam = "cSoil") |>
    #     nc_to_df(varnam = "cSoil") |>
    #     tidyr::drop_na() |>
    #     dplyr::rename(csoil_change = "cSoil"),
    #   by = c("lon", "lat")
    # ) |>

    # ## Veg C change during the last decade
    # left_join(
    #   read_nc_onefile(paste0(dir, "/processed/", filn_cVeg_change, ".nc"), ignore_time = TRUE, varnam = "cVeg") |>
    #     nc_to_df(varnam = "cVeg") |>
    #     tidyr::drop_na() |>
    #     dplyr::rename(cveg_change = "cSoil"),
    #   by = c("lon", "lat")
    # ) |>

    ## add model name as column
    mutate(modl = modl)

  # ## manipulated
  # if (modl == "SDGVM"){
  #   gdf <- gdf |>
  #     mutate(cwood_final = cwood_final * 0.0, cwood_init = cwood_init * 0.0)
  # }

  return(gdf)
}

collect_gdf_bymodl_byvar <- function(
  modl,
  dir,
  filn_init,
  filn_final,
  varnam
){

  rlang::inform(paste("Collecting outputs for", modl, ", variable:", varnam))

  gdf <- read_nc_onefile(paste0(dir, "/processed/", filn_init, ".nc"), ignore_time = TRUE, varnam = varnam) |>
    nc_to_df(varnam = varnam) |>
    tidyr::drop_na(!!varnam) |>
    dplyr::rename_with(~stringr::str_c(., "_init"), 3) |>
    left_join(
      read_nc_onefile(paste0(dir, "/processed/", filn_final, ".nc"), ignore_time = TRUE, varnam = varnam) |>
        nc_to_df(varnam = varnam) |>
        tidyr::drop_na(!!varnam) |>
        dplyr::rename_with(~stringr::str_c(., "_final"), 3),
      by = c("lon", "lat")
    )

  return(gdf)
}
