whatitis_land<-as.data.frame(
  read_nc_onefile(paste0('.', "/processed/", 'YIBs_npp_FINAL_MEAN', ".nc"), ignore_time = TRUE, varnam = "npp") %>%
    nc_to_df(varnam = "npp") %>%
    tidyr::drop_na(myvar) %>%
    dplyr::rename(landCoverFrac = myvar))

whatitis_land$lat<-as.character(whatitis_land$lat)
whatitis_gpp<-as.data.frame(
  read_nc_onefile(paste0('.', "/processed/", 'YIBs_gpp_FINAL_MEAN', ".nc"), ignore_time = TRUE, varnam = "gpp") %>%
  nc_to_df(varnam = "gpp") %>%
  tidyr::drop_na(myvar) %>%
  dplyr::rename(gpp = myvar))%>%
  mutate(lon=round(lon,digits=6),
         lat=round(lat,digits=6))
whatitis_gpp$lat<-as.character(whatitis_gpp$lat)


tryjoin<-left_join(whatitis_land,whatitis_gpp,
  by = c("lon"="lon", "lat"="lat"))
