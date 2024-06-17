library(tidyverse)
rm(list=ls())
#setwd("~/E/RA/DGVM_density_plot/trendy_v9") #h
setwd('G:/Oxford/Chapter_four/Beni/DGVM_density_plot/trendy_v9')



load("gdf_Trendyv9.rda")

gdf<-gdf%>%
  filter(modl %in% c("CABLE-POP" , "CLM5.0" ,    "ISAM" ,"JSBACH",  "LPJ-GUESS" ,"ORCHIDEE", "ORCHIDEE-CNP" ))
## all models pooled
### GPP -> NPP

gdf %>%
  ## sample 1957 points from each model (corresponds to the number of gridcell of the model with coarsest res.)
  mutate(data = purrr::map(data, ~slice_sample(., n = 5174))) %>% # smallest set here
  unnest(data) %>%
  mutate(modl = "ALL") %>%
  dplyr::filter(dnpp < 2 & dnpp > -2 & dgpp < 2 & dgpp > -2) %>%  
  dplyr::filter(!is.nan(dnpp), !is.nan(dgpp), !is.infinite(dnpp), !is.infinite(dgpp)) %>%
  ggplot(aes(y = dnpp, x = dgpp)) +
  stat_density_2d(aes(fill = after_stat(nlevel)), geom = "polygon") +
  theme_classic() +
  geom_abline(intercept=0, slope=1, linetype="dotted") +
  geom_vline(xintercept = 0, color = "grey50", size = 0.2) +
  geom_hline(yintercept = 0, color = "grey50", size = 0.2) +
  scale_fill_gradientn(colours = colorRampPalette( c("gray65", "navy", "red", "yellow"))(5),
                       guide = "legend") +
  # facet_wrap(. ~ modl, nrow = 3,  scales = "free") +
  xlim(-0.2, 0.75) + ylim(-0.2, 0.75)+
  labs(title = "TRENDY v9", subtitle = "All models pooled", x = expression(Delta ~ "GPP/GPP"), y = expression(Delta ~ "NPP/NPP"))+
  theme(legend.position = "none",
        strip.background = element_blank())
ggsave("dgpp_dnpp_trendyv9_pooled.jpg", width = 6, height = 5)


### NPP -> vegC

gdf %>%
  ## sample 1957 points from each model (corresponds to the number of gridcell of the model with coarsest res.)
  mutate(data = purrr::map(data, ~slice_sample(., n = 5174))) %>% # smallest set here
  unnest(data) %>%
  mutate(modl = "ALL") %>%
  dplyr::filter(dcveg_star < 2.5 & dcveg_star > -1.5 & dnpp < 2.5 & dnpp > -1.5) %>%
  dplyr::filter(!is.nan(dnpp), !is.nan(dcveg_star), !is.infinite(dnpp), !is.infinite(dcveg_star)) %>%
  ggplot(aes(y = dcveg_star, x = dnpp)) +
  stat_density_2d(aes(fill = after_stat(nlevel)), geom = "polygon") +
  theme_classic() +
  geom_abline(intercept=0, slope=1, linetype="dotted") +
  geom_vline(xintercept = 0, color = "grey50", size = 0.2) +
  geom_hline(yintercept = 0, color = "grey50", size = 0.2) +
  scale_fill_gradientn(colours = colorRampPalette( c("gray65", "navy", "red", "yellow"))(5),
                       guide = "legend") +
  # facet_wrap(. ~ modl, nrow = 3,  scales = "free") +
  xlim(-0.2, 0.75) + ylim(-0.2, 0.75)+
  labs(title = "TRENDY v9", subtitle = "All models pooled", x = expression(Delta ~ "NPP/NPP"), y = expression(Delta ~ "cVeg/cVeg"))+
  theme(legend.position = "none",
        strip.background = element_blank())
ggsave("dcveg_dnpp_trendyv9_pooled.jpg", width = 6, height = 5)

### NPP -> SOC

gdf %>%
  ## sample 1957 points from each model (corresponds to the number of gridcell of the model with coarsest res.)
  mutate(data = purrr::map(data, ~slice_sample(., n = 5219))) %>% # smallest set here
  unnest(data) %>%
  mutate(modl = "ALL") %>%
  dplyr::filter(dcsoil_star < 2.5 & dcsoil_star > -1.5 & dnpp < 2.5 & dnpp > -1.5) %>%
  dplyr::filter(!is.nan(dcsoil_star), !is.nan(dnpp), !is.infinite(dcsoil_star), !is.infinite(dnpp)) %>%
  ggplot(aes(x = dnpp, y = dcsoil_star)) +
  stat_density_2d(aes(fill = after_stat(nlevel)), geom = "polygon") +
  theme_classic() +
  geom_abline(intercept=0, slope=1, linetype="dotted") +
  geom_vline(xintercept = 0, color = "grey50", size = 0.2) +
  geom_hline(yintercept = 0, color = "grey50", size = 0.2) +
  scale_fill_gradientn(colours = colorRampPalette( c("gray65", "navy", "red", "yellow"))(5),
                       guide = "legend") +
  # facet_wrap(. ~ modl, nrow = 3,  scales = "free") +
  xlim(-0.2, 0.75) + ylim(-0.2, 0.75)+
  labs(title = "TRENDY v9", subtitle = "All models pooled",y = expression(Delta ~ "cSoil/cSoil"), x = expression(Delta ~ "NPP/NPP"))+
  theme(legend.position = "none",
        strip.background = element_blank())
ggsave("dnpp_dcsoil_trendyv9_pooled.jpg", width = 6, height = 5)


### SOC vs. biomass change 

gdf %>%
  ## sample 1957 points from each model (corresponds to the number of gridcell of the model with coarsest res.)
  mutate(data = purrr::map(data, ~slice_sample(., n = 5219))) %>% # smallest set here
  unnest(data) %>%
  mutate(modl = "ALL") %>%
  dplyr::filter(dcsoil_star < 2 & dcsoil_star > -2 & dcveg_star < 2 & dcveg_star > -2) %>%  
  dplyr::filter(!is.nan(dcsoil_star), !is.nan(dcveg_star), !is.infinite(dcsoil_star), !is.infinite(dcveg_star)) %>%   
  ggplot(aes(x = dcveg_star, y = dcsoil_star)) +
  stat_density_2d(aes(fill = after_stat(nlevel)), geom = "polygon") +
  theme_classic() +
  geom_abline(intercept=0, slope=1, linetype="dotted") +
  geom_vline(xintercept = 0, color = "grey50", size = 0.2) +
  geom_hline(yintercept = 0, color = "grey50", size = 0.2) +
  scale_fill_gradientn(colours = colorRampPalette( c("gray65", "navy", "red", "yellow"))(5),
                       guide = "legend") +
  # facet_wrap(. ~ modl, nrow = 3,  scales = "free") +
  xlim(-0.2, 0.75) + ylim(-0.2, 0.75)+
  labs(title = "TRENDY v9", subtitle = "All models pooled",y = expression(Delta ~ "cSoil/cSoil"),  x = expression(Delta ~ "cVeg/cVeg"))+
  theme(legend.position = "none",
        strip.background = element_blank())
ggsave("dcveg_dcsoil_trendyv9_pooled.jpg", width = 6, height = 5)


### cVeg -> cAgVeg


gdf %>%
  ## sample 1957 points from each model (corresponds to the number of gridcell of the model with coarsest res.)
  mutate(data = purrr::map(data, ~slice_sample(., n = 5219))) %>% # smallest set here
  unnest(data) %>%
  mutate(modl = "ALL") %>%
  dplyr::filter(dcveg_ag < 2.5 & dcveg_ag > -1.5 & dcveg < 2.5 & dcveg > -1.5) %>%
  dplyr::filter(!is.nan(dcveg_ag), !is.nan(dcveg), !is.infinite(dcveg_ag), !is.infinite(dcveg)) %>%
  ggplot(aes(x = dcveg, y = dcveg_ag)) +
  stat_density_2d(aes(fill = after_stat(nlevel)), geom = "polygon") +
  theme_classic() +
  geom_abline(intercept=0, slope=1, linetype="dotted") +
  geom_vline(xintercept = 0, color = "grey50", size = 0.2) +
  geom_hline(yintercept = 0, color = "grey50", size = 0.2) +
  scale_fill_gradientn(colours = colorRampPalette( c("gray65", "navy", "red", "yellow"))(5),
                       guide = "legend") +
  # facet_wrap(. ~ modl, nrow = 3,  scales = "free") +
  xlim(-0.2, 0.75) + ylim(-0.2, 0.75)+
  labs(title = "TRENDY v9", subtitle = "All models pooled", y = expression(Delta ~ "cAgVeg/cAgVeg"), x = expression(Delta ~ "cVeg/cVeg"))+
  theme(legend.position = "none",
        strip.background = element_blank())
ggsave("dcveg_dcveg_ag_trendyv9_pooled.jpg", width = 6, height = 5)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                  histogram                               ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


nice_gdf <- gdf %>% # first count the percentage
  mutate(n = purrr::map_int(data, ~nrow(.))) %>%
  mutate(data = purrr::map(data, ~slice_sample(., n = 5174))) %>% # smallest set here
  unnest(data) %>%
  mutate(modl = "TRENDY V9")%>%
  dplyr::filter(!is.nan(dgpp), !is.nan(dnpp), !is.infinite(dgpp), !is.infinite(dnpp)) %>%
  mutate(l_npp_gpp = dnpp / dgpp)%>%
  filter(l_npp_gpp<100000, l_npp_gpp>-1000000) # to remove NA and infinite

Labels<-nice_gdf%>%
  mutate(l_npp_gpp_larger_than_one = l_npp_gpp>1)%>%
  group_by(modl)%>%
  summarise(number_of_records=n(),
            percentage_larger_than_one = round(sum(l_npp_gpp_larger_than_one)/number_of_records,digits =2),
            percentage_smaller_than_one = 1-percentage_larger_than_one)
nice_gdf %>% 
  ggplot() +
  geom_density(aes(x = l_npp_gpp, y = ..density..)) +
  geom_vline(xintercept = 1, linetype = "dotted") +
  xlim(-1,3) +
  geom_text(data = Labels,aes(x=-Inf, y = Inf,label=percentage_smaller_than_one),hjust = -2, vjust= 1.5)+
  geom_text(data = Labels,aes(x=Inf, y = Inf,label=percentage_larger_than_one),hjust = 3,vjust= 1.5)+
  facet_wrap(. ~ modl, ncol = 3,  scales = "free") +
  labs(x = expression(italic("L")[NPP:GPP]), y = "Density")

ggsave("hist_dgpp_dnpp_trendyv9_pooled.jpg", width = 3, height = 2)



nice_gdf <- gdf  %>% # first count the percentage
  mutate(n = purrr::map_int(data, ~nrow(.))) %>%
  mutate(data = purrr::map(data, ~slice_sample(., n = 5174))) %>% # smallest set here
  unnest(data) %>%
  mutate(modl = "TRENDY V9")%>%
  dplyr::filter(!is.nan(dnpp), !is.nan(dcveg_star), !is.infinite(dnpp), !is.infinite(dcveg_star)) %>%
  mutate(l_cveg_star_npp = dcveg_star / dnpp) %>% 
  filter(l_cveg_star_npp<100000, l_cveg_star_npp>-1000000) # to remove NA and infinite

Labels<-nice_gdf%>%
  mutate(l_larger_than_one = l_cveg_star_npp>1)%>%
  group_by(modl)%>%
  summarise(number_of_records=n(),
            percentage_larger_than_one = round(sum(l_larger_than_one)/number_of_records,digits =2),
            percentage_smaller_than_one = 1-percentage_larger_than_one)


nice_gdf %>%
  ggplot() +
  geom_density(aes(x = l_cveg_star_npp, y = ..density..)) +
  geom_vline(xintercept = 1, linetype = "dotted") +
  xlim(-1,3) +
  geom_text(data = Labels,aes(x=-Inf, y = Inf,label=percentage_smaller_than_one),hjust = -2, vjust= 1.5)+
  geom_text(data = Labels,aes(x=Inf, y = Inf,label=percentage_larger_than_one),hjust = 3,vjust= 1.5)+
  facet_wrap(. ~ modl, ncol = 3,  scales = "free") +
  labs(x = expression(italic("L")[cVeg_star:NPP]), y = "Density")

ggsave("hist_dcveg_dnpp_trendyv9_pooled.jpg", width = 3, height = 2)



nice_gdf <- gdf %>% # first count the percentage
  mutate(n = purrr::map_int(data, ~nrow(.))) %>%
  mutate(data = purrr::map(data, ~slice_sample(., n = 5174))) %>% # smallest set here
  unnest(data) %>%
  mutate(modl = "TRENDY V9")%>%
  dplyr::filter(!is.nan(dcveg), !is.nan(dcveg_ag), !is.infinite(dcveg), !is.infinite(dcveg_ag)) %>%
  mutate(l_cvegag_cveg = dcveg_ag / dcveg) %>% 
  filter(l_cvegag_cveg<100000, l_cvegag_cveg>-1000000) # to remove NA and infinite

Labels<-nice_gdf%>%
  mutate(l_larger_than_one = l_cvegag_cveg>1)%>%
  group_by(modl)%>%
  summarise(number_of_records=n(),
            percentage_larger_than_one = round(sum(l_larger_than_one)/number_of_records,digits =2),
            percentage_smaller_than_one = 1-percentage_larger_than_one)

nice_gdf %>%
  ggplot() +
  geom_density(aes(x = l_cvegag_cveg, y = ..density..)) +
  geom_vline(xintercept = 1, linetype = "dotted") +
  xlim(-1,3)  +
  geom_text(data = Labels,aes(x=-Inf, y = Inf,label=percentage_smaller_than_one),hjust = -2, vjust= 1.5)+
  geom_text(data = Labels,aes(x=Inf, y = Inf,label=percentage_larger_than_one),hjust = 3,vjust= 1.5)+
  facet_wrap(. ~ modl, ncol = 3,  scales = "free") +
  labs(x = expression(italic("L")[cVeg_ag:cVeg]), y = "Density")

ggsave("hist_dcveg_dcveg_ag_trendyv9_pooled.jpg", width = 3, height = 2)



nice_gdf <- gdf%>% # first count the percentage
  mutate(n = purrr::map_int(data, ~nrow(.))) %>%
  mutate(data = purrr::map(data, ~slice_sample(., n = 5174))) %>% # smallest set here
  unnest(data) %>%
  mutate(modl = "TRENDY V9")%>%
  dplyr::filter(!is.nan(dnpp), !is.nan(dcsoil_star), !is.infinite(dnpp), !is.infinite(dcsoil_star)) %>%
  mutate(l_soc_npp = dcsoil_star / dnpp) %>% 
  filter(l_soc_npp<100000, l_soc_npp>-1000000) # to remove NA and infinite

Labels<-nice_gdf%>%
  mutate(l_larger_than_one = l_soc_npp>1)%>%
  group_by(modl)%>%
  summarise(number_of_records=n(),
            percentage_larger_than_one = round(sum(l_larger_than_one)/number_of_records,digits =2),
            percentage_smaller_than_one = 1-percentage_larger_than_one)


nice_gdf %>%
  ggplot() +
  geom_density(aes(x = l_soc_npp, y = ..density..)) +
  geom_vline(xintercept = 1, linetype = "dotted") +
  xlim(-1,3)  +
  geom_text(data = Labels,aes(x=-Inf, y = Inf,label=percentage_smaller_than_one),hjust = -2, vjust= 1.5)+
  geom_text(data = Labels,aes(x=Inf, y = Inf,label=percentage_larger_than_one),hjust = 3,vjust= 1.5)+
  facet_wrap(. ~ modl, ncol = 3,  scales = "free") +
  labs(x = expression(italic("L")[cSoil:NPP]), y = "Density")

ggsave("hist_dnpp_dcsoil_star_trendyv9_pooled.jpg", width = 3, height = 2)



nice_gdf <- gdf%>% # first count the percentage
  mutate(n = purrr::map_int(data, ~nrow(.))) %>%
  mutate(data = purrr::map(data, ~slice_sample(., n = 5174))) %>% # smallest set here
  unnest(data) %>%
  mutate(modl = "TRENDY V9")%>%
  dplyr::filter(!is.nan(dcveg_star), !is.nan(dcsoil_star), !is.infinite(dcveg_star), !is.infinite(dcsoil_star)) %>%
  mutate(l_soc_veg = dcsoil_star / dcveg_star) %>% 
  filter(l_soc_veg<100000, l_soc_veg>-1000000) # to remove NA and infinite

Labels<-nice_gdf%>%
  mutate(l_larger_than_one = l_soc_veg>1)%>%
  group_by(modl)%>%
  summarise(number_of_records=n(),
            percentage_larger_than_one = round(sum(l_larger_than_one)/number_of_records,digits =2),
            percentage_smaller_than_one = 1-percentage_larger_than_one)


nice_gdf %>%
  ggplot() +
  geom_density(aes(x = l_soc_veg, y = ..density..)) +
  geom_vline(xintercept = 1, linetype = "dotted") +
  xlim(-1,3)  +
  geom_text(data = Labels,aes(x=-Inf, y = Inf,label=percentage_smaller_than_one),hjust = -2, vjust= 1.5)+
  geom_text(data = Labels,aes(x=Inf, y = Inf,label=percentage_larger_than_one),hjust = 3,vjust= 1.5)+
  facet_wrap(. ~ modl, ncol = 3,  scales = "free") +
  labs(x = expression(italic("L")[cSoil:cveg_star]), y = "Density")

ggsave("hist_dcveg_star_dcsoil_star_trendyv9_pooled.jpg", width = 3, height = 2)
