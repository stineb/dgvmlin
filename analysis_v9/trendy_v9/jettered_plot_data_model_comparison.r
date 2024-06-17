library(tidyverse)
library(ggbreak) 

library(ggplot2)
load("H:/Oxford/Chapter_four/Beni/DGVM_density_plot/trendy_v9/gdf_Trendyv9_20220828.rda")


setwd('H:/Imperial_college/Master_Project/Paper_writingup/github_NPP_eCO2_effect_with_Deriviation_meta_analysis/Figure_1')
#setwd("~/GitHub/github_NPP_eCO2_effect_with_Deriviation_meta_analysis/Figure_1")


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            total biomass VS AGB                          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Read in data
grass<-openxlsx::read.xlsx('../input_data/after_Oskar_equation_grass_TMX_v20220109.xlsx')
tree<-openxlsx::read.xlsx('../input_data/after_Oskar_equation_tree_with_ref_v20220109.xlsx')
grass <- grass%>%select(c(id, yi_TB, yi_AGB))
tree <- tree%>%select(c(id, yi_TB, yi_AGB))
agb <- read.csv("../input_data/AGB_effects2019.csv")%>%
  rename("obs"=X, "Site"=SITE)%>%
  select(Site, id, vi, Myc)
anpp <- read.csv("../input_data/ANPP_effects.csv")%>%
  rename("obs"=X, "Site"=SITE)%>%
  select(Site, id, vi, Myc)
dat <- rbind(grass, tree)%>%
  left_join(anpp, by="id")

no_obs <- filter(dat, is.na(Site))[,(1:3)]
no_obs <- no_obs%>%left_join(agb, by="id")
no_obs_no_site <- filter(no_obs, is.na(Site))
# There are 6 rows don't have site data, I don't know if it's because of typo in their id.
# Please check!
make_pct <- function(x) (exp(x) - 1) * 100   

dat <- dat%>%filter(!is.na(Site))%>%
  rbind(no_obs)%>%
  filter(yi_AGB>-0.2)%>%
  filter(yi_TB>-0.2)
dat <- na.omit(dat) # 228 down to 94. Too many NAs
dat <- dat%>%filter(yi_AGB>-10)
dat <- mutate(dat, obs=(1:nrow(dat)))%>%
  mutate(yi_AGB=make_pct(yi_AGB),
         yi_TB = make_pct(yi_TB),
         l_cvegag_cveg =yi_AGB/yi_TB )%>% 
  filter(l_cvegag_cveg<3, l_cvegag_cveg>-1)

l_cvegag_cveg_df<-gdf %>%
  mutate(n = purrr::map_int(data, ~nrow(.))) %>%
  mutate(data = purrr::map(data, ~slice_sample(., n = 5174,replace=T))) %>% # smallest set here
  unnest(data)  %>%
  dplyr::filter(!is.nan(dcveg), !is.nan(dcveg_ag), !is.infinite(dcveg), !is.infinite(dcveg_ag)) %>%
  mutate(l_cvegag_cveg = dcveg_ag / dcveg) %>% 
  filter(l_cvegag_cveg<3, l_cvegag_cveg>-1) # to remove NA and infinite


ggplot() +
  geom_point(data=dat, aes(x=l_cvegag_cveg,y=-0.2,col=Myc), position = position_jitter(height = 0.1), size=1,col = "#00BFC4")+                               # ggplot2 histogram & density
  geom_histogram(binwidth=0.1,data=l_cvegag_cveg_df,aes(x=l_cvegag_cveg,y = stat(density)),fill="#404080", color="#e9ecef", alpha=0.6) +
  geom_density(data=dat,aes(l_cvegag_cveg),col = "#00BFC4",size=0.8)+ 
  geom_vline(xintercept = 1, color = "grey")+
  theme_classic()+ scale_y_break(c(2, 4.8),ticklabels=4.8)+xlab('relative increase of \naboveground biomass (%) / total biomass (%)')+ylab('Kernel density estimate')


ggsave(file='../Figure_1/cVeg_ag_vs_Veg_data_model_histogram.jpg',width=5.43, height=3.42)

ggplot() +
  geom_point(data=dat, aes(x=l_cvegag_cveg,y=-0.2,col=Myc), position = position_jitter(height = 0.1), size=1)+                               # ggplot2 histogram & density
  geom_histogram(binwidth=0.1,data=l_cvegag_cveg_df,aes(x=l_cvegag_cveg,y = stat(density)),fill="#404080", color="#e9ecef", alpha=0.6) +
  geom_density(data=dat,aes(l_cvegag_cveg,col=Myc),size=0.8)+ 
  geom_vline(xintercept = 1, color = "grey")+
  theme_classic()+ scale_y_break(c(2, 4.8),ticklabels=4.8)+xlab('relative increase of \naboveground biomass (%) / total biomass (%)')+ylab('Kernel density estimate')+
  labs(col="Mycorrhizal \nassociation")

ggsave(file='../Figure_1/cVeg_ag_vs_Veg_data_model_histogram_with_Myc.jpg',width=5.43, height=3.42)



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            total biomass VS belowground                  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



l_croot_cveg_df <- gdf %>%
  mutate(n = purrr::map_int(data, ~nrow(.))) %>%
  mutate(data = purrr::map(data, ~slice_sample(., n = 5174,replace=T))) %>% # smallest set here
  unnest(data)  %>%
  dplyr::filter(!is.nan(dcveg), !is.nan(dCroot), !is.infinite(dcveg), !is.infinite(dCroot)) %>%
  mutate(l_croot_cveg = dCroot / dcveg) %>% 
  filter(l_croot_cveg<3, l_croot_cveg>-1) # to remove NA and infinite


Total_eCO2<-openxlsx::read.xlsx('../input_data/Effects_Total.xlsx')%>%
  rename(total_yi=es)
cRoot_eCO2<-read.csv('../input_data/Effects_Belowground_HY_id.csv')%>%
  rename(root_yi=es,
         Site=Site.Name,
         vi=var,
         obs=X)%>%
  left_join(Total_eCO2[,c('total_yi','id_old_science','id')],by='id_old_science')
row.names(cRoot_eCO2)<-(cRoot_eCO2$id_old_science)



cRoot_eCO2['DUKE-Ph_robinia_c_Robinia pseudoacacia-ECM_Nlow','total_yi']<- -0.3619880 # id mismatch, grab it from Total_eCO2
cRoot_eCO2['DUKE-Ph_robinia_cf_Robinia pseudoacacia-ECM_Nhigh','total_yi']<- -0.1876999 # id mismatch, grab it from Total_eCO2

cRoot_eCO2<-cRoot_eCO2%>%
  tidyr::drop_na(root_yi,total_yi)%>%
  filter(root_yi<1)%>%
  filter(total_yi<1)%>%
  mutate(Tree_or_not = case_when(
    System.Type =='Tree Stand' ~ 'Tree Stand',
    TRUE ~ 'grass_shrub'))%>%
  mutate(l_croot_cveg= make_pct(root_yi)/make_pct(total_yi))%>% 
  filter(l_croot_cveg<3, l_croot_cveg>-1)
  
ggplot() +
  geom_point(data=cRoot_eCO2, aes(x=l_croot_cveg,y=-0.2), position = position_jitter(height = 0.1), size=1,col = "#00BFC4")+                               # ggplot2 histogram & density
  geom_histogram(binwidth=0.1,data=l_croot_cveg_df,aes(x=l_croot_cveg,y = stat(density)),fill="#404080", color="#e9ecef", alpha=0.6) +
  geom_density(data=cRoot_eCO2,aes(l_croot_cveg),col = "#00BFC4",size=0.8)+
  theme_classic()+ scale_y_break(c(1, 2.8),ticklabels=2.9)+
  theme_classic()+xlab('relative increase of \nbelowground biomass (%) / total biomass (%)')+ylab('Kernel density estimate')+ 
  geom_vline(xintercept = 1, color = "grey")
ggsave(file='../Figure_1/cVeg_bg_vs_Veg_data_model_histogram_no_myc.jpg',width=5.43, height=3.42)

p1 <- ggplot() + 
  geom_hline(yintercept = 0, lty=2, size=1) + 
  geom_vline(xintercept = 0, lty=2, size=1) + theme_classic() + 
  geom_point(data=cRoot_eCO2,alpha=0.7,
             show.legend = TRUE,
             #col=carto_pal(12, "Bold")[3],
             aes(y=make_pct(root_yi), x=make_pct(total_yi), col=Myc) )
ggsave(file='../Figure_1/cVeg_bg_vs_Veg_data_model_histogram.jpg',width=5.43, height=3.42)

ggplot() +
  geom_point(data=cRoot_eCO2, aes(x=l_croot_cveg,y=-0.2,col=Myc), position = position_jitter(height = 0.1), size=1)+                               # ggplot2 histogram & density
  geom_histogram(binwidth=0.1,data=l_croot_cveg_df,aes(x=l_croot_cveg,y = stat(density)),fill="#404080", color="#e9ecef", alpha=0.6) +
  geom_density(data=cRoot_eCO2,aes(l_croot_cveg,col=Myc),size=0.8)+
  theme_classic()+ scale_y_break(c(1, 2.8),ticklabels=2.9)+
  theme_classic()+xlab('relative increase of \nbelowground biomass (%) / total biomass (%)')+ylab('Kernel density estimate')+ 
  geom_vline(xintercept = 1, color = "grey")+
  labs(col="Mycorrhizal \nassociation")

ggsave(file='../Figure_1/cVeg_bg_vs_Veg_data_model_histogram_with_Myc.jpg',width=5.43, height=3.42)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            GPP VS NPP                               ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


l_npp_gpp_df<-gdf %>%
  mutate(n = purrr::map_int(data, ~nrow(.))) %>%
  mutate(data = purrr::map(data, ~slice_sample(., n = 5174,replace=T))) %>% # smallest set here
  unnest(data) %>%
  dplyr::filter(!is.nan(dgpp), !is.nan(dnpp), !is.infinite(dgpp), !is.infinite(dnpp)) %>%
  mutate(l_npp_gpp = dnpp / dgpp)%>% 
  filter(l_npp_gpp<3, l_npp_gpp>-1)



df <- read_csv("../input_data/NewData_wide_CORRECTED2.csv") %>%
  
  # something is wrong in columns ambient_Sd, ambient_Se, elevated...
  mutate( ambient_Sd  = as.numeric(ambient_Sd),  ambient_Se  = as.numeric(ambient_Se), 
          elevated_Sd = as.numeric(elevated_Sd), elevated_Se = as.numeric(elevated_Se) )

list_exp_gt1yr <- df %>% 
  filter(!is.na(Year)) %>% 
  group_by(exp_nam) %>% 
  summarise(nyears=max(Year)) %>% 
  filter(nyears>1) %>% 
  dplyr::select(exp_nam) %>% 
  unlist() %>% 
  unname()
  

# save experiments names
df_experiments <- df %>% select(exp_nam, prev_name) %>% distinct()

df_c <- df %>%
  
  # ## Take this info from experiments below
  # select(-Fumigation_type, -Vegetation_type) %>% 
  
  # ## Add prev_name back
  # left_join( df_experiments, by = "exp_nam") %>% 
  
  ## filter experiments with only manipulated CO2
  ## (no other factors manipulated, strong reduction of data)
  filter(treatment=="c") %>%
  
  ## More than 1 year data
  filter(exp_nam %in% list_exp_gt1yr) 


df_c <- df_c %>% 
  filter(exp_nam != "POPFACE_pooled")

df_c <- df_c %>% 
  mutate(exp_nam = ifelse(exp_nam == "Durham_DukeFACE", "DUKE", exp_nam))

all_selvars <- c()



selvars <- c("GPP","gpp",	"GPP_woody")
all_selvars <- c(all_selvars, selvars)

df_c_sub <- df_c %>% 
  filter(Data_type %in% selvars) %>% 
  mutate(varnam = "gpp")
# selvars <- c("NPP","npp","NPP_woody","total_biomass") # using this sentence will get the results I love, but probably wrong
 selvars <- c("NPP","npp","NPP_woody")
all_selvars <- c(all_selvars, selvars)

df_c_sub <- df_c_sub %>% 
  bind_rows(
    df_c %>% 
      filter(Data_type %in% selvars) %>% 
      mutate(varnam = "npp")
  )



df_c_sub <- df_c_sub %>%         
  
  # get standard deviation for all data
  mutate( 
    my_ambient_sd = ambient_Sd, 
    my_elevated_sd = elevated_Sd 
  ) %>%
  rowwise() %>% 
  mutate( 
    my_ambient_sd = ifelse( is.na(my_ambient_sd),  ambient_Se  * sqrt(n_plots), my_ambient_sd ),
    my_elevated_sd  = ifelse( is.na(my_elevated_sd), elevated_Se * sqrt(n_plots), my_elevated_sd ),
    elevated=as.numeric(elevated)
  ) %>%
  
  ## Get logarithm of response ratio and its variance
  metafor::escalc( 
    measure = "ROM", 
    m1i = elevated, sd1i = my_elevated_sd, n1i = n_plots, 
    m2i = ambient,  sd2i = my_ambient_sd,  n2i = n_plots, 
    data=., 
    append = TRUE, var.names = c("logr", "logr_var") ) %>% 
  as_tibble() %>% 
  mutate( logr_se = sqrt(logr_var)/sqrt(n_plots) )

df_c_agg <- df_c_sub %>% 
  
  filter(!is.na(logr_var) & !is.na(logr)) %>% 
  mutate( id = paste(exp_nam, varnam, sep = "_XXX_")) %>% 
  
  MAd::agg( id = id, es = logr, var = logr_var, n.1 = n_plots, n.2 = n_plots, cor = 1.0, method = "BHHR", data = . ) %>% 
  
  as_tibble() %>% 
  mutate( id = str_split(id, "_XXX_") ) %>% 
  mutate( exp_nam = purrr::map_chr(id, 1),
          varnam = purrr::map_chr(id, 2) ) %>% 
  select(exp_nam, varnam, es, var) %>% 
  
  ## add number of plots column and varnam
  left_join( df_c_sub %>% 
               group_by( exp_nam, varnam ) %>%
               summarise( n_plots = sum(n_plots) ) %>% 
               select( exp_nam, varnam, n_plots ),
             by = c("exp_nam", "varnam") ) %>% 
  dplyr::rename( logr = es, 
                 logr_var = var ) %>% 
  mutate( logr_se = sqrt(logr_var)/sqrt(n_plots) ) %>% 
  # left_join( df_varnams, by = "varnam" ) %>% 
  
  ## filter NA for exp_nam due to unidentified experiment name in soil decomposition dataset
  filter(exp_nam!="NA" & !is.na(exp_nam))

df_c_wide <- df_c_agg %>% 
  select(exp_nam, varnam, logr) %>%
  tidyr::spread( varnam, logr )

## add standard error
df_c_wide_se <- df_c_agg %>%
  select(exp_nam, varnam, logr_se) %>%
  tidyr::spread( varnam, logr_se)

write_csv(df_c_wide, file = "df_c_wide.csv")
write_csv(df_c_wide_se, file = "df_c_wide_se.csv")
df_c_wide$npp[df_c_wide$exp_nam=='EucFACE']<-0.0265

df_c_wide %>% 
  knitr::kable()

tmp <- df_c_wide %>% 
  filter(!is.na(npp) & !is.na(gpp)) %>% 
  mutate(l_npp_gpp = npp / gpp) %>% 
  left_join(
    df_c_wide_se %>% 
      select(exp_nam, npp_se = npp, gpp_se = gpp) %>% 
      filter(!is.na(npp_se) & !is.na(gpp_se)),
    by = "exp_nam"
  ) %>% 
  mutate(certainty = 1/(gpp_se * npp_se))


tmp %>% 
  ggplot(aes(x = gpp, y = npp, label = exp_nam, size = certainty)) +
  geom_point(color = "red") +
  xlim(-0.1, 0.7) + ylim(-0.1, 0.7) +
  geom_abline(a = 0, b = 1, linetype = "dotted") +
  coord_equal() +
  theme_classic() +
  labs(subtitle = paste("N = ", nrow(tmp)))

ggplot() +
  geom_point(data=tmp, aes(x=l_npp_gpp,y=-0.2,col = "#00BFC4"), position = position_jitter(height = 0.1), size=1)+                               # ggplot2 histogram & density
  geom_histogram(binwidth=0.1,data=l_npp_gpp_df,aes(x=l_npp_gpp,y = stat(density)),fill="#404080", color="#e9ecef", alpha=0.6) +
  geom_density(data=tmp,aes(l_npp_gpp,col = "#00BFC4"),size=0.8)+ 
  geom_vline(xintercept = 1, color = "grey")+
  theme_classic()+xlab('relative increase of NPP (%) / GPP (%)')+ylab('Kernel density estimate')+
  scale_colour_manual(name="Mycorrhizal \nassociation", values=c("#00BFC4"),
                      labels=c("ECM"))
# all of them are ECM, find information in Cesar's data sheet. for eucface, informaiton could be found here (https://www.sciencedirect.com/science/article/abs/pii/S0038071720300316?via%3Dihub), only understroy C4 grass bond with AM
ggsave(file='../Figure_1/npp_vs_gpp_data_model_histogram.jpg',width=5.43, height=3.42)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            SOC VS Cveg                               ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nice_gdf <- gdf %>%
  unnest(data) %>%
  bind_rows(
    .,
    gdf %>%
      mutate(n = purrr::map_int(data, ~nrow(.))) %>%
      mutate(data = purrr::map(data, ~slice_sample(., n = 5174,replace = T))) %>% # smallest set here
      unnest(data) %>%
      mutate(modl = "ALL")
  ) %>%
  dplyr::filter(!is.nan(dcveg_star), !is.nan(dcsoil_star), !is.infinite(dcveg_star), !is.infinite(dcsoil_star)) %>%
  mutate(l_soc_veg = dcsoil_star / dcveg_star) %>% 
  filter(l_soc_veg<3, l_soc_veg>-1) # to remove NA and infinite

nice_gdf %>%
  ggplot() +
  geom_histogram(binwidth=0.1,aes(x=l_soc_veg,y = stat(density)),fill="#404080", color="#e9ecef", alpha=0.6) +
  xlim(-1,3) +
  theme_classic()+ xlab('relative increase of \nSOC (%) / total biomass (%)')+ylab('Kernel density estimate')+ 
  geom_vline(xintercept = 1, color = "grey")

field_data<-read_csv('H:/Imperial_college/Master_Project/SOC_CO2_upscalling/soilC_meta.csv') # From Terrer 2020 et al Nature
filtered <-field_data %>% mutate(Myc= recode(Myc, Nfixer = "N-fixer"), N=recode(N,Nlow = "non-fertilized",Nhigh = "N-fertilized")) %>%
           filter( N=="non-fertilized", Experiment_type != "Chamber", Disturbance=="intact")%>%
           mutate(Myc=str_replace(Myc,'Nfixer','Others'),
                  Myc=str_replace(Myc,'AM_ER','Others'),
                  Myc=str_replace(Myc,'N-fixer','Others'),
                  Myc=str_replace(Myc,'NM','Others'),
                  l_soc_veg=make_pct(yi)/make_pct(biomass))
  

ggplot()+
geom_point(data=filtered,alpha=0.7, show.legend = FALSE,
           aes(y=make_pct(yi), x=make_pct(biomass), col=Myc))

ggplot() +
  geom_point(data=filtered, aes(x=l_soc_veg,y=-0.2,col=Myc), position = position_jitter(height = 0.1), size=1)+                               # ggplot2 histogram & density
  geom_histogram(data=nice_gdf,binwidth=0.15,aes(x=l_soc_veg,y = stat(density),alpha="All models"), color="#e9ecef",fill='#404080') +
  geom_density(data=filtered%>%filter(Myc!='Others'),aes(l_soc_veg,col=Myc),size=0.8)+ 
  geom_vline(xintercept = 1, color = "grey")+
  scale_color_manual(values=c("#F8766D","#00BFC4","grey"))+
  scale_alpha_manual(name='Models',values=(0.6))+
  theme_classic()+xlab('relative increase of \nSOC (%) / total biomass (%)')+ylab('Kernel density estimate')+
  xlim(-3,3)+
  labs(col="Mycorrhizal \nassociation")
  


ggsave("../Figure_1/soc_vs_cveg_data_model_histogram_by_myc.jpg",width=5.43, height=3.42)


ggplot() +
  geom_point(data=filtered, aes(x=l_soc_veg,y=-0.2), position = position_jitter(height = 0.1), size=1,col='#00BFC4')+                               # ggplot2 histogram & density
  geom_histogram(data=nice_gdf,binwidth=0.15,aes(x=l_soc_veg,y = stat(density),alpha="All models"), color="#e9ecef",fill='#404080') +
  geom_density(data=filtered%>%filter(Myc!='Others'),aes(l_soc_veg),size=0.8,col='#00BFC4')+ 
  geom_vline(xintercept = 1, color = "grey")+
  scale_color_manual(values=c("#00BFC4"))+
  scale_alpha_manual(name='Models',values=(0.6))+
  theme_classic()+xlab('relative increase of \nSOC (%) / total biomass (%)')+ylab('Kernel density estimate')+
  xlim(-3,3)+
  labs(col="Mycorrhizal \nassociation")
ggsave("../Figure_1/soc_vs_cveg_data_model_histogram_no_myc.jpg",width=5.43, height=3.42)
