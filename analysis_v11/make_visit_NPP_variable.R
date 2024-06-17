# it is really annoying that VISIT does not report NPP
# we have to make NPP from GPP-RA

# Install and load the necessary package
install.packages("ncdf4")
library(ncdf4)

# Paths to the input files
gpp_file_path <- "H:/Trendy_v11_data_temporary/VISIT_S1_gpp.nc"
ra_file_path <- "H:/Trendy_v11_data_temporary/VISIT_S1_ra.nc"

# Open the input GPP and RA NetCDF files
gpp_nc <- nc_open(gpp_file_path)
ra_nc <- nc_open(ra_file_path)

# Read the GPP and RA data
gpp_data <- ncvar_get(gpp_nc, "gpp")
ra_data <- ncvar_get(ra_nc, "ra")  # Assuming the variable name in the file is 'ra'

# Compute NPP
npp_data <- gpp_data - ra_data

# Define dimensions
lon <- ncvar_get(gpp_nc, "lon")
lat <- ncvar_get(gpp_nc, "lat")
time <- ncvar_get(gpp_nc, "time")

lon_dim <- ncdim_def("lon", "degrees_east", lon)
lat_dim <- ncdim_def("lat", "degrees_north", lat)
time_dim <- ncdim_def("time", "months since 1860-01-01", time, unlim=TRUE)

# Define NPP variable
npp_var <- ncvar_def("npp", "kg m-2 s-1", list(lon_dim, lat_dim, time_dim), -99999, prec="float")

# Create a new NetCDF file for NPP
npp_file_path <- "H:/Trendy_v11_data_temporary/VISIT_S1_NPP.nc"
npp_nc <- nc_create(npp_file_path, npp_var)

# Copy data to the NPP variable
ncvar_put(npp_nc, npp_var, npp_data)

# Copy global attributes from GPP file
for (att_name in names(gpp_nc$att)) {
  ncatt_put(npp_nc, 0, att_name, ncatt_get(gpp_nc, 0, att_name)$value)
}

# Close the datasets
nc_close(gpp_nc)
nc_close(ra_nc)
nc_close(npp_nc)

## The above is given by chatGPT


#  hello, I have an .NC file, with the following structure: Source:             
#                                                                               
#  H:\Trendy_v11_data_temporary\VISIT_S1_gpp.nc                                 
#                                                                               
#  Format:                                                                      
#                                                                               
#  classic                                                                      
#                                                                               
#  Global Attributes:                                                           
#                                                                               
#  title = 'VISIT 1860-2021 monthly output for Global Carbon Budget 2022'       
#                                                                               
#  contact = 'Etsushi Kato e-kato@iae.or.jp'                                    
#                                                                               
#  experiment_id = 'S1 : recycling CRU TS4.06 1901-1920 climate + atm. CO2 +    
#  time-invariant pre-industrial land use mask without wood harvest'            
#                                                                               
#  Dimensions:                                                                  
#                                                                               
#  lon = 720                                                                    
#                                                                               
#  lat = 360                                                                    
#                                                                               
#  time = 1944 (UNLIMITED)                                                      
#                                                                               
#  Variables:                                                                   
#                                                                               
#  lon                                                                          
#                                                                               
#  Size: 720x1                                                                  
#                                                                               
#  Dimensions: lon                                                              
#                                                                               
#  Datatype: double                                                             
#                                                                               
#  Attributes:                                                                  
#                                                                               
#  units = 'degrees_east'                                                       
#                                                                               
#  lat                                                                          
#                                                                               
#  Size: 360x1                                                                  
#                                                                               
#  Dimensions: lat                                                              
#                                                                               
#  Datatype: double                                                             
#                                                                               
#  Attributes:                                                                  
#                                                                               
#  units = 'degrees_north'                                                      
#                                                                               
#  time                                                                         
#                                                                               
#  Size: 1944x1                                                                 
#                                                                               
#  Dimensions: time                                                             
#                                                                               
#  Datatype: double                                                             
#                                                                               
#  Attributes:                                                                  
#                                                                               
#  units = 'months since 1860-01-01'                                            
#                                                                               
#  gpp                                                                          
#                                                                               
#  Size: 720x360x1944                                                           
#                                                                               
#  Dimensions: lon,lat,time                                                     
#                                                                               
#  Datatype: single                                                             
#                                                                               
#  Attributes:                                                                  
#                                                                               
#  units = 'kg m-2 s-1'                                                         
#                                                                               
#  missing_value = -99999      



# nice, please use R to do the same




## Now I also have VISIT_S1_ra.nc in the same structure, given that NPP = GPP - RA, how should I create a new nc file (VISIT_S1_NPP.nc) for NPP in the same structure?

## the codes have this problem: Error in nc_dim("lon", length(lon)) : could not find function "nc_dim"



