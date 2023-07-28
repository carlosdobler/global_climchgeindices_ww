# MISSING AUS NOR 


# source("~/00-mount.R")

library(climdex.pcic)
library(dismo)
source("scripts/00-set-up.R")

source("scripts/04_etccdi.R")
source("scripts/05_biovars.R")
source("scripts/write_nc.R")

plan(multicore, workers = availableCores() - 2)

st_read("~/bucket_mine/misc_data/ne_50m_land/ne_50m_land.shp") %>%
  mutate(a = 1) %>%
  select(a) %>%
  st_rasterize(st_as_stars(st_bbox(), dx = 0.2, dy = 0.2, values = NA)) -> land_rast


# DOMAIN LOOP
dom <- c("AFR", "AUS", "CAM", "CAS", "EAS", "EUR", "NAM", "SAM", "SEA", "WAS")[5]

# master tables
c("maximum_temperature", "minimum_temperature", "precipitation") %>% set_names() %>% 
  map(function(var){
    
    pos_model <- 3
    pos_date <- 9
    
    dir_datasrc <- str_glue("~/bucket_risk/RCM_regridded_data/REMO2015/{dom}/daily/{var}/")
    
    tibble(file = dir_datasrc %>%
             list.files()) %>%
      mutate(
        model = file %>%
          str_split("_", simplify = T) %>%
          .[ , pos_model],
        
        t_i = file %>%
          str_split("_", simplify = T) %>%
          .[ , pos_date] %>%
          str_sub(end = 8),
        
        t_f = file %>%
          str_split("_", simplify = T) %>%
          .[ , pos_date] %>%
          str_sub(start = 10, end = 17)
        
      ) %>%
      filter(t_f != "")
    
  }) -> tb_files



# MODEL LOOP
for(mod in c("Had", "MPI", "Nor")[3]){                                                                 # **************************
  
  print(str_glue("[{dom}] [{mod}] STARTED"))
  tic(str_glue("      Done with model {mod} of domain {dom}")) # 1
  
  # number of years
  if(mod == "Had"){
    yrs <- 130
  } else {
    yrs <- 131
  }
  
  
  # DOWNLOAD FILES ******
  
  print(str_glue(" "))
  print(str_glue("[{dom}] [{mod}] Downloading files"))
  tic(str_glue("   Done downloading")) # 3
  
  dir_down <- str_glue("~/pers_disk/{dom}") # create download directory
  dir.create(dir_down)
  source("scripts/02_download.R")
  
  toc() # 3
  
  
  # CREATE DATES VECTOR *****
  
  if(mod == "Had"){
    as.PCICt("1970-01-01", cal = "360") + (seq(0, yrs*360-1) * (60*60*24)) -> dates
  } else {
    seq(as_date("19700101"), as_date("21001231"), by = "1 day") %>% 
      as_datetime() %>% as.PCICt(cal = "gregorian") -> dates
  }
  
  
  # CREATE TILES INDICES *****
  
  source("scripts/03_tiling.R")
  
  
  # CALCULATE INDICES *****
  
  case_when(mod == "Had" ~ 2014,
            mod == "MPI" ~ 2006,
            mod == "Nor" ~ 2019) -> mid_yr
  
  # loop across chunks
  dir_tmp_chunks <- "~/pers_disk/tmp_chunks"
  dir.create(dir_tmp_chunks)
  
  pwalk(chunks_ind[12:52,], function(lon_ch, lat_ch, r, ...){                                               # ***************************
    
    print(str_glue(" "))
    print(str_glue("[{dom}] [{mod}] Processing chunk {r} / {nrow(chunks_ind)}"))
    tic(str_glue("      Done processing chunk")) # 4
    
    # import all variables
    
    dir_tmp_vars <- "~/pers_disk/tmp_vars"
    dir.create(dir_tmp_vars)
    
    walk(c("tasmax", "tasmin", "pr"), function(v){
      
      # dir_down %>% 
      #   list.files(full.names = T) %>% 
      #   .[str_detect(., v)] -> files_vector
      
      tb_files %>%
        map_dfr(~.x) %>%
        filter(str_detect(file, v)) %>%
        filter(str_detect(file, mod)) %>% 
        arrange(t_i) %>% 
        pull(file) %>% 
        {str_c(dir_down,"/", .)} -> files_vector
      
      imap(files_vector, function(f, i){
        
        tic(str_glue("   Imported {v} - {i} / {length(files_vector)}")) # 5
        
        read_ncdf(f, ncsub = cbind(start = c(lon_chunks[[lon_ch]][1],
                                             lat_chunks[[lat_ch]][1],
                                             1),
                                   count = c(lon_chunks[[lon_ch]][2] - lon_chunks[[lon_ch]][1] +1,
                                             lat_chunks[[lat_ch]][2] - lat_chunks[[lat_ch]][1] +1,
                                             NA))) %>%
          suppressMessages() -> s_imp
        
        toc() # 5
        
        return(s_imp)
        
      }) -> s
      
      s %>%
        do.call(c, .) %>%
        setNames("var") -> s
      
      if(str_detect(v, "tas")){
        s %>%
          mutate(var = var %>% set_units(degC)) -> s
      } else {
        s %>%
          mutate(var = var %>% set_units(kg/m^2/d)) -> s
      }
      
      st_set_dimensions(s, "time", values = dates) -> s
      
      print(str_glue("      Saving"))
      tic(str_glue("      Done saving"))
      saveRDS(s, str_glue("{dir_tmp_vars}/tmp_star_{v}.rds"))
      toc()
      
    })
    
    # stack three variables along time
    list.files(dir_tmp_vars, full.names = T) %>%
      {c(.[str_detect(., "tasmax")], .[str_detect(., "tasmin")], .[str_detect(., "pr")])} %>% 
      map(readRDS) -> s
    
    unlink(dir_tmp_vars, recursive = T)
    
    s %>% 
      map(mutate, var = var %>% set_units(NULL)) %>% 
      {do.call(c, c(., along = 3))} -> s_stack
    
    # add lat to determine northern hemisphere
    s[[1]] %>%
      filter(time == as_date("1970-01-01")) %>%
      st_dim_to_attr() %>%
      dplyr::select("lat") %>% 
      {c(s_stack, ., along = 3)} -> s_stack
    
    
    # *****
    
    # calculate ETCCDI indices
    
    print(str_glue(" "))
    print(str_glue("   Calculating etccdi"))
    tic(str_glue("   Done with etccdi")) # 6.1
    
    cbind(seq(1, length(dates)*3, length(dates)),
          seq(1, length(dates)*3, length(dates)) + length(dates)-1) -> ind
    
    func_ecctdi(s_stack, ind, dates) -> s_etccdi
    
    # save ETCCDI indices
    
    print(str_glue("   Saving etccdi"))
    
    walk2(seq(1, yrs*27, yrs), 
          c("fd", "su", "id", "tr",
            "gsl",
            "txx", "tnx", "txn", "tnn",
            "tn10p", "tx10p", "tn90p", "tx90p",
            "wsdi", "csdi", "dtr",
            "rx1day", "rx5day",
            "sdii", "r10mm", "r20mm", "rnnmm",
            "cdd", "cwd",
            "r95ptot", "r99ptot", "prcptot"),
          
          function(t, i){
            
            s_etccdi %>% 
              slice(time, t:(t+yrs-1)) %>% 
              st_set_dimensions("time", values = dates %>% .[month(.) == 1 & day(.) == 1]) %>% 
              setNames(i) %>% 
              
              saveRDS(str_glue("{dir_tmp_chunks}/star_{i}_{lon_ch}_{lat_ch}.rds"))
            
          })
    
    toc() # 6.1
    
    
    # *****
    
    # calculate biovars
    
    print(str_glue("   Calculating biovars"))
    tic(str_glue("   Done with biovars")) # 6.2
    
    func_biovars(s_stack, ind, dates) -> s_biovar
    
    # save biovars
    
    print(str_glue("   Saving biovars"))
    
    walk2(seq(1, yrs*19, yrs), 
          str_glue("biovar{seq(1,19) %>% str_pad(2, 'left', '0')}"),
          
          function(t, i){
            
            s_biovar %>% 
              slice(time, t:(t+yrs-1)) %>% 
              st_set_dimensions("time", values = dates %>% .[month(.) == 1 & day(.) == 1]) %>% 
              setNames(i) %>% 
              
              saveRDS(str_glue("{dir_tmp_chunks}/star_{i}_{lon_ch}_{lat_ch}.rds"))
            
          })
    
    toc() # 6.2
    
    print(str_glue(" "))
    
    toc() # 4
    
    print(str_glue(" "))
    
  }) # end of chunk loop
  
  
  # MOSAICK *****
  
  print(str_glue(" "))
  print(str_glue("[{dom}] [{mod}] Mosaicking"))
  tic(str_glue("   Done mosaicking")) # 7
  
  if(mod == "Had") last_yr <- 2099 else last_yr <- 2100
  
  # loop across variables
  c("fd", "su", "id", "tr",
    "gsl",
    "txx", "tnx", "txn", "tnn",
    "tn10p", "tx10p", "tn90p", "tx90p",
    "wsdi", "csdi", "dtr",
    "rx1day", "rx5day",
    "sdii", "r10mm", "r20mm", "rnnmm",
    "cdd", "cwd",
    "r95ptot", "r99ptot", "prcptot",
    str_glue("biovar{seq(1,19) %>% str_pad(2, 'left', '0')}")) %>% 
    
    walk(function(v){
      
      print(str_glue("Processing variable {v}"))

      
      print(str_glue("   Reading chunks"))
      
      list.files(dir_tmp_chunks, full.names = T) %>% 
        .[str_detect(., v)] %>% 
        map(readRDS) -> chunks
      
      print(str_glue("   Mosaicking chunks"))
      
      map(chunks, function(s){
        
        st_set_dimensions(s, "lon", 
                          values = st_get_dimension_values(s, "lon") %>% round(1)) -> s
        
        st_set_dimensions(s, "lat", 
                          values = st_get_dimension_values(s, "lat") %>% round(1)) -> s
        
        s %>% st_set_crs(4326) -> s
        
        s %>% 
          as("Raster")
        
      }) %>% 
        do.call(merge, .) %>% 
        st_as_stars() %>% 
        st_set_dimensions("band", 
                          values = dates %>% .[month(.) == 1 & day(.) == 1], 
                          name = "time") %>% 
        setNames(v) -> var_mosaic
      
      print(str_glue("   Saving mosaic"))
      
      func_write_nc(var_mosaic,
                    str_glue("~/bucket_mine/results/global_climchgeindices_ww/{v}_{mod}_{dom}_annual_1970_{last_yr}.nc"))
      
    }) # end of loop across variables
  
  toc() # 7
  
  unlink(dir_tmp_chunks, recursive = T)
  unlink(dir_down, recursive = T)
  
  print(str_glue(" "))
  toc() # 1

} # end of model loop


