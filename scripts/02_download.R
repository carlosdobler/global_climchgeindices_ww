
# download all variables for a given model
tb_files %>% 
  map(filter, str_detect(model, mod)) %>%
  iwalk(function(tb, i){
    
    tb %>%
      pull(file) %>% 
      
      walk(function(f){
        
        orig <- str_glue("gs://cmip5_data/RCM_regridded_data/REMO2015/{dom}/daily/{i}/{f}")
        
        dest <- str_glue("{dir_down}/{f}")
        
        system(str_glue("gsutil -m cp {orig} {dest}"),
               ignore.stdout = TRUE, ignore.stderr = TRUE)
        
      })
      
  })