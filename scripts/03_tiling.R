# size of chunk (pixels in each dim)
sz <- 50

dir_down %>% 
  list.files(full.names = T) %>% 
  sample(1) -> f

f %>% 
  read_stars(proxy = T) -> s_proxy

# lon *****
s_proxy %>% 
  dim() %>% 
  .[1] %>% 
  seq_len() -> d_lon

round(length(d_lon)/sz) -> n_lon

split(d_lon, 
      ceiling(seq_along(d_lon)/(length(d_lon)/n_lon))) %>% 
  map(~c(head(.x, 1), tail(.x, 1))) -> lon_chunks

# lat *****
s_proxy %>% 
  dim() %>% 
  .[2] %>% 
  seq_len() -> d_lat

round(length(d_lat)/sz) -> n_lat

split(d_lat, 
      ceiling(seq_along(d_lat)/(length(d_lat)/n_lat))) %>% 
  map(~c(head(.x, 1), tail(.x, 1))) -> lat_chunks


# ******


f %>% 
  read_ncdf(ncsub = cbind(start = c(1, 1, 1),
                          count = c(NA,NA,1))) %>% 
  suppressMessages() %>% 
  adrop() -> s_proxy

imap_dfr(lon_chunks, function(lon_ch, lon_i){
  imap_dfr(lat_chunks, function(lat_ch, lat_i){
    
    s_proxy %>% 
      slice(lon, lon_ch[1]:lon_ch[2]) %>% 
      slice(lat, lat_ch[1]:lat_ch[2]) -> s_proxy_sub
    
    st_warp(land_rast %>%
              st_set_dimensions(which = c(1,2),
                                names = c("lon", "lat")),
            s_proxy_sub) -> land_rast_sub
    
    c(s_proxy_sub, land_rast_sub) %>% 
      as_tibble() %>%
      rename(var = 3) %>% 
      filter(!is.na(var)) %>%
      summarize(prop = sum(!is.na(a))/n()) %>% 
      pull(prop) -> prop
    
    tibble(
      cover = ifelse(prop < 0.005, F, T),
      lon_ch = lon_i,
      lat_ch = lat_i
    )
    
  })
}) %>% 
  filter(cover == TRUE) %>% 
  mutate(r = row_number()) -> chunks_ind


# ******

rm(f, s_proxy, d_lon, n_lon, d_lat, n_lat, sz)
