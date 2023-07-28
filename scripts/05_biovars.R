
func_biovars <- function(star, ind, dates){
  
  star %>% 
    st_apply(c(1,2), function(x){
      
      tx <- x[ind[1,1]:ind[1,2]]
      tn <- x[ind[2,1]:ind[2,2]]
      pr <- x[ind[1,1]:ind[1,2]]
      
      list(tx = tx, 
           tn = tn, 
           pr = pr) %>% 
        imap(function(v, i){
          
          tibble(v = v,
                 time = dates) %>% 
            group_by(year = year(dates),
                     month = month(dates)) -> tb
          
          if(i == "tx" | i == "tn"){
            tb %>% summarize(v = mean(v)) -> tb
          } else {
            tb %>% summarize(v = sum(v)) -> tb
          }
          
          tb %>% 
            pull(v) %>% 
            
            split(., ceiling(seq_along(.)/12))
          
        }) %>%
        transpose() -> vars_years
      
      map(vars_years, function(v){
        
        biovars(prec = v$pr,
                tmin = v$tn,
                tmax = v$tx) %>% unname()
        
      }) %>% 
        transpose() %>% 
        map(unlist) %>% 
        map(unname) %>% 
        unlist()
      
    },
    FUTURE = T,
    .fname = "time")
  
}
