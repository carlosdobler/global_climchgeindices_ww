
func_ecctdi <- function(star, ind, dates){
  
  star %>% 
    st_apply(c(1,2), function(x){
      
      north <- ifelse(tail(x, 1) < 0, F, T)
      
      tx <- x[ind[1,1]:ind[1,2]]
      tn <- x[ind[2,1]:ind[2,2]]
      pr <- x[ind[1,1]:ind[1,2]]
      
      climdexInput.raw(tmax = tx,
                       tmin = tn,
                       prec = pr,
                       tmax.dates = dates,
                       tmin.dates = dates,
                       prec.dates = dates,
                       base.range = c(mid_yr-10, mid_yr+10),
                       northern.hemisphere = north
      ) -> ci
      
      fd <- climdex.fd(ci) %>% unname() # 1
      su <- climdex.su(ci) %>% unname() # 2
      id <- climdex.id(ci) %>% unname() # 3
      tr <- climdex.tr(ci) %>% unname() # 4
      
      gsl <- climdex.gsl(ci) %>% unname() # 5
      
      txx <- climdex.txx(ci, "annual") %>% unname() # 6
      tnx <- climdex.tnx(ci, "annual") %>% unname() # 7
      txn <- climdex.txn(ci, "annual") %>% unname() # 8
      tnn <- climdex.tnn(ci, "annual") %>% unname() # 9
      
      tn10p <- climdex.tn10p(ci, "annual") %>% unname() # 10
      tx10p <- climdex.tx10p(ci, "annual") %>% unname() # 11
      tn90p <- climdex.tn90p(ci, "annual") %>% unname() # 12
      tx90p <- climdex.tx90p(ci, "annual") %>% unname() # 13
      
      wsdi <- climdex.wsdi(ci) %>% unname() # 14
      csdi <- climdex.csdi(ci) %>% unname() # 15
      dtr <- climdex.dtr(ci, "annual") %>% unname() # 16
      
      rx1day <- climdex.rx1day(ci, "annual") %>% unname() # 17
      rx5day <- climdex.rx5day(ci, "annual") %>% unname() # 18
      
      sdii <- climdex.sdii(ci) %>% unname() # 19
      r10mm <- climdex.r10mm(ci) %>% unname() # 20
      r20mm <- climdex.r20mm(ci) %>% unname() # 21
      rnnmm <- climdex.rnnmm(ci, 1) %>% unname() # 22
      
      cdd <- climdex.cdd(ci) %>% unname() # 23
      cwd <- climdex.cwd(ci) %>% unname() # 24
      
      r95ptot <- climdex.r95ptot(ci) %>% unname() # 25
      r99ptot <- climdex.r99ptot(ci) %>% unname() # 26
      prcptot <- climdex.prcptot(ci) %>% unname() # 27
      
      c(fd, su, id, tr,
        gsl,
        txx, tnx, txn, tnn,
        tn10p, tx10p, tn90p, tx90p,
        wsdi, csdi, dtr,
        rx1day, rx5day,
        sdii, r10mm, r20mm, rnnmm,
        cdd, cwd,
        r95ptot, r99ptot, prcptot)
      
    },
    FUTURE = T,
    .fname = "time") 

}
