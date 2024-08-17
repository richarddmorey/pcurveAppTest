
find_ncp_uniroot0 = function(family=c('chi2','f'),pr=1/3,alphaBound=0.05,...){
  family = match.arg(family, c('chi2','f'))
  
  if(family=='chi2'){
    return(find_ncp_uniroot_chi2(pr,alphaBound,...))
  }else if(family=='f'){
    return(find_ncp_uniroot_f(pr,alphaBound,...))
  }else{
    stop('Unknown family.')
  }
}

find_ncp_uniroot_chi2 = function(pr=1/3,alphaBound=0.05,df){
  crit = qchisq(alphaBound, lower.tail = FALSE, df=df)
  fun = function(ncp0){
    ncp = ncp0 / (1-ncp0)
    pchisq(crit, ncp = ncp, lower.tail = FALSE, df = df) - pr
  }
  rt = uniroot(fun,interval = c(0,.9999))$root
  ncp = rt / (1-rt)
  actual_pr = pchisq(crit, df=df, ncp=ncp, lower.tail = FALSE)
  return(ncp)
}

find_ncp_uniroot_f = function(pr=1/3,alphaBound=0.05,df1,df2){
  crit = qf(alphaBound, lower.tail = FALSE, df1 = df1, df2 = df2)
  fun = function(ncp0){
    ncp = ncp0 / (1-ncp0)
    pf(crit, ncp = ncp, lower.tail = FALSE, df1 = df1, df2 = df2) - pr
  }
  rt = uniroot(fun,interval = c(0,.9999))$root
  ncp = rt / (1-rt)
  return(ncp)
}

find_ncp_uniroot = memoise::memoise(find_ncp_uniroot0)

pcurve_prep0 = memoise::memoise(
  function(stat, df1, df2, value, comment, pr = 1/3, alphaBound = .05){
    stat = tolower(stat)
    # Convert stats to F or chi2
    if(stat=='z'){
      df1 = 1
      value = value^2
      stat = 'chi2'
    }else if(stat=='t'){
      df2 = df1
      df1 = 1
      value = value^2
      stat = 'f'
    }else if(stat=='r'){
      df2 = df1
      df1 = 1
      value = value^2/((1-value^2)/df2)
      stat = 'f'
    }
    if(stat=='f'){
      if(df1 >= 1 & df2 >= 1){
        ncp = find_ncp_uniroot('f',df1=df1,df2=df2,pr = pr, alphaBound = alphaBound)
        if(value>=0){
          lp = pf(value,df1,df2,lower.tail = FALSE,log.p = TRUE)
        }else{
          lp = NaN
        }
      }else{
        ncp = NaN
        lp = NaN
      }
    }else if(stat=='chi2'){
      if(df1 >= 1){
        ncp = find_ncp_uniroot('chi2',df=df1, pr = pr, alphaBound = alphaBound)
        if(value>=0){
          lp = pchisq(value,df1,lower.tail = FALSE,log.p = TRUE)
        }else{
          lp = NaN
        }
      }else{
        ncp = NaN
        lp = NaN
      }
    }else{
      stop('Invalid stat: ', stat)
    }
    return(
      data.frame(
        stat=stat,
        df1=df1,
        df2=df2,
        value=value,
        comment=comment,
        ncp=ncp,
        lp = lp
        )
      )
  })
  
pcurve_prep = function(stat, df1, df2, value, comment, pr=1/3, alphaBound=.05){
  k0 = length(stat)
  res = mapply(
    FUN = pcurve_prep0,
    stat = stat,
    df1 = df1,
    df2 = df2,
    value = value,
    comment = comment,
    SIMPLIFY = FALSE
  )
  do.call(rbind, args = res)
}

pcurve_prep0_EV = function(stat, df1, df2, value, alphaBound = 0.05, ...){
  lp = switch(
    tolower(stat),
    chi2 = ifelse(df1>=1 & value >= 0, pchisq(value,df1,lower.tail = FALSE, log.p = TRUE), NaN),
    f = ifelse(df1>=1 & df2>=1 & value >= 0, pf(value,df1,df2,lower.tail = FALSE, log.p = TRUE), NaN),
    stop('Unknown stat: ', stat)
  )
  return(lp - log(alphaBound))
}

pcurve_prep0_LEV = memoise::memoise(function(stat, df1, df2, value, ncp, alphaBound = 0.05,...){
  stat = tolower(stat)
  if(stat == 'f'){
    if(df1<1 | df2<1 | value < 0) return(NaN)
    crit = qf(alphaBound, df1, df2, lower.tail = FALSE)
    if(value<crit) return(NaN)
    actual_pr = pf(crit,df1,df2,ncp=ncp,lower.tail = FALSE)
    lp = log((pf(value,df1,df2,ncp) - (1-actual_pr))/actual_pr)
  }else if(stat == 'chi2'){
    if(df1<1 | value < 0) return(NaN)
    crit = qchisq(alphaBound, df1, lower.tail = FALSE)
    if(value<crit) return(NaN)
    actual_pr = pchisq(crit,df1,ncp=ncp,lower.tail = FALSE)
    lp = log((pchisq(value,df1,ncp) - (1-actual_pr))/actual_pr)
  }else{
    stop('Unknown stat: ', stat)
  }
  return(lp)
})

pcurve = function(prep_table, alphaBound = 0.05, test = c("EV","LEV")){
  test = match.arg(test, c("EV","LEV"))
  prep_table$significant = prep_table$lp < log(alphaBound)
  if(test == "EV"){
    lp = prep_table$lp - log(alphaBound)
  }else{
    lp = sapply(1:nrow(prep_table),function(i){
      v = prep_table[i,]
      v$alphaBound = alphaBound
      do.call(pcurve_prep0_LEV,args = v)
    })
  }
  k0 = nrow(prep_table)
  lp = lp[prep_table$significant]
  k = sum(prep_table$significant)
  if(k > 0){
  # Probit
    lp |>
      qnorm(log.p=TRUE) |>
      sum() -> sumz
    teststat_probit = sumz/sqrt(k)
    pval_probit = pnorm(teststat_probit)
    # Log
    sumx = sum(lp) 
    teststat_log = -2*sumx
    pval_log = pchisq(teststat_log,2*k,lower.tail = FALSE)
    tests = data.frame(
      test = test,
      alphaBound = alphaBound,
      teststat_log = teststat_log,
      pval_log = pval_log,
      teststat_probit = teststat_probit,
      pval_probit = pval_probit,
      k_sig = k,
      k_total = k0
    )
  }else{
    tests = NULL
  }
  return(list(prep_table = prep_table, tests = tests))
}

xtab = function(tab, class){
  xtable::xtable(tab, digits = c(0,NA,3,2,4,2,4,0,0)) |>
    print(
      type = "html", 
      print.results = FALSE,
      include.rownames=FALSE,
      html.table.attributes = paste0('class="',class,'"')
    )
}