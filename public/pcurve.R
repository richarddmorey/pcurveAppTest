
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

stat_string = function(stat, df1, df2, value){
    if(stat=='z') return(paste0("Z=",value))
    if(stat=='f') return(paste0("F(",df1,",",df2,")=",value))
    paste0(stat,"(",df1,")=",value)
}

pcurve_prep0 = memoise::memoise(
  function(stat, df1, df2, value, comment, line, pr = 1/3, alphaBound = .05){
    stat = tolower(stat)
    string = stat_string(stat, df1, df2, value)
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
        line=line,
        string=string,
        ncp=ncp,
        lp = lp
        )
      )
  })
  
pcurve_prep = function(stat, df1, df2, value, comment, line, pr=1/3, alphaBound=.05){
  k0 = length(stat)
  res = mapply(
    FUN = pcurve_prep0,
    stat = stat,
    df1 = df1,
    df2 = df2,
    value = value,
    comment = comment,
    line = line,
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

pcurve_all = function(prep_table){
  s = expand.grid(alphaBound=c(.05,.025), test = c("EV","LEV"), stringsAsFactors=FALSE)
  res = mapply(FUN = pcurve, 
    test = s$test, alphaBound = s$alphaBound,
    MoreArgs = list(prep_table = prep_table),
    SIMPLIFY = FALSE
    ) 
  tests = lapply(res, \(el) el$tests)
  prep_table2 = res[[which(s$test == 'EV' & s$alphaBound == 0.05)]]$prep_table

  x = do.call(rbind, tests)
  rownames(x) = NULL
  return(list(
    prep_table = prep_table2,
    tests = x
  ))
}

pcurve = function(prep_table, alphaBound = 0.05, test = c("EV","LEV")){
  test = match.arg(test, c("EV","LEV"))
  prep_table$significant = prep_table$lp < log(alphaBound)
  test_string = paste0(test,alphaBound)
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
      qnorm(log.p=TRUE) -> qn
    contribution_probit = qn / sqrt(k)
    teststat_probit = sum(contribution_probit)
    pval_probit = pnorm(teststat_probit)
    # Log
    contribution_log = -2*lp
    teststat_log = sum(contribution_log)
    pval_log = pchisq(teststat_log,2*k,lower.tail = FALSE)
  
    prep_table$contr_log = prep_table$contr_probit = NA
    prep_table$contr_log[prep_table$significant] = contribution_log
    prep_table$contr_probit[prep_table$significant] = contribution_probit
    
    tests = data.frame(
      test = test,
      alphaBound = alphaBound,
      teststat_log = teststat_log,
      pval_log = pval_log,
      teststat_probit = teststat_probit,
      pval_probit = pval_probit,
      k_total = k0,
      k_sig = k
    )
  }else{
    tests = data.frame(
      test = test,
      alphaBound = alphaBound,
      teststat_log = NA,
      pval_log = NA,
      teststat_probit = NA,
      pval_probit = NA,
      k_total = k0,
      k_sig = k
      )
  }
  return(list(prep_table = prep_table, tests = tests))
}

make_tables = function(prep_table, pvalcols = c(), prep_class, test_class){
  pc = pcurve_all(prep_table)
  tests = pc[['tests']]
  prep_table2 = pc[['prep_table']]
  return(c(
    xtab_prep(prep_table2, prep_class),
    xtab_tests(tests, pvalcols, test_class)
  ))
}

xtab_prep = function(tab, class){
  tab$p = sapply(tab$lp, expString)
  tab$sig = tab$lp < log(.05)
  tab = tab[,c("line","string", "comment", "p", "sig","contr_log","contr_probit","ncp")]
  tab$sig = ifelse(tab$sig,"✅","❌")
  knitr::kable(tab, 
               format = "html", 
               digits = c(0,0,0,0,0,3,3,3),
               align = c('r',rep('l',4),rep('r',3)),
               row.names = FALSE,
               escape=FALSE,
               col.names = c(
                 'Line',
                 'Input',
                 'Comment',
                 '<span class="nott">p</span>',
                 'Sig.?',
                 'Fisher',
                 'Stouffer',
                 'LEV NCP'
               )
  )
}
  

xtab_tests = function(tab, pvalcols = c(), class){
  for(col in pvalcols){
    tab[,col] = pval_style(tab[,col])
  }
  knitr::kable(tab, 
    format = "html", 
    digits = c(NA,3,2,4,2,4,0,0),
    align = c('l',rep('r',7)),
    escape=FALSE,
    col.names = c(
      'Test',
      '&alpha;',
      'Fisher &chi;<sup>2</sup>',
      'Fisher <i>p</i>',
      'Stouffer Z',
      'Stouffer <i>p</i>',
      '# studies',
      '# sig.')
    )

#  xtable::xtable(tab, digits = c(0,NA,3,2,4,2,4,0,0)) |>
#    print(
#      type = "html", 
#      print.results = FALSE,
#      sanitize.text.function = \(x) x,
#      include.rownames=FALSE,
#      html.table.attributes = paste0('class="',class,'"')
#    )
}

pval_cut_class = function(x, breaks = c(-Inf,0,.05,.1,Inf), labels = c('pnon','psignificant','pmarginal','pnon')){
  cut(x, breaks = breaks, labels = labels )|> as.character()
}

pval_style = function(x, ...){
  class = pval_cut_class(x, ...)
  x =prettyNum(x, digits = 4)
  paste0('<div class="pvaltab ',class,'">',x,'</div>')
}

expString <- function(x){
  if(is.na(x)) return("NA")
  doubleBase = .Machine$double.base
  toBase10log = x / log(10)
  toBaselog = x / log(doubleBase)

  numMax = .Machine$double.max.exp
  numMin = .Machine$double.min.exp

  if(toBaselog>numMax){
    first <- prettyNum( 10 ^ (toBase10log - floor(toBase10log)) )
    second <- prettyNum( floor(toBase10log) )
    return( paste( first, "e+", second, sep="" ) )
  }else if(toBaselog < numMin){
    first <- prettyNum( 10 ^ (1 - (ceiling(toBase10log) - toBase10log)) )
    second <- prettyNum( ceiling(toBase10log)-1 )
    return( paste( first, "e", second, sep="" ) )
  }else{
    return( prettyNum( exp(x) ) )
  }
}


make_plot_data = function(prep_df,alphaBound = .05, conf = .9){
  prep_df = prep_df[prep_df$lp<log(alphaBound),]
  prep_df = prep_df[order(prep_df$lp),]
  k = nrow(prep_df)
  if(k == 0){ 
    plotdata = data.frame()
  }else{
    pval = exp(prep_df$lp)
    p_string = sapply(prep_df$lp, expString)
    Fp     = 1:k/k
    lo = qbeta((1-conf)/2,1:k,k-1:k+1)*.05
    up = qbeta(1-(1-conf)/2,1:k,k-1:k+1)*.05
    med = qbeta(.5,1:k,k-1:k+1)*.05
    geo_mean = 10^(sum(log10(pval))/k)
    geo_mean_lo =  10^((qchisq((1-conf)/2,2*k) - 2*k*log(alphaBound)) / (-2*k/log10(exp(1))))
    geo_mean_up =  10^((qchisq(1-(1-conf)/2,2*k) - 2*k*log(alphaBound)) / (-2*k/log10(exp(1))))
    plotdata = data.frame(
      pval = pval,
      p_string = p_string,
      Fp = Fp,
      lo = lo,
      med = med,
      up = up,
      comment = prep_df$comment,
      input_string = prep_df$string,
      line = prep_df$line
    )
    assign("plotdata2",c(geo_mean,geo_mean_lo,geo_mean_up),.GlobalEnv)
  }
  assign("plotdata", plotdata, .GlobalEnv)
  return(plotdata)
 }

