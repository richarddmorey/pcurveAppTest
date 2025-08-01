#' R functions to support an online p curve app
#' Richard D. Morey, 2024/2025
#' Email: richarddmorey@gmail.com
#' See https://github.com/richarddmorey/pcurveAppTest

#' Compute  f(a) = log(1 - exp(-a))  stably
#' 
#' See https://github.com/cran/Rmpfr/blob/d41d4cd3982b0d0c5d7aabf78e1bbc4acca2d6f6/R/special-fun.R#L647
#' 
#' @param a numeric vector of positive values
#' @param cutoff  log(2) is optimal, see  Maechler (201x) .....
#' @return f(a) == log(1 - exp(-a)) == log1p(-exp(-a)) == log(-expm1(-a))
#' @author Martin Maechler, May 2002 .. Aug. 2011
#' @references Maechler(2012)
#' Accurately Computing log(1 - exp(-|a|)) Assessed by the Rmpfr package.
#' http://cran.r-project.org/web/packages/Rmpfr/vignettes/log1mexp-note.pdf
# MM: ~/R/Pkgs/Rmpfr/inst/doc/log1mexp-note.Rnw
log1mexp <- function(a, cutoff = log(2)) ## << log(2) is optimal >>
{
  if(has.na <- any(ina <- is.na(a))) {
    y <- a
    a <- a[ok <- !ina]
  }
  if(any(a < 0))## a == 0  -->  -Inf	(in both cases)
    warning("'a' >= 0 needed")
  tst <- a <= cutoff
  r <- a
  r[ tst] <- log(-expm1(-a[ tst]))
  r[!tst] <- log1p(-exp(-a[!tst]))
  if(has.na) { y[ok] <- r ; y } else r
}

#' Find a noncentrality parameter that yields a fixed probability to the right
#' of a truncation point.
#' 
#' The noncentrality parameter is approximated numerically, so some caution is
#' required in using it (e.g. don't assume that \eqn{Pr(x>t;ncp)=pr)}.
#' 
#' Function `find_ncp_uniroot` is a memoised version of this function 
#' so will be more efficient in repeated calls; it is expected that users 
#' will opt for `find_ncp_uniroot`.
#' 
#' Functions `find_ncp_uniroot_chi2` and `find_ncp_uniroot_f` are helper 
#' functions that do the actual work for the two possible test statistic types.
#'
#' @param Type of test statistic ('chi2' or 'f') 
#' @param pr Fixed probability to right of truncation point under alternative
#' @param alphaBound Fixed probability to right of truncation point under null
#' @param ... Parameters to pass to helper functions (not currently used)
#'
#' @returns The noncentrality parameter that yields `pr` probability to the
#'  right of the truncation point as a numeric value.
#' @export
#' 
find_ncp_uniroot0 = function(family=c('chi2','f'),pr=1/3,alphaBound=0.05,...){
  if(pr == alphaBound) return(0)
  if(pr<alphaBound) stop("pr cannot be less than alphaBound.")
  family = match.arg(family, c('chi2','f'))
  
  if(family=='chi2'){
    return(find_ncp_uniroot_chi2(pr,alphaBound,...))
  }else if(family=='f'){
    return(find_ncp_uniroot_f(pr,alphaBound,...))
  }else{
    stop('Unknown family.')
  }
}

#' @rdname find_ncp_uniroot0
#' @export 
find_ncp_uniroot = memoise::memoise(find_ncp_uniroot0)

#' @rdname find_ncp_uniroot0
#' @export 
find_ncp_uniroot_chi2 = function(pr=1/3,alphaBound=0.05,df){
  crit = qchisq(alphaBound, lower.tail = FALSE, df=df)
  fun = function(ncp0){
    ncp = ncp0 / (1-ncp0)
    pchisq(crit, ncp = ncp, lower.tail = FALSE, df = df) - pr
  }
  rt = uniroot(fun,interval = c(0,.9999))$root
  ncp = rt / (1-rt)
  return(ncp)
}

#' @rdname find_ncp_uniroot0
#' @export 
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


#' Construct a string representation of a statistical result appropriate for 
#' the p curve app.
#'
#' @param stat A test statistic type. Expected to be "z", "t", "f", "r", or 
#' "chi2" (lower case character vector of length 1)
#' @param df1 Degrees of freedom; either NA (stat "z"), single (stat "r",
#' "t", "chi2), or numerator if stat is "f" (numeric vector of length 1)
#' @param df2 Degrees of freedom for denominator (stat "f") or NA (numeric 
#' vector of length 1)
#' @param value The value of the test statistic (numeric vector of length 1)
#'
#' @returns A character vector of length 1 
#' @export
stat_string = function(stat, df1, df2, value){
    if(stat=='z') return(paste0("Z=",value))
    if(stat=='f') return(paste0("F(",df1,",",df2,")=",value))
    paste0(stat,"(",df1,")=",value)
}

#' Create a table of values p curve values, ready to compute a p curve analysis.
#' 
#' Function `pcurve_prep0` is for single results (so each argument should be
#' of length 1). `pcurve_prep` is the vectorized version (arguments should be of 
#' the same length, except pr and alphaBound, which should be of length 1).
#' 
#' Function `pcurve_prep0_LEV` computes the log p value for use in the test LEV.
#' 
#' @param stat Test statistic ("f","z","t","r", or "chi2")
#' @param df1 numeric NA (stat "z"), degrees of freedom (for stat "t", "r", or "chi2"; 
#' numerator df for stat "f") 
#' @param df2 numeric NA (stat "z", "t", "r", "chi2") or denominator df (stat "f")
#' @param value test statistic value
#' @param comment A character string comment (describing the result)
#' @param line The line number in the analysis from which result was taken
#' @param pr Probability to be used in test LEV
#' @param alphaBound alpha to be used for the right truncation value
#' @param ncp Noncentrality parameter for test LEV
#' @param ... Not used
#'
#' @returns `pcurve_prep0` returns a data frame with one row; `pcurve_prep` 
#' returns a data frame with one row per element of the first 6 arguments. 
#' @export
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
        ncp = find_ncp_uniroot('f', df1=df1, df2=df2, pr = pr, alphaBound = alphaBound)
        if(value>=0){
          lp = pf(value,df1,df2,lower.tail = FALSE, log.p = TRUE)
        }else{
          lp = NaN
        }
      }else{
        ncp = NaN
        lp = NaN
      }
    }else if(stat=='chi2'){
      if(df1 >= 1){
        ncp = find_ncp_uniroot('chi2', df=df1, pr = pr, alphaBound = alphaBound)
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

#' @rdname pcurve_prep0
#' @export   
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
    MoreArgs = list(
      pr = pr,
      alphaBound = alphaBound
    ),
    SIMPLIFY = FALSE
  )
  do.call(rbind, args = res)
}

#' @rdname pcurve_prep0
#' @export  
pcurve_prep0_LEV = memoise::memoise(
  function(stat, df1, df2, value, ncp, alphaBound = 0.05,...){
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
    # We recompute the LEV probability to prevent a bug that 
    # yields negative p values for some ncp values (due to the optimization)
    actual_pr = pchisq(crit,df1,ncp=ncp,lower.tail = FALSE)
    lp = log((pchisq(value,df1,ncp) - (1-actual_pr))/actual_pr)
  }else{
    stop('Unknown stat: ', stat)
  }
  return(lp)
})

#' Compute p curve tests from a table of values
#' 
#' Given a table of values (as output from function `pcurve_prep`), compute all 
#' pcurve tests. Function `pcurve` takes data frame with ONE row as input; function 
#' `pcurve_all` is vectorized over rows. 
#'
#' @param prep_table Data frame from `pcurve_prep` (single row for `pcurve`, 
#' whole data frame for `pcurve_all`)
#' @param alphaBound alpha for left truncation value
#' @param test The test to perform (either EV or LEV).
#'
#' @returns Returns a list containing two elements: `prep_table` which is the function 
#' input, and `tests` which is a data frame containing the test results.
#' @export
pcurve = function(prep_table, alphaBound = 0.05, test = c("EV","LEV")){
  test = match.arg(test, c("EV","LEV","LS"))
  prep_table$significant = prep_table$lp < log(alphaBound)
  test_string = paste0(test,alphaBound)
  if(test == "EV"){
    lp = prep_table$lp - log(alphaBound)
  }else if(test == "LS"){
    lp = log1mexp(-(prep_table$lp - log(alphaBound)))
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

#' @rdname pcurve
#' @export  
pcurve_all = function(prep_table){
  s = expand.grid(alphaBound=c(.05,.025), test = c("EV","LEV","LS"), stringsAsFactors=FALSE)
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


#' Create nice tables for website display
#'
#' @param prep_table A data frame from `pcurve_prep`
#' @param pvalcols A vector indicating which columns contain p values for formatting
#' @param prep_class A css class for the prep table of statistics 
#' @param test_class  A css class for the table of test results
#'
#' @returns A character vector containing two formatted HTML tables, one for
#' the test statistics and one for the test results.
#' @export
make_tables = function(prep_table, pvalcols = c(), prep_class, test_class){
  pc = pcurve_all(prep_table)
  tests = pc[['tests']]
  prep_table2 = pc[['prep_table']]
  return(c(
    xtab_prep(prep_table2, prep_class),
    xtab_tests(tests, pvalcols, test_class)
  ))
}

#' Create nicely-formatted prep (test statistic) HTML table
#'
#' This function uses the `prep_table` element from the output of `pcurve_all`
#' to create a nicely-formatted HTML table (using `knitr::kable`). Each element 
#' of the `tab` argument is a row of the table (one test statistic).
#'
#' @param tab A list of rows of the `prep_table` element of `pcurve_all`
#' @param class A css class to apply to the table
#'
#' @returns A character vector of length 1 containing an HTML table.
#' @importFrom knitr::kable
#' @export
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
  

#' Create a nicely-formatted table of p curve results
#'
#' This function uses the `tests` element from the output of `pcurve_all`
#' to create a nicely-formatted HTML table (using `knitr::kable`). 
#' 
#' @param tab A data frame of p curve test results 
#' @param pvalcols Names of columns that represent p values (for formatting) 
#' @param class A css class to apply to the table
#'
#' @returns A character vector of length one containing an HTML table
#' @importFrom knitr::kable
#' @export
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
}

#' Categorize p values by size for table formatting
#'
#' @param x Vector of p values
#' @param breaks Breaks for the categories
#' @param labels Labels for th categories
#'
#' @returns A character vector of categories of the p values. These categories will
#' be used as css classes for formatting.
#' @export
pval_cut_class = function(x, breaks = c(-Inf,0,.05,.1,Inf), labels = c('pnon','psignificant','pmarginal','pnon')){
  cut(x, breaks = breaks, labels = labels ) |> as.character()
}

#' Style p values
#' 
#' This function takes a vector of p values applies styling based on the
#' size of the p value.
#'
#' @param x Vector of p values
#' @param ... Arguments to pass to `pval_cut_class`
#'
#' @returns A character vector of HTML divs containing styled p values
#' @export
pval_style = function(x, ...){
  class = pval_cut_class(x, ...)
  x = prettyNum(x, digits = 4)
  paste0('<div class="pvaltab ',class,'">',x,'</div>')
}

#' Create a character representation from a logarithmic value
#' 
#' Computes a nicely-formatted character representation of `exp(x)` 
#' while preventing numerical overflow issues
#'
#' @param x Numerical value (the logarithm of a number)
#'
#' @returns A character vector
#' @export
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

#' Create table data appropriate for passing to d3.js (for ECDF plot)
#' 
#' This function is used for the ECDF function in the app. It takes the
#' information in the prep table, computes some important values, and 
#' formats it so that it can be used in the plot.
#'
#' @param prep_df Prep table as computed by `pcurve_prep`
#' @param alphaBound Value used for right truncation of test statistics
#' @param conf Confidence level for order statistic bounds and log-based test
#'
#' @returns A list containing two elements: the first is a data frame 
#' containing the main plot data,and the second element is a numeric 
#' vector containing the extra information to plot the test EV 
#' results (currently, at the bottom of the ECDF plot).
#' 
#' @export
make_plot_data = memoise::memoise(
  function(prep_df,alphaBound = .05, conf = .9){
    prep_df = prep_df[prep_df$lp<log(alphaBound),]
    prep_df = prep_df[order(prep_df$lp),]
    k = nrow(prep_df)
    if(k == 0){ 
      plotdata = data.frame()
      plotdata2 = c()
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
      plotdata2 = c(geo_mean,geo_mean_lo,geo_mean_up)
    }
    return(
      list(
        plotdata  = plotdata,
        plotdata2 = plotdata2
        )
    )
  }
)

