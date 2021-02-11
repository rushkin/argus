#' flag
#'
#' Compare the latest in historical data with the prediction range from the earlier data
#'
#' @param df tibble, as multiple rows produced by to_row()
#' @param level value between 0 and 1, confidence level used for prediction confidence interval
#'
#' @return a named list (names are variables from df, other than timestamp), each element is a list with components fit (prediction value), lwr, upr (lower and upper limits of prediction confindence interval), obs (observed value), flag (1 if obs>upr, -1 if obs<lwr, 0 otherwise).
#' @export
#'
flag=function(df, level=0.9){

  eps=1e-6

  suppressWarnings({
  df=df%>%arrange(desc(timestamp))%>%
    mutate_all(as.numeric)
  })

  tr=df%>%slice(-1)
  va=df%>%slice(1)

  m=lapply(names(df)[names(df)!='timestamp'],
           function(n){
             z=tr%>%select(!!n, timestamp)%>%drop_na()
             if(nrow(z)==0) return(list(flag=0))
             mm=lm(as.formula(paste(n,'~ timestamp')), z)%>%
               predict(va, interval='prediction', level=level)%>%
               as_tibble()%>%
               mutate(obs=va[[n]])
             if(!is.na(mm$upr)){
               if(va[[n]]>((1+eps)*mm$upr)) return(c(list(flag=1), as.list(mm)))
             }
             if(!is.na(mm$lwr)){
               if(va[[n]]<((1-eps)*mm$lwr)) return(c(list(flag=-1), as.list(mm)))
             }
             return(c(list(flag=0), as.list(mm)))
           }
  )%>%setNames(names(df)[names(df)!='timestamp'])

  return(m)

}
