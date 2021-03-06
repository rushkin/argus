#' describe
#'
#' Make a summary dataframe for a dataframe.
#'
#' @param dat dataframe or tibble
#' @param screen_vars vector of variable names whose values should not be show in the summary
#' @param n_example_values max number of value examples to show
#' @param max_nchar max nchar length of value examples
#' @param round_digits digits for rounding displayed numbers
#' @param show_size logical, whether to include the data size at the top of the summary
#'
#' @return a tibble
#' @export
#'
#' @examples describe(tibble(x=c(1,2,3)))
describe=function(dat,
                  screen_vars=c('username','ip'),
                  n_example_values=5,
                  max_nchar=25,
                  round_digits=2,
                  show_size=TRUE
){

  require(tidyverse)
suppressWarnings({
  round_if_num_=function(x, digits){
    if(class(x)[1]=='numeric'){
      prettyNum(round(x, digits),big.mark=',')
    }else{
      as.character(x)
    }
  }

  ans=dat%>%
    summarize(across(everything(), function(x) class(x)[1]))%>%gather(value='type')%>%
    mutate(`distinct values`=NA, min=NA, median=NA, max=NA, `missingness`=NA, values=NA)

  temp=dat%>%
    summarize(across(ans%>%filter(type %in% c('numeric','Date','POSIXct'))%>%pull(key),
                     function(x){
                       c(
                         round_if_num_(min(x, na.rm=TRUE),round_digits),
                         round_if_num_(median(x,na.rm=TRUE),round_digits),
                         round_if_num_(max(x, na.rm=TRUE),round_digits),

                         prettyNum(signif(100*mean(is.na(x)),round_digits), big.mark=',')
                       )
                     }))

  if(all(dim(temp)>0)){
    temp=temp%>%t()%>%as_tibble(rownames=NA)%>%
      setNames(c('min1','median1','max1','missingness1'))%>%
      rownames_to_column('key')
    ans=ans%>%left_join(temp, by='key')%>%
      mutate(
        min=coalesce(min, min1),
        median=coalesce(median, median1),
        max=coalesce(max, max1),
        missingness=coalesce(missingness, missingness1)
      )%>%
      select(names(ans))
  }

  temp=dat%>%
    summarize(across(ans%>%filter(type %in% c('character'))%>%pull(key),
                     function(x){
                       z=sort(unique(x))
                       z=ifelse(nchar(z)>max_nchar, paste0(substring(z, 1, max_nchar),'...'), z)
                       nch=nchar(x)

                       if(length(z)==0){
                         exmpl=''
                       }else if(length(z)<=n_example_values){
                         exmpl=paste0('\"',paste(z, collapse='\", \"'),'\"')
                       }else{
                         exmpl=paste0('\"',paste(z[seq_len(n_example_values)], collapse='\", \"'),'\" ...')
                       }
                       c(
                         length(z),
                         paste0(prettyNum(min(nch, na.rm=TRUE), big.mark=','),' char'),
                         paste0(prettyNum(median(nch, na.rm=TRUE), big.mark=','),' char'),
                         paste0(prettyNum(max(nch, na.rm=TRUE), big.mark=','),' char'),
                         prettyNum(signif(100*mean(is.na(x)),round_digits), big.mark=','),
                         exmpl
                       )
                     }))

  if(all(dim(temp)>0)){
    temp=temp%>%t()%>%as_tibble(rownames=NA)%>%
      setNames(c('n values1','min1','median1','max1','missingness1', 'values1'))%>%
      rownames_to_column('key')
    ans=ans%>%left_join(temp, by='key')%>%
      mutate(
        min=coalesce(min, min1),
        median=coalesce(median, median1),
        max=coalesce(max, max1),
        missingness=coalesce(missingness, missingness1),
        `distinct values`=coalesce(`distinct values`,`n values1`),
        values=coalesce(values, values1)
      )%>%
      select(names(ans))%>%
      mutate(values=ifelse(key %in% screen_vars, NA, values))
  }

  temp=dat%>%
    summarize_if(is.logical,
                 function(x){
                   c(
                     paste0('\"TRUE\" fraction ',signif(mean(x,na.rm=TRUE), round_digits)),
                     prettyNum(signif(100*mean(is.na(x)),round_digits), big.mark=',')
                   )
                 })

  if(all(dim(temp)>0)){
    temp=temp%>%t()%>%as_tibble(rownames=NA)%>%
      setNames(c('values1','missingness1'))%>%
      rownames_to_column('key')
    ans=ans%>%left_join(temp, by='key')%>%
      mutate(
        missingness=coalesce(missingness, missingness1),
        values=coalesce(values, values1)
      )%>%
      select(names(ans))
  }

  ans=ans%>%rename(c('missingness %'='missingness'))

  if(show_size){
    ans=tibble(key=paste0('DATA SIZE: ',paste(prettyNum(dim(dat), big.mark=','), collapse=' x ')))%>%
      bind_rows(ans)%>%
      select(names(ans))
  }
})
  return(ans)
}


#' to_row
#'
#' Convert the summary table (as produced by describe()) to a 1-row wide format, adding the timestamp column
#'
#' @param df - table produced by describe()
#' @param timestamp - if not given, will be Sys.time()
#'
#' @return a tibble
#' @export
#'
#' @examples to_row(describe(tibble(x=c(1,2,3))))
to_row=function(df, timestamp=NULL){

  require(tidyverse)

  if(is.null(timestamp)) timestamp=Sys.time()
  suppressWarnings({
  df%>%
    drop_na(type)%>%
    select(-type)%>%
    gather(key='k', value='v', -key)%>%
    mutate(key=paste0(key,'_',k)%>%
             str_replace_all('[[:punct:]]','_')%>%
             str_replace_all(' ','_')%>%
             str_replace_all('_+','_')
    )%>%
    select(-k)%>%
    mutate(v=v%>%str_remove_all(' char$')%>%str_remove_all(','))%>%
    spread(key='key', value='v')%>%
    mutate_all(

      function(x){
        temp=as.POSIXct(x, optional=TRUE)
        if(!is.na(temp)) return(temp)
        temp=as.numeric(x)
        if(!is.na(temp)) return(temp)
        return(x)
      }

    )%>%
    mutate(timestamp=timestamp)
  })
}
