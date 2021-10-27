#' dedup Function
#' This function allows you get possible duplicates using jarowinkler algorithm on RecordLinkage Package
#' @param CBMS CORE HPQ VN05201701 main.csv
#' @keywords cbms
#' @export
#' @examples
#' dedup()
#'
#' dedup()

dedup <- function(hpq,main){
  hpq <- data.table::fread(hpq)
  main <- data.table::fread(main)
  main <- main %>%
    select(main.id, hnum, street)
  pats <- c("ÃƒÂ|ÃË|Â|ÃƒÂ±|Ã")

  a <- hpq %>%
    mutate(across(4:6, str_replace, pats, "ñ"))
  a <- a %>%
    mutate(across(4:6, str_to_lower))
  a <- a %>%
    mutate(across(4:6,str_remove_all,"[^[:alnum:]]"))


  a <- a %>%
    select(main.id,mfname,msname,mmname) %>%
    mutate(flname = paste(mfname," ",mmname," ",msname),
           fname = paste(mfname," ",msname))

  dedup1 <- RecordLinkage::compare.dedup (a, blockfld =list(1, 5:6),
                           strcmp = c("mfname","mmname","msname"), strcmpfun = jarowinkler)
  dedup.match <- dedup1$pairs

  dedup.match1 <- dedup.match %>%
    mutate(idiff = id1-id2)
  dedup.match1$mean <- rowMeans(dedup.match1[,4:6],na.rm=F)
  dedup.match100 <- dedup.match1 %>%
    filter(idiff < -10) %>%
    arrange(desc(mean)) %>%
    select(id1,id2)
  df <- data.frame(main.id = c(t(dedup.match100)))
  df <- a[df$main.id,]
  final <- merge(df,main, by="main.id", sort = F)
  return(final)
}

