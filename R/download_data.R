cohorts <- function(){
  c("ACC", "BLCA", "BRCA", "CESC", "CHOL", "COADREAD", "COAD",
     "DLBC", "ESCA", "GBMLGG", "GBM", "HNSC", "KICH", "KIPAN",
     "KIRC", "KIRP", "LAML", "LGG", "LIHC", "LUAD", "LUSC", "MESO",
     "OV", "PAAD", "PCPG", "PRAD", "READ", "SARC", "SKCM", "STAD",
     "STES", "TGCT", "THCA", "THYM", "UCEC", "UCS", "UVM")
}

gistic_paths <- function(){
  path_gistic_urls <- system.file("gistic_urls.csv", package = "TCGAgisticDB")
  read.csv(path_gistic_urls, header = TRUE, check.names = FALSE)
}

#' Download gistic ZIPfiles from filrebrowse
#'
#' @param cohort
#' @param destdir
#'
gistic_download <- function(cohort, destdir){

  df_gistic_urls <- gistic_paths()


  # Assertions
  if (!cohort %in% df_gistic_urls[["Cohort"]]) stop("Could not find cohort ", cohort, ". \n\nCohorts must be one of: \n",paste0(cohorts, collapse = ", "))

  # Create URL
  url = df_gistic_urls[["URL"]][match(cohort, df_gistic_urls[["Cohort"]])]

  if(!valid_url(url)) {
     warning("Skipping Cohort: ", cohort, " since cannot find gistic file")
      browser()
     return(NULL)
  }
  # Build destination path
  destfile <- paste0(destdir, "/", basename(url))

  # Download data
  message("Downloading ", cohort, " GISTIC2 files to \n", destfile, "\n\n")
  download.file(url = url, destfile = destfile)
}

gistic_download_all_cohorts <- function(destdir){
  df_gistic_urls <- gistic_paths()
  for (i in seq_len(nrow(df_gistic_urls))){
    gistic_download(cohort = df_gistic_urls[["Cohort"]][i], destdir = destdir)
  }
}

valid_url <- function(url_in,t=2){
  con <- url(url_in)
  check <- suppressWarnings(try(open.connection(con,open="rt",timeout=t),silent=T)[1])
  suppressWarnings(try(close.connection(con),silent=T))
  ifelse(is.null(check),TRUE,FALSE)
}
