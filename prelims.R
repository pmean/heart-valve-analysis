# prelims.R
# Steve Simon
# 5-30-2017

library(dplyr)
library(lubridate)
library(magrittr)
library(survival)
library(tidyr)

clean_up_names <- function(x) {
  x[1] <- "id"                                      # consistent name for primary key
  x                                             %>%
    tolower                                     %>% # convert to lowercase
    gsub("\\.", "_", .)                         %>% # convert dots to underscores
    gsub("_+",  "_", .)                         %>% # convert multiple underscores to single
    gsub("_$",  "",  .)                         %>% # remove trailing underscore
    return
}

empty <- function(x) {
  is.na(x) | x=="" | grepl("^[[:space:]]*$", x)
}
empty("  ")
empty(NA)
empty("")
empty("  x")

filled <- function(x) {
  !is.na(x) & x!="" & !grepl("^[[:space:]]*$", x)
}


identify_empty_columns <- function(df) {
  df %>% empty %>% as.matrix %>% apply(2, all)
}

identify_empty_rows <- function(df) {
  df %>% empty %>% as.matrix %>% apply(1, all)
}

remove_empty_space <- function(df) {
  dfname <- deparse(substitute(df))
  df %>% dim %>% paste(c("rows", "columns")) %>% paste(collapse=" and ")
  i <- identify_empty_rows(df)
  j <- identify_empty_columns(df)
  cat("\n*** ")
  cat(dfname)
  cat("\nEmpty rows: ")
  cat(paste(which(i), collapse=", "))
  if (sum(i)==0) {cat("none")}
  cat("\nEmpty columns: ")
  cat(paste(names(df)[j], collapse=", "))
  if (sum(j)==0) {cat("none")}
  df1 <- df[!i, !j]
  cat("\n")
  cat(dfname)
  cat(" has ")
  df %>% dim %>% paste(c("rows", "columns")) %>% paste(collapse=" and ") %>% cat
  cat(" reduced to ")
  df1 %>% dim %>% paste(c("rows", "columns")) %>% paste(collapse=" and ") %>% cat
  cat("\n")
  return(df1)
}

isolate_month <- function(v) {
  v                                             %>%
    gsub("/.*", "", .)                          %>%
    gsub("-.*", "", .)                          %>%
    return
}
isolate_day <- function(v) {
  v                                             %>%
    sub("^.*?/", "", .)                         %>%
    sub("/.*$?", "", .)                         %>%
    sub("^.*?-", "", .)                         %>%
    sub("-.*$?", "", .)                         %>%
    return
}
isolate_year <- function(v) {
  v                                             %>%
    sub("^.*/", "", .)                          %>%
    sub("^.*-", "", .)                          %>%
    return
}
fix_dates <- function(v) {
  old_v <- v
  v <- ifelse(v=="", 777, v)
  v                                             %>%
    sub("//", "/", .)                           %>%
    sub("999.00", "999", .)                     %>%
    sub("00/00/", "07/01/", .)                  %>%
    sub("^0/00/", "07/01/", .)                  %>%
    sub("/00/", "/15/", .)                      %>%
    sub("6/695",  "06/06/1995", .)              %>%
    sub("812/06", "08/12/2006", .)              %>%
    sub("6/6/1902", "888", .)                   %>%
    sub("9/25/1902", "999", .)                  -> v
  update_flag <- v!=old_v
  old_v                                         %>%
    paste("changed to")                         %>%
    paste(v)                                    %>%
    tibble                                      %>%
    filter(update_flag)                         %>%
    distinct                                    %>%
    unlist                                      %>%
    cat(sep="\n")
  return(v)
}
examine_dates <- function(v) {
  two_slashes <- grepl("/", sub("/", "", v))
  two_dashes  <- grepl("-", sub("-", "", v))
  special_cases <- ! (two_slashes | two_dashes) | is.na(v)
  if (sum(special_cases)>0) {
    cat("\n")
    cat("Special cases")
    v[special_cases]                            %>%
      table(useNA="ifany")                      %>%
      addmargins                                %>%
      print
  }
  v                                             %>%
    isolate_month                               %>%
    as.numeric                                  %>%
    is_in(1:12)                                 %>%
    not                                         %>%
    and(!special_cases)                         -> bad_month
  if (sum(bad_month)>0) {
    cat("\n")
    cat("Bad month ")
    print(table(v[bad_month]))
  }
  v                                             %>%
    isolate_day                                 %>%
    as.numeric                                  %>%
    is_in(1:31)                                 %>%
    not                                         %>%
    and(!special_cases)                         %>%
    and(!bad_month)                             -> bad_day
  if (sum(bad_day)>0) {
    cat("\n")
    cat("Bad month ")
    print(table(v[bad_day]))
  }
  v                                             %>%
    isolate_year                                %>%
    as.numeric                                  %>%
    is_in(1950:2017)                            %>%
    not                                         %>%
    and(!special_cases)                         %>%
    and(!bad_month)                             %>%
    and(!bad_day)                               -> bad_year
  if (sum(bad_year)>0) {
    cat("\n")
    cat("Bad year ")
    print(table(v[bad_year]))
  }
}


loop_through_dates <- function(df) {
  dfname <- deparse(substitute(df))
  names(df) %>% grep("date", ., value=TRUE) -> date_names
  for (d in date_names) {
    cat("\n\n**** ")
    cat(paste(dfname, d))
    examine_dates(df[, d])
    df[, d] <- fix_dates(df[, d])
    cat("\n\n**** Revised ")
    cat(paste(dfname, d))
    examine_dates(df[, d])
  }
}
