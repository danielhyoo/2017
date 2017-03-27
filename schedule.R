library("lubridate")
library("yaml")
library("purrr")
QTR_START <- ymd(20170327)
QTR_END <- ymd(20170602)

class_session <- function(x, class. = TRUE) {
  if (class.) {
    res <- list(
      start = ymd_hm(paste(format(x, "%Y-%m-%d"),
                           "17:00"), tz = "US/Pacific"),
      end = ymd_hm(paste(format(x, "%Y-%m-%d"),
                         "18:20"), tz = "US/Pacific"),
      type = "class"
    )

  } else {
    res <- list(
      start = ymd_hm(paste(format(x, "%Y-%m-%d"),
                           "13:30"), tz = "US/Pacific"),
      end = ymd_hm(paste(format(x, "%Y-%m-%d"),
                         "15:20"), tz = "US/Pacific"),
      type = "lab"
    )
  }
  res$date <- format(x, "%Y-%m-%d")
  res$start <- format(res$start, "%Y-%m-%d %H:%M")
  res$end <- format(res$end, "%Y-%m-%d %H:%M")
  res$reading_before = list()
  res$reading_after = list()
  res$in_class = list()
  res
}

map(seq(QTR_START, QTR_END, by = 1),
    function(x) {
      if (wday(x) %in% c(2, 4)) { # m, w
        class_session(x, TRUE)
      } else if (wday(x) %in% 6) { # f
        class_session(x, TRUE)
      }
    }) %>%
  compact() %>%
  yaml::as.yaml() %>%
  cat(file = "schedule.yml")
