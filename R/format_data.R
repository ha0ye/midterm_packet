format_jobs <- function(df)
{
    df %>%
        dplyr::mutate(date = lubridate::parse_date_time(.data$date_start, "my"),
                      date_end = tidyr::replace_na(date_end, "present"),
                      Dates = glue::glue("{date_start} - {date_end}")) %>%
        dplyr::arrange(dplyr::desc(.data$date)) %>%
        dplyr::select(Institution, Position, Dates)
}

format_grants_table <- function(df)
{
    df %>%
        dplyr::mutate(Role = glue::glue("{role} ({percentage}%)"),
                      `Reporting Agency` = funder,
                      `Grant Title` = title,
                      Dates = glue::glue('{strftime(date_start, format = "%m/%y")} - {strftime(date_end, format = "%m/%y")} '),
                      `Awarded/Anticipated` = total_award,
                      `Candidate Allocation` = allocation) %>%
        dplyr::select(Role, `Reporting Agency`,
                      `Grant Title`, Dates,
                      `Awarded/Anticipated`, `Candidate Allocation`)
}

format_talk <- function(df)
{
    df %>%
        dplyr::mutate(author = replace_na(.data$presenters, "H. Ye"),
                      location = ifelse(.data$location == "Virtual", "(virtual)", .data$location),
                      to_print = glue::glue('{author}. {title}, {event_info}, {date}, {location}.\n', .trim = FALSE)) %>%
        dplyr::select(.data$date, .data$to_print) %>%
        underline_first_author() %>%
        format_author()
}

format_reviews <- function(df)
{
    df %>%
        mutate(year = year(parse_date_time(date, "my"))) %>%
        group_by(journal) %>%
        summarize(n = n(),
                  date = max(year),
                  yearspan = ifelse(min(year) == max(year),
                                    min(year),
                                    paste0(min(year), "-", max(year)))) %>%
        mutate(num_reviews = ifelse(n == 1,
                                    "1 review",
                                    paste0(n, " reviews")),
               to_print = glue::glue("*{journal}*, reviewer, {yearspan}, {num_reviews} conducted."))
}

format_awards <- function(df)
{
    df %>%
        mutate(to_print = glue::glue("{award}, {organization}, {date}")) %>%
        select(date, to_print)
}

append_zenodo_stats <- function(df,
                                zenodo_stats_file = "data/zenodo_stats.RDS")
{
    if (!file.exists(zenodo_stats_file))
    {
        return(df)
    }

    zenodo_stats <- readRDS("data/zenodo_stats.RDS")
    df %>%
        left_join(zenodo_stats, by = "doi") %>%
        mutate(is_zenodo = is.finite(views) & is.finite(downloads),
               zenodo_text = ifelse(is_zenodo,
                                    paste0("(views: ", views,
                                           "; downloads: ", downloads,
                                           ")"),
                                    ""),
               to_print = glue::glue("{to_print} {zenodo_text}")
        )
}
