format_jobs <- function(df)
{
    df %>%
        dplyr::mutate(date = lubridate::parse_date_time(date_start, "my"),
                      date_end = tidyr::replace_na(date_end, "present"),
                      Dates = glue::glue("{date_start} - {date_end}")) %>%
        dplyr::arrange(dplyr::desc(date)) %>%
        dplyr::select(Institution, Position, Dates)
}

format_grants_table <- function(df)
{
    df %>%
        dplyr::mutate(Role = glue::glue("{role} ({percentage}%)"),
                      `Reporting Agency` = funder,
                      `Grant Title` = title,
                      Dates = glue::glue('{strftime(date_start, format = "%b/%y")} - {strftime(date_end, format = "%b/%y")} '),
                      `Awarded/Anticipated` = glue::glue('{total_award}\n{total_award}'),
                      `Candidate Allocation` = allocation) %>%
        dplyr::select(Role, `Reporting Agency`,
                      `Grant Title`, Dates,
                      `Awarded/Anticipated`, `Candidate Allocation`)
}

format_talk <- function(df)
{
    df %>%
        dplyr::mutate(author = replace_na(presenters, "Ye, H."),
                      location = ifelse(location == "Virtual", "(virtual)", location),
                      to_print = glue::glue('{author} _{title}_, {event_info}, {date}, {location}.\n', .trim = FALSE)) %>%
        dplyr::select(date, to_print) %>%
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

join_zenodo_stats <- function(df,
                                zenodo_stats_file = "data/zenodo_stats.RDS")
{
    if (!file.exists(zenodo_stats_file))
    {
        warning("Did not find zenodo stats file - ", zenodo_stats_file)
        return(df)
    }

    zenodo_stats <- readRDS("data/zenodo_stats.RDS")
    df %>%
        left_join(zenodo_stats, by = "doi", suffix = c("", ".z"))
}

format_zenodo_stats <- function(df)
{
    df %>%
        mutate(is_zenodo = is.finite(views) & is.finite(downloads),
               zenodo_text = ifelse(is_zenodo,
                                    paste0("(views: ", views,
                                           "; downloads: ", downloads,
                                           ")"),
                                    "")
        )
}

format_videos <- function(df)
{
    df %>%
        rowwise() %>%
        mutate(bib = list(BibEntry(bibtype = "Video",
                                   title = .data$title,
                                   author = paste(.data$speaker, "and", .data$hosts),
                                   date = .data$date,
                                   url = .data$url,
                                   key = .data$title)),
               to_print = purrr::map_chr(.data$bib, format_ref)) %>%
        select(date, to_print)
}

format_outreach <- function(df)
{
    df %>%
        mutate(instructor = replace_na(instructor, "Hao Ye"),
               other_instructor = str_replace_all(other_instructor, ", ", " and "),
               title = ifelse(is.na(session), title, glue::glue("{title}: {session}")),
               author = ifelse(is.na(other_instructor),
                               instructor,
                               paste0(instructor, " and ", other_instructor)),
               multiauthor = !is.na(other_instructor)) %>%
        rowwise() %>%
        mutate(bib = list(BibEntry(bibtype = "Misc",
                                   title = .data$title,
                                   author = .data$author,
                                   location = paste0(.data$event_info, ", ", .data$date),
                                   date = NA,
                                   key = .data$title)),
               to_print = purrr::map_chr(.data$bib, format_ref)) %>%
        label_coinstructor() %>%
        format_author()
}

combine_talks_and_outreach <- function(talks, outreach, locale = "international",
                         refereed_label = "Refereed")
{
    talks <- filter(talks, format != "submitted", locale == {{locale}})
    outreach <- filter(outreach, locale == {{locale}})

    if (NROW(talks) + NROW(outreach) == 0)
    {
        cat("None\n")
    }

    if (any(talks$invited == "y", na.rm = TRUE) ||
        any(outreach$invited == "y", na.rm = TRUE))
    {
        cat("### Invited\n\n")
        bind_rows(
            talks %>%
                filter(invited == "y") %>%
                format_talk(),
            outreach %>%
                filter(invited == "y") %>%
                format_outreach()
        ) %>%
            print_data(prefix = "", sep = "\n\n<br />\n\n")
    }

    if (any(talks$invited != "y", na.rm = TRUE) ||
        any(outreach$invited != "y", na.rm = TRUE))
    {
        cat("### ", refereed_label, "\n", sep = "")
        bind_rows(
            talks %>%
                filter(invited == "n") %>%
                format_talk(),
            outreach %>%
                filter(invited == "n") %>%
                format_outreach()
        ) %>%
            print_data(prefix = "", sep = "\n\n<br />\n\n")
    }
}

join_bib_data <- function(df, my_bib)
{
    df %>%
        dplyr::rowwise() %>%
        dplyr::mutate(bib = case_when(
            is.na(bib_id) ~ list(BibEntry(bibtype = "Misc",
                                          title = .data$title,
                                          author = str_replace_all(.data$authors, ", ", " and "),
                                          doi = .data$doi,
                                          date = .data$date,
                                          key = .data$title,
            )),
            TRUE ~ list(my_bib[bib_id])),
            to_print = format_ref(bib))
}

label_coinstructor <- function(df,
                               replacement = c("^([A-Za-z\\-]+, [A-Za-z\\-]+\\.)" = "\\1 [co-instructor]",
                                               "(, |and )([A-Za-z\\-]+\\. [A-Za-z\\-]+)" = "\\1\\2 [co-instructor]"))
{
    dplyr::mutate(df, to_print = case_when(
        multiauthor ~ stringr::str_replace_all(.data$to_print,
                                               {{replacement}}),
        TRUE ~ to_print)
    )
 }
