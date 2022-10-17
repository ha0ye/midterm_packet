print_talks <- function(talks, locale = "international")
{
    talks <- filter(talks, locale == {{locale}})
    if (NROW(talks) == 0)
    {
        cat("None\n")
    }

    if (any(talks$invited == "y"))
    {
        cat("### Invited\n\n")
        talks %>%
            filter(invited == "y") %>%
            format_talk() %>%
            print_data(prefix = "", sep = "\n\n<br />\n\n")
    }

    if (any(talks$invited == "n"))
    {
        cat("### Refereed\n")
        talks %>%
            filter(invited == "n") %>%
            format_talk() %>%
            print_data(prefix = "", sep = "\n\n<br />\n\n")
    }
}

print_service <- function(service, sublocale = "university")
{
    service %>%
        filter(sublocale == {{sublocale}}) %>%
        mutate(date_end = replace_na(date_end, "present")) %>%
        format_service() %>%
        print_data(prefix = "* ", sep = "\n\n")
}

print_memberships <- function(memberships, locale = "international")
{
    memberships %>%
        filter(locale == {{locale}}) %>%
        mutate(date_end = replace_na(date_end, "present")) %>%
        format_service() %>%
        print_data(prefix = "* ", sep = "\n\n",
                   empty_message = "None\n")
}

print_awards <- function(awards, locale = "international")
{
    awards %>%
        filter(locale == {{locale}}) %>%
        format_awards() %>%
        print_data(prefix = "", sep = "\n\n",
                   empty_message = "None")
}