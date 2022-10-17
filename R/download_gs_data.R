
out_of_date <- function(filename, max_age = lubridate::make_difftime(day = 1))
{
    if (!file.exists(filename))
    {
        return(TRUE)
    }
    last_updated <- fs::file_info(filename)$modification_time
    age <- Sys.time() - last_updated
    age > max_age
}

if (out_of_date("data/degrees.RDS"))
{
    dat_degrees <- get_gs_data(data_gs_url, sheet = "degrees")
    saveRDS(dat_degrees, "data/degrees.RDS")
}

if (out_of_date("data/jobs.RDS"))
{
    dat_jobs <- get_gs_data(data_gs_url, sheet = "jobs")
    saveRDS(dat_jobs, "data/jobs.RDS")
}

if (out_of_date("data/teaching.RDS"))
{
    dat_teaching <- get_gs_data(data_gs_url, sheet = "teaching",
                                report_start_date, report_end_date)
    saveRDS(dat_teaching, "data/teaching.RDS")
}

if (out_of_date("data/research.RDS"))
{
    dat_research <- get_gs_data(data_gs_url, sheet = "research",
                                report_start_date, report_end_date)
    saveRDS(dat_research, "data/research.RDS")
}


if (out_of_date("data/grants.RDS"))
{
    dat_grants <- get_gs_data(data_gs_url, sheet = "grants",
                              report_start_date, report_end_date,
                              date_cols = c("date_start", "date_end"))
    saveRDS(dat_grants, "data/grants.RDS")
}

if (out_of_date("data/service.RDS"))
{
    dat_service <- get_service_data(data_gs_url,
                                   report_start_date, report_end_date)
    saveRDS(dat_service, "data/service.RDS")
}

if (out_of_date("data/reviews.RDS"))
{
    dat_reviews <- read_sheet(data_gs_url, sheet = "reviews", col_types = "c") %>%
        filter(parse_date_time(date, "my") >= report_start_date &
                   parse_date_time(date, "my") <= report_end_date)
    saveRDS(dat_reviews, "data/reviews.RDS")
}

if (out_of_date("data/awards.RDS"))
{
    dat_awards <- get_gs_data(data_gs_url, sheet = "awards",
                              report_start_date, report_end_date)
    saveRDS(dat_awards, "data/awards.RDS")
}

if (out_of_date("data/talks.RDS"))
{
    dat_talks <- get_gs_data(data_gs_url, sheet = "talks",
                              report_start_date, report_end_date)
    saveRDS(dat_talks, "data/talks.RDS")
}

