
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

if (out_of_date("data/research.RDS"))
{
    dat_research <- get_gs_data(data_gs_url, sheet = "research",
                                report_start_date, report_end_date)
    saveRDS(dat_research, "data/research.RDS")
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
