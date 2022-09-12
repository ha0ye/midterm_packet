
out_of_date <- function(filename, max_age = lubridate::make_difftime(day = 1))
{
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
