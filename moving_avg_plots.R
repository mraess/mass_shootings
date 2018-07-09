
# Plotting stuff ----------------------------------------------------------

library(lubridate)
library(tidyquant)


mass_shootings %>% ggplot(aes(date, fatalities)) + geom_point()

mean_shooting_victims <- mass_shootings %>%
        tq_transmute(
                select     = total_victims,
                mutate_fun = apply.yearly, 
                FUN        = mean,
                na.rm      = TRUE,
                col_rename = "mean_count"
        )
mean_shooting_victims


# Rolling mean
mass_rolling_mean <- mass_shootings %>% 
        tq_mutate(
                # tq_mutate args
                select     = total_victims,
                mutate_fun = rollapply, 
                # rollapply args
                width      = 6,
                align      = "right",
                FUN        = mean,
                # mean args
                na.rm      = TRUE,
                # tq_mutate args
                col_rename = "roll_mean"
        )

mass_rolling_mean

# ggplot
mass_rolling_mean %>%
        ggplot(aes(x = date, y = total_victims)) +
        # Data
        geom_point(alpha = 0.8, color = ifelse(mass_rolling_mean$total_victims < 200,"#E68415", "#C94024")) +
        geom_line(aes(y = roll_mean), color = palette_light()[[1]], size = 1) +
        scale_x_date(date_breaks = "2 years", date_labels =  "%Y") +
        theme_tufte() +
        theme(plot.title = element_text(hjust = 0.5)) +
        ggtitle("Moving average of total victims") +
        labs(y = "Total victims", x = "Year") +
        coord_cartesian(xlim = c(lubridate::date("1982-01-01"), lubridate::date("2019-01-01")), ylim = c(0, 600))


savings %>%
        gather(metric, value, srate:srate_ma10) %>%
        ggplot(aes(date, value, color = metric)) +
        geom_line() +
        coord_cartesian(xlim = c(date("2000-01-01"), date("2015-04-01")), ylim = c(0, 11))


        theme(axis.text.x = element_text(angle = 0),
              panel.grid = element_blank(),
              panel.grid.major.y = element_line(color = "grey"),
              panel.background = element_rect(fill = "#4F2F2F"))
        
        
## Moving averages of fatalities

ma <- mass_shootings %>%
        select(date, total_victims) %>% 
        mutate(srate_ma01 = rollmean(total_victims, k = 5, fill = NA, align = "right"))


,
               srate_ma02 = rollmean(total_victims, k = 9, fill = NA),
               srate_ma03 = rollmean(total_victims, k = 11, fill = NA),
               srate_ma05 = rollmean(total_victims, k = 15, fill = NA),
               srate_ma10 = rollmean(total_victims, k = 19, fill = NA))

ma


ma %>%
        gather(metric, value, srate:srate_ma10) %>%
        ggplot(aes(date, value, color = metric)) +
        geom_line()

