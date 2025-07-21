# Load pacman or install if not available
if (!require(pacman)) install.packages("pacman")
library(pacman)

# Load required packages
p_load(wbstats, ggplot2, dplyr)

# Function to fetch World Bank data
fetch_wb_data <- function() {
  tryCatch({
    message("Attempting to fetch data from World Bank API...")
    countries <- c("AU", "DE", "JP", "SG", "KR", "SE", "US") # ISO2 codes
    data <- wb_data(
      indicator = "SL.TLF.CACT.FE.ZS", # Female labor force participation rate (%)
      country = countries,
      start_date = 2000,
      end_date = 2020,
      return_wide = TRUE
    ) %>%
      mutate(
        ID = factor(
          iso2c,
          levels = c("AU", "DE", "JP", "SG", "KR", "SE", "US"),
          labels = c(1, 2, 3, 4, 5, 6, 7)
        ),
        flfp = SL.TLF.CACT.FE.ZS,
        year = date
      ) %>%
      select(ID, flfp, year) %>%
      filter(!is.na(flfp) & !is.na(year)) # Remove rows with missing flfp or year
    if (nrow(data) == 0) {
      message("No valid data after filtering. Check API response for missing or invalid values.")
      return(NULL)
    }
    message("Data successfully fetched from World Bank API. ", nrow(data), " rows available.")
    return(data)
  }, error = function(e) {
    message("Error fetching data from World Bank API: ", e$message)
    message("Please check your internet connection, DNS settings (e.g., run 'ipconfig /flushdns' on Windows or 'sudo dscacheutil -flushcache' on macOS), or try again later.")
    return(NULL)
  })
}

# Fetch data
data <- fetch_wb_data()

# Proceed if data is available
if (!is.null(data)) {
  # Define country labels, colors, and shapes to match Stata
  country_labels <- c(
    "1" = "Australia",
    "2" = "Germany",
    "3" = "Japan",
    "4" = "Singapore",
    "5" = "South Korea",
    "6" = "Sweden",
    "7" = "U.S."
  )
  
  colors <- c(
    "1" = "#000080", # navy
    "2" = "#800000", # maroon
    "3" = "#228B22", # forest_green
    "4" = "#FFA500", # orange
    "5" = "#90EE90", # eltgreen approximated as lightgreen
    "6" = "#FF0000", # red
    "7" = "#E6E6FA"  # lavender
  )
  
  shapes <- c(
    "1" = 1,   # circle_hollow
    "2" = 17,  # triangle filled
    "3" = 0,   # square_hollow
    "4" = 4,   # lgx approximated as 'x'
    "5" = 5,   # diamond_hollow
    "6" = 16,  # circle filled
    "7" = 17   # triangle filled
  )
  
  # Create the plot to replicate Stata's twoway graph
  ggplot(data, aes(x = year, y = flfp, color = ID, group = ID)) +
    geom_line(linewidth = 0.3) + # vthin line width
    geom_point(aes(shape = ID), size = 2, stroke = 0.3) + # marker size and stroke
    scale_color_manual(values = colors, labels = country_labels, name = NULL) +
    scale_shape_manual(values = shapes, labels = country_labels, name = NULL) +
    labs(
      title = "labor force participation rate (%), female",
      x = "year",
      y = ""
    ) +
    scale_x_continuous(
      breaks = seq(2000, 2020, 1),
      labels = seq(2000, 2020, 1),
      limits = c(2000, 2020),
      expand = c(0, 0)
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 8),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 0), # ytitle size zero
      axis.text.y = element_text(size = 8),
      panel.grid.major.x = element_blank(), # no vertical grid
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = alpha("#ADD8E6", 0.5), linewidth = 0.1, linetype = "solid"), # ltblue%50, vthin
      panel.grid.minor.y = element_blank(),
      legend.position.inside = c(0.5, -0.2), # Updated for ggplot2 3.5.0+
      legend.direction = "horizontal",
      legend.box.background = element_rect(color = "black", linewidth = 0.1, linetype = "solid"),
      legend.box.margin = margin(t = 10, b = 10, l = 10, r = 10),
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.4, "cm"),
      legend.spacing.x = unit(0.2, "cm"),
      legend.spacing.y = unit(0.1, "cm"),
      plot.margin = margin(t = 20, b = 60, l = 20, r = 20) # adjust for legend
    ) +
    guides(
      color = guide_legend(
        nrow = 4,
        byrow = TRUE,
        override.aes = list(shape = shapes, linewidth = 0.3)
      ),
      shape = guide_legend(
        nrow = 4,
        byrow = TRUE
      )
    )

ggsave(
  filename = "flfp_plot.png",    # output file
  plot     = last_plot(),        # grabs the most recent ggplot
  width    = 5.5,                # inches
  height   = 4,                  # inches
  units    = "in",
  dpi      = 300                  # hi‑res
)

