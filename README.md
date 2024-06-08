## YR Headwaters

June 7, 2024

The YR Headwaters app is a simple demo Shiny app that enables users to visualize various statistics summarized at the watershed level.

The app can be run from a local machine using the following steps:

  1. Install R (download from r-project.org and follow instructions)
  2. Install the following additional packages:

    install.packages(c("sf","DT","leaflet","leafem","dplyr","shinydashboard"))

  3. Start the Shiny app:

    shiny::runGitHub("beaconsproject/yr_headwaters")
