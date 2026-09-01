# Patient-Surge-Model
This application is a decision-support tool for exploring hospital bed demand during surge events. It uses discrete-event simulation (DES) to represent patient arrivals, care trajectories, bed occupancy, fallback placement, and queues across multiple hospital units.

## Directory structure

```text
hospital-surge-capacity/
├── app.R
├── manifest.json
├── README.md
├── R/
│   ├── 00_packages.R
│   ├── 01_config.R
│   ├── data/
│   │   └── profiles_deloitte.R
│   ├── helpers/
│   │   ├── simulation_metrics.R
│   │   └── report_functions.R
│   ├── simulation/
│   │   └── hospital_trajectory.R
│   ├── modules/
│   │   └── mod_profiles.R
│   ├── ui/
│   │   ├── ui_sidebar.R
│   │   └── ui_main.R
│   ├── app_ui.R
│   └── app_server.R
├── data/
├── www/
│   ├── styles.css
│   ├── logo.png
└── └── images/

```
