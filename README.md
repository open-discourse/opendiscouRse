# opendiscouRse <img src="man/figures/logo.png" align="right" alt="" width="200" />

<img src="https://img.shields.io/badge/Development-Late%20Development%20Stage-green" />

`R` package containing functions to obtain descriptive statistics and conduct various forms of analyses using the **Open Discourse** database.

The package is currently under development, while a majority of functions are already working robustly while being tested.

Currently, the package is based on a functional approach, while the goal is to provide an additional OOP infrastructure by using `R6` object classes (currently working: `OpenDiscourse()`, currently not working: `OData()`).

One of the main advantages of using an OOP framework lies in the exclusive allocation of functions (methods). Users would be able to see all available methods, while misuse and confusions are inhibited, which should ease the usage of the package and thus make users more easily comfortable in working with the **Open Discourse** database.

---

# Installation

You can install the package with `devtools::install_github("open-discourse/opendiscouRse")`.

# Functions

### `get` functions

- `get_age()`: Get age of politician.
- `get_age_hist()`: Get age of politician based on a historic date.
- `get_ets()`: Get electoral term affiliations of politicians.
- `get_profession_groups()`: Get professional group affiliation of politicians' jobs.
- `get_faction:color()`: Get color code of factions.
- `get_state()`: Get state affiliation of elected politician (either list or direct mandate).
- `get_table_1()`: Get main table (*Table 1*) with descriptive summaries of the database.
- `get_implausible_data()`: Get data that is implausible based on specific data table based information.

### `count` functions

- `count_data()`: Count observations by grouping variables.
- `rel_freq_data()`: Relative frequencies in data when grouping by more than one variable.

### `plot` functions

- `plot_dist()`: Plotting distributions by group variables.
- `plot_cov()`: Plotting coverage (`NAs`) of data tables.
- `plot_count()`: Plotting summarized data frames generated by `count_data()`.
- `plot_rel_freq()`: Plotting summarized data frames generated by `rel_freq_data()`.
- `theme_od()`: `ggplot2` theme for **Open Discourse**.


