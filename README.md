# Harmonic Tide

Predict tide heights from harmonic constituents.

The library builds a tide table for one station and time range. It detects high and low tide times. It also estimates constituents from measured water levels.

This is a supporting project. It stays compact by design.

## Capabilities

- Load constituent amplitudes and phases for one station.
- Compute tide height for each hour in a date range.
- Write a tide table and a plot data file.
- Detect high and low tide times.
- Estimate constituents from measured water levels.

## How it works

The model uses the harmonic method. Height follows this sum:

    h(t) = z0 + sum_k a_k * cos(w_k * t - p_k)

z0 is the mean sea level. a_k is the amplitude. w_k is the speed. p_k is the phase lag. Time t uses hours since 2000-01-01T00:00:00 UTC. Speeds use standard astronomical values in degrees per hour.

The fit step estimates z0, amplitudes, and phases from measured levels. It solves a linear least-squares problem with a QR decomposition. The library includes a modified Gram-Schmidt solver. It needs no external math library.

## Requirements

- Fortran compiler (gfortran recommended)
- fpm 0.10 or newer

## Build

Run this command from the project root:

    fpm build

The project has one dependency. The test-drive library runs the test suite. fpm has no lockfile format. The dependency is pinned to a fixed release tag in fpm.toml instead.

## Run the demos

Build a tide table:

    fpm run --app tide_predict

Estimate constituents:

    fpm run --app tide_fit

Both commands use the sample data in the data directory. Output files appear in the project root.

## Command line

### tide_predict

Build a tide table and plot data for one station.

    fpm run --app tide_predict -- --station FILE --start DATE --days N --table TABLE --plot PLOT

| Option     | Default                        | Purpose                          |
|------------|--------------------------------|----------------------------------|
| --station  | data/wilson_cove.constituents  | Constituent file                 |
| --start    | 2024-06-01                     | First hour (YYYY-MM-DD)          |
| --days     | 10                             | Number of days                   |
| --end      | none                           | Last day (overrides --days)      |
| --table    | tide_table.txt                 | Tide table output                |
| --plot     | tide_plot.csv                  | Plot data output                 |

### tide_fit

Estimate constituents from measured water levels.

    fpm run --app tide_fit -- --station FILE --data FILE --output FILE

| Option     | Default                        | Purpose                          |
|------------|--------------------------------|----------------------------------|
| --station  | data/wilson_cove.constituents  | Template constituents            |
| --data     | data/wilson_cove_measured.csv  | Measured level file              |
| --output   | fitted_constituents.txt        | Fitted constituents              |

## Data files

### Constituent file

The station file holds one line per constituent. Lines that start with a hash mark are comments. A line that starts with the token z0 sets the mean sea level. Each other line holds a name, an amplitude, and a phase.

    z0      1.852
    M2      1.120   235.6
    S2      0.460   262.4

The amplitude uses metres. The phase uses degrees. The fit uses this file as a template. It reads the names and the speeds. It ignores the amplitudes and the phases.

### Measured level file

The measured level file uses CSV. The first row is the header. Each data row holds an ISO datetime and a height in metres.

    time,height_m
    2024-01-01T00:00,1.025

## Sample output

Run the prediction with three days of data. The tide table lists every hour:

    2024-06-01T00:00   1.249 m
    2024-06-01T01:00   0.676 m

The table lists the highs and lows at the end:

     2024-06-01T02:20   0.378 m  LOW
     2024-06-01T08:16   2.839 m  HIGH
     2024-06-01T13:56   0.564 m  LOW

Run the fit with the sample data. The report shows the fit quality:

    Samples: 2160
    Unknowns: 21
    Mean sea level (fitted): 1.852 m
    RMS error: .0285 m

The report also compares the template values with the fitted values. The synthetic sample adds noise, so small differences are expected.

## Plot the data

The plot file holds four columns. The columns are time, serial hours, height, and event. Each hourly sample has the event "sample". Each extremum has the event "high" or "low".

Use gnuplot to draw the curve:

    gnuplot -e "set datafile separator ','; plot 'out/tide_plot.csv' using 2:3 with lines"

## Test

Run the test suite:

    fpm test

The suite covers dates, constituents, prediction, extrema, and fitting. All tests are deterministic.

## Reproduce the sample data

The measured level file is synthetic. The generator computes heights from the station constituents. It adds a fixed seed noise. The output is reproducible.

Run the generator:

    fpm run --app generate_measured -- data/wilson_cove.constituents data/wilson_cove_measured.csv 2024-01-01 90 20240101

The generator takes the station file, the output file, a start date, the day count, and a seed.

## Project layout

| Path                       | Purpose                                   |
|----------------------------|-------------------------------------------|
| src/ht_time.f90            | Calendar and serial hour conversions      |
| src/ht_constituents.f90    | Constituent sets and file loading         |
| src/ht_prediction.f90      | Tide height series                        |
| src/ht_extrema.f90         | High and low tide detection               |
| src/ht_fit.f90             | Least-squares constituent estimation      |
| src/ht_output.f90          | Tide table and plot writers               |
| src/ht_cli.f90             | Command line helpers                      |
| app/                       | Command line programs                     |
| test/                      | Test suite                                |
| data/                      | Sample station and measured levels        |

## Limitations

The prediction uses mean solar time. It does not model the moon node factor or the astronomical equilibrium arguments. These terms matter only for long-term accuracy.

The detection samples hourly heights. It finds times within about a minute. It cannot find events that fall between samples when the tide is nearly flat.

The fit separates constituents only when the record is long enough. Two constituents with close speeds need a long record. A short record mixes their signals. The sample record covers 90 days.

The library is a compact demonstration. It is not a substitute for operational tide software.

## License

MIT. See the LICENSE file.
