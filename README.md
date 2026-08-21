# pharmacoepi-2026-non-adherence

## Repository for the manuscript: Primary non-adherence to electronic prescriptions in Lithua-nia, 2018–2024: nationwide prevalence, factors, and regional variation

## Structure

- `data/raw/` — raw prescription/dispensing files
- `data/dictionaries/` — ATC and municipality dictionaries in CSVs
- `data/processed/` — merged and aggregated data built by the pipeline
- `scripts/` — pipeline code: 
  - `downloading_data/`
  - `create_dictionaries/`,
  - `processing/`
  - `manuscript/`
  - `helpers/`
- `output/` — generated tables and figures

## Run

```r
source("scripts/RUN_ALL.R")
```

## License

See `LICENSE`.
