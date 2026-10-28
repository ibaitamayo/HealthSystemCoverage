# Contributing

Use fictional fixtures only. Do not submit patient-level event logs, cohort
membership, clinical text, credentials, private source maps or real reports in
issues, pull requests, screenshots or continuous-integration artifacts.

Keep source-system recognition configurable. Do not introduce default Spanish,
English or other label matching, infer demographic attributes, impute clinical
timestamps, or identify a service solely from a medicine without explicit
evidence. Distinguish case, person, event and observation-window denominators.

Before proposing a change, from the package root:

```bash
R CMD build .
R CMD check --no-manual HealthSystemCoverage_0.2.1.tar.gz
```

Optional process dependencies and Pandoc are required for complete report tests.
No live OMOP connection is required for the test suite. Do not alter a site's
official renv lockfile while testing this repository.
