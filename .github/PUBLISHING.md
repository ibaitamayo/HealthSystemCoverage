# Publication checklist

Do not publish until the owner has confirmed:

1. GitHub account/organisation, repository name and public/private visibility.
2. Maintainer name and email in DESCRIPTION.
3. Copyright holder and the existing MIT licence in LICENSE and LICENSE.md.

Replace the OWNER installation placeholder and add the confirmed repository URL
and issue tracker in DESCRIPTION. No real clinical examples, source maps,
credentials, local paths, private diagnostics or reports belong in this repository.

Build/check the package and run its fictional demonstration. Confirm the Git
file list and source archive do not include local results, RDS files, config.R,
renv environments, credentials or patient logs. Commit only reviewed source.

Create/push the repository only through the user's authenticated connection.
Do not request tokens or passwords in chat. Do not guess an organisation or
publish privately supplied results. The repository root must be the R package
root, so install_github needs no subdirectory argument.

This initial publication omits GitHub Actions with the owner's approval.
Local checks do not constitute a CI run. If Actions is added later, inspect its
actual results before claiming CI passed.
A release tag and source asset may be created after validation.
GitHub-only publication is distinct from CRAN submission; no CRAN submission
is part of this workflow.
