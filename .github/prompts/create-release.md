---
name: create-release
description: Prepare and submit a patch, minor, or major CRAN release
---

# Create a CRAN Release

Ask the user exactly one question before doing anything else:

> Which release type should I create: patch, minor, or major?

Pause for the answer. Accept only `patch`, `minor`, or `major`
(case-insensitive); do not infer the release type from the repository state.
After receiving a valid answer, take ownership of the remaining release work.

## Prepare the release

Read `AGENTS.md` and `.github/workflows/submit-cran.yaml` before making changes.
Use the release process and validation commands documented there, together with
the requirements below.

1. Verify `gh` authentication, fetch and prune `origin`, and inspect the working
   tree. Preserve unrelated local changes. If necessary, use a clean worktree.
2. Query current CRAN package metadata for the latest version actually
   published on CRAN and use that version as the semantic-version base.
   Cross-check it against `DESCRIPTION`, open release pull requests, and GitHub
   releases and tags. A successful submission workflow is not CRAN acceptance,
   so never use it as the version base.
3. Compute the requested version from the CRAN-published base:

   - patch: `x.y.z` to `x.y.(z+1)`
   - minor: `x.y.z` to `x.(y+1).0`
   - major: `x.y.z` to `(x+1).0.0`

4. If that target version already has a branch, pull request, tag, or GitHub
   Release but is not on CRAN, treat it as an in-flight or rejected submission.
   Resume and repair that release rather than incrementing past it or creating
   a duplicate. Otherwise, start a `release-x.y.z` branch from the latest
   `origin/main`, never from the caller's possibly stale local branch.
5. Review commits and merged pull requests since the latest CRAN release, plus
   the existing development section in `NEWS.md`. Do not invent changes.
6. Synchronize the release version in `DESCRIPTION`, `codemeta.json`, and the
   first `NEWS.md` heading. Remove the development `.9000` suffix. Regenerate
   generated metadata through repository tooling when available rather than
   hand-editing unrelated fields.
7. Edit `NEWS.md` into concise, user-facing release notes. Preserve its existing
   section style, and omit routine dependency, lint, formatting, CI, and
   generated-file maintenance.
8. Update `cran-comments.md` with the correct release type and version. Mention
   a CRAN issue only when the repository evidence supports it, and keep check
   and reverse-dependency results accurate.

Do not refresh dependencies merely because this is a release.

## Validate and open the pull request

Run the repository's complete release gate, fixing release-related failures and
rerunning the affected checks until they pass:

```bash
air format . --check
make lint
make hooks
make check
git diff --check
```

Clean build/check artifacts, inspect the final diff, and confirm that only
intentional release files changed. Commit, push, and open a ready-for-review
pull request against `main`. The title should identify the exact release
version and contain the exact text `CRAN Release`. The body should summarize the
release notes, version surfaces, CRAN comments, and validation performed.

Monitor the pull request until every required GitHub Actions check passes and
there are no unresolved review threads. Address actionable review feedback,
reply to every review comment, push fixes, and revalidate without asking the
user to drive routine follow-up.

## Submit to CRAN

Keep the Pull Request whose title contains `CRAN Release` open. After all checks
pass and all review threads are resolved, check out the release branch and
submit it directly:

```bash
Rscript -e 'assignInNamespace("yesno", function(...) FALSE, "devtools"); devtools::submit_cran()'
```

Do not dispatch `.github/workflows/submit-cran.yaml` before CRAN acceptance
because that workflow also creates a GitHub Release and tag. Inspect the direct
submission output if it fails. Fix problems on the same release branch and
retry only after its checks are green.

Once submission succeeds, report the exact version, remind the maintainer to
confirm the CRAN email, and stop. Do not merge the Pull Request, create a tag,
or create a GitHub Release. Successful submission and email confirmation are
not CRAN acceptance.

Wait until the user explicitly states that CRAN accepted the package. Then
verify that CRAN publishes the target version, squash-merge the `CRAN Release`
Pull Request, and confirm that `main` contains the accepted version. Build the
source tarball from that merged commit and create the GitHub Release and tag.
Use the complete matching version section from `NEWS.md` verbatim as the
release body; do not summarize it and never use `--generate-notes`. Attach the
source tarball and verify the tag target, release body, and asset.
