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
2. Start a `release-x.y.z` branch from the latest `origin/main`, never from the
   caller's possibly stale local branch.
3. Determine the latest published stable version from the repository's releases
   and cross-check it against `DESCRIPTION`. Compute the new version with
   semantic versioning:
   - patch: `x.y.z` to `x.y.(z+1)`
   - minor: `x.y.z` to `x.(y+1).0`
   - major: `x.y.z` to `(x+1).0.0`
4. Review commits and merged pull requests since the latest release, plus the
   existing development section in `NEWS.md`. Do not invent changes.
5. Synchronize the release version in `DESCRIPTION`, `codemeta.json`, and the
   first `NEWS.md` heading. Remove the development `.9000` suffix. Regenerate
   generated metadata through repository tooling when available rather than
   hand-editing unrelated fields.
6. Edit `NEWS.md` into concise, user-facing release notes. Preserve its existing
   section style, and omit routine dependency, lint, formatting, CI, and
   generated-file maintenance.
7. Update `cran-comments.md` with the correct release type and version. Mention
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
version, and the body should summarize the release notes, version surfaces,
CRAN comments, and validation performed.

Monitor the pull request until every required GitHub Actions check passes and
there are no unresolved review threads. Address actionable review feedback,
reply to every review comment, push fixes, and revalidate without asking the
user to drive routine follow-up.

## Submit to CRAN

Keep the release pull request open and dispatch the submission workflow from
the release branch:

```bash
gh workflow run submit-cran.yaml --ref release-x.y.z
```

Find the resulting workflow run, watch it to completion, and inspect logs if it
fails. Fix problems on the release branch and retry only after the pull request
checks are green again. Once the workflow succeeds, report the pull request,
exact version, validation, workflow run, and that the maintainer must approve
the email from CRAN. Do not claim that the CRAN release is complete before that
manual approval and CRAN publication occur.
