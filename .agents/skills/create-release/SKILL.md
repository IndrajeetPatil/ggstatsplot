---
name: create-release
description: Prepare, validate, submit, resume, or publish a CRAN release for this R package. Use only for release work, not ordinary maintenance pull requests.
---

# Create a CRAN release

Use the repository's two-stage workflow: submit from an open release branch,
then publish the GitHub Release from `main` only after CRAN acceptance.

## Establish the release target

1. Read `AGENTS.md` and `.github/workflows/submit-cran.yaml`.
2. Verify GitHub authentication, fetch `origin`, and preserve unrelated local
   changes. Use a clean worktree when needed.
3. Query CRAN for the latest version actually published. Cross-check it against
   `DESCRIPTION`, open release pull requests, and GitHub tags and releases.
4. If resuming an existing release branch, pull request, or rejected
   submission, keep its target version. Do not increment past it or create a
   duplicate.
5. When starting a new release and the user has not specified the release type,
   ask whether it is `patch`, `minor`, or `major`. Compute the new version from
   the CRAN-published version, not from an unaccepted submission.
6. Start a new `release-x.y.z` branch from the latest `origin/main` only when no
   matching release is already in flight.

## Prepare and validate

1. Review commits and merged pull requests since the latest CRAN release and
   the existing development section in `NEWS.md`. Do not invent changes.
2. Synchronize the target version in `DESCRIPTION`, the first `NEWS.md`
   heading, and `codemeta.json`; remove any development `.9000` suffix.
3. Keep NEWS concise and user-facing. Omit routine dependency, lint,
   formatting, CI, and generated-file maintenance.
4. Update `cran-comments.md` accurately. Mention a CRAN issue only when the
   repository evidence supports it.
5. Do not refresh dependencies merely because this is a release.
6. Run the complete release gate:

   ```bash
   air format . --check
   make lint
   make hooks
   make check
   git diff --check
   ```

7. Clean build artifacts, inspect the complete diff, commit the validated
   release state, and push the release branch. Only then open or update a
   ready-for-review pull request against `main`; its title must contain the
   exact text `CRAN Release` and its body must describe the final net diff and
   validation.
8. Wait for every required check to pass. Address, reply to, and resolve every
   actionable review thread. After each fix, rerun the affected validation,
   commit and push it, and re-fetch the live thread and check state.

## Submit while the pull request stays open

1. Keep the `CRAN Release` pull request open. Verify that the worktree is clean
   and the remote release-branch SHA matches the validated local `HEAD`, then
   dispatch the package workflow from that branch:

   ```bash
   gh workflow run submit-cran.yaml --ref release-x.y.z
   ```

2. Find and watch the resulting run. On a non-default branch, the reusable
   workflow must build and submit to CRAN while the GitHub Release job is
   skipped. Inspect the logs and verify that no tag or GitHub Release was
   created. Record the workflow run's head SHA as the exact submitted commit.
3. If submission fails, fix it on the same release branch and retry only after
   the updated pull request checks are green.
4. When submission succeeds, report the exact version and workflow run, remind
   the maintainer to confirm the CRAN email, and stop. Submission success and
   email confirmation are not CRAN acceptance.
5. Never merge the pull request, create a tag, or create a GitHub Release until
   the user explicitly states that CRAN accepted the package.

## Publish only after CRAN acceptance

1. After the user explicitly reports acceptance, verify that CRAN publishes the
   target version.
2. Squash-merge the `CRAN Release` pull request and verify that `main` contains
   the accepted version.
3. Dispatch `submit-cran.yaml` from `main` and watch it to completion. On the
   default branch, the reusable workflow must skip CRAN submission, resolve the
   most recently merged matching `CRAN Release <version>` pull request, and
   rebuild and publish from that pull request's original submitted head SHA.
4. Verify that the tag targets the recorded submitted SHA, not a later `main`
   tree; the source tarball is attached; and the release body is the complete
   matching `NEWS.md` version section verbatim. Never use generated release
   notes or summarize NEWS.
