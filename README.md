# exercism.el

Emacs integration for [Exercism](https://exercism.org).

This is a modernized fork of [anonimitoraf/exercism.el](https://github.com/anonimitoraf/exercism.el) with fewer dependencies, an exercise list buffer with solved/unsolved status, and a self-check command.

## Prerequisites

Download the `exercism` CLI by following [the official guide](https://exercism.org/cli-walkthrough).

## Dependencies

- Emacs 29.1+
- [request](https://github.com/tkf/emacs-request)

## Quick Start

```emacs-lisp
(use-package request
  :ensure t
  :defer t)

(use-package exercism
  :ensure nil
  :load-path "~/projects/mine/exercism.el"
  :commands (exercism exercism-configure exercism-self-check)
  :bind (("C-c x" . exercism)))
```

Invoke `M-x exercism` or `C-c x` to open the exercise list for the current track.

## Configure

[Get your API token](https://exercism.org/settings/api_cli) and run `M-x exercism-configure`.

### Path Configuration

Before customizing `exercism--workspace`, change it on the CLI first:

```bash
exercism -w "path/to/dir"
```

#### no-littering

```emacs-lisp
(setq exercism--workspace (no-littering-expand-var-file-name "exercism/"))
```

## Set Current Track

Choose the track you want to work on. The first run may take a few minutes while the track initializes locally.

## List Exercises

`M-x exercism` or `C-c x` opens the exercise list with solved/unsolved status for every exercise on the current track.

The exercise list buffer supports:

- `RET` — open exercise (downloads if needed)
- `u` — toggle unsolved-only filter
- `s` — submit exercise
- `n` / `p` — move between exercises
- `g` — reload list
- `d` — download all unlocked exercises
- `q` — quit

## Open an Exercise

Open exercises from the list buffer.

## Download All Unlocked Exercises

Use `d` in the exercise list buffer to download all unlocked exercises for the current track.

## Run Tests

Run tests for the current exercise. Results appear in `*compilation*`.

Requires Exercism CLI 3.2.0+ (check with `?` or `M-x exercism-self-check`).

## Submit

- `s` — submit current exercise
- `S` — submit, then open the submission page in a browser

While a submit is in progress, the exercise list Status column shows `submitting` (animated), then `submitted` or `failed` when the CLI returns. Press `g` in the list to refresh statuses from the API.

Marking an exercise as complete still happens on the Exercism website.

## Self-Check

`?` or `M-x exercism-self-check` verifies CLI setup, config, token, workspace, and API connectivity.

## Testing

From the repository root:

```bash
./scripts/run-exercism-ert.sh
```

By default, dependencies are loaded from `~/.emacs.d/elpa`. Override with:

```bash
EMACS_USER_DIR=~/.emacs.d ./scripts/run-exercism-ert.sh
```

## Known Limitations

- Registering for a track is not supported in Emacs; use the [Exercism website](https://exercism.org/tracks).
- The exercise list may include locked exercises because the CLI does not expose unlock status alone.

## Contributing

PRs, suggestions, and bug reports are welcome.

## License

GPL-3.0-or-later
