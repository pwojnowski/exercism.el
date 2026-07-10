# exercism.el

Emacs integration for [Exercism](https://exercism.org).

This is a modernized fork of [anonimitoraf/exercism.el](https://github.com/anonimitoraf/exercism.el) with fewer dependencies, an exercise list buffer with solved/unsolved status, and a self-check command.

## Prerequisites

Download the `exercism` CLI by following [the official guide](https://exercism.org/cli-walkthrough).

## Dependencies

- Emacs 29.1+
- [request](https://github.com/tkf/emacs-request)
- [transient](https://github.com/magit/transient)

## Quick Start

```emacs-lisp
(use-package request
  :ensure t
  :defer t)

(use-package exercism
  :ensure nil
  :load-path "~/projects/mine/exercism.el"
  :commands (exercism exercism-configure exercism-cli-version exercism-self-check
                      exercism-list-exercises exercism-list-unsolved-exercises)
  :bind (("C-c x" . exercism)))
```

Invoke `M-x exercism` or `C-c x` to open the transient menu.

<img src="./demos/menu.png" width=300 />

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

- `l` — list all exercises with solved/unsolved status
- `u` — list unsolved exercises only

The exercise list buffer supports:

- `RET` — open exercise (downloads if needed)
- `s` — submit exercise
- `n` / `p` — move between exercises
- `g` — reload list
- `q` — quit

## Open an Exercise

Open exercises from the list buffer, or use `e` to open a previously downloaded exercise offline.

## Download All Unlocked Exercises

Use `d` in the transient menu to download all unlocked exercises for the current track.

## Run Tests

Run tests for the current exercise. Results appear in `*compilation*`.

Requires Exercism CLI 3.2.0+ (`M-x exercism-cli-version`).

## Submit

- `s` — submit current exercise
- `S` — submit, then open the submission page in a browser

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
