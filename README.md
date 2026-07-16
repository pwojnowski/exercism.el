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

Invoke `M-x exercism` or `C-c x` to open the exercise list for the current track. On first use, if setup is incomplete, the self-check report opens automatically; if setup is complete but no track is selected, the track picker opens instead.

## Configure

[Get your API token](https://exercism.org/settings/api_cli) and run `M-x exercism-configure`. You will be prompted for your API token and workspace directory (defaulting to a saved workspace, or `~/Exercism`).

### Path Configuration

`M-x exercism-configure` writes the chosen workspace to the Exercism CLI config via `--workspace`. To change it later, run configure again or use:

```bash
exercism configure --workspace "path/to/dir"
```

#### no-littering

Pick your no-littering directory when prompted during `M-x exercism-configure`, or set it in Emacs after configuring on the CLI:

```emacs-lisp
(setq exercism--workspace (no-littering-expand-var-file-name "exercism/"))
```

## Set Current Track

Press `t` in the exercise list to open the track picker. An API token is required so enrollment status can be checked.

- `RET` on a joined track — select it and initialize locally if needed
- `RET` on a not-joined track — open the track page in your browser, join there, confirm when prompted, then Emacs verifies enrollment before selecting

The first run on a new track may take a few minutes while `hello-world` is downloaded locally.

## List Exercises

`M-x exercism` or `C-c x` opens the exercise list with solved/unsolved status for every exercise on the current track. Unsolved exercises retain the track's response order, followed by solved exercises in their response order.

The exercise list buffer supports:

- `RET` — open exercise (downloads if needed)
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

`?` or `M-x exercism-self-check` verifies CLI setup, config, token, workspace, and API connectivity. It also opens automatically when you run `C-c x` or `M-x exercism` before Exercism is configured locally.

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

- Track enrollment uses the Exercism website in a browser; Emacs verifies enrollment after you confirm.
- The exercise list may include locked exercises because the CLI does not expose unlock status alone.

## Contributing

PRs, suggestions, and bug reports are welcome.

## License

GPL-3.0-or-later
