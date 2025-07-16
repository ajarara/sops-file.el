# sops-file.el
A package for viewing + editing [sops](https://github.com/getsops/sops) files. It handles sops pin prompts, I wrote this particularly to handle pin-guarded [age](https://github.com/FiloSottile/age/) keys [stored on yubikeys](https://github.com/str4d/age-plugin-yubikey).

# Requirements
- sops minimum version 3.10.2 (latest as of 5/20/25)
  - needed for https://github.com/getsops/sops/pull/1400
- env (for disabling pinentry)
- minimum emacs version unknown

# Installation / Quick Start
Using straight.el:
``` emacs-lisp
(use-package sops-file
  :straight
  `(:host github :repo "ajarara/sops-file.el")
  :after yaml-mode  ;; yaml-mode is optional
  :config
  ;; adds an auto-mode-alist entry and yaml-mode hooks if yaml-mode is installed
  (sops-file-auto-mode 1)
  ;; for age-plugin-yubikey users with multiple yubikeys, if a card isn't detected from your 
  ;; identity file sops will prompt you to insert it or skip it.
  ;; by default sops-file will skip these unavailable cards -- set this to nil to be prompted
  (setq sops-file-skip-unavailable-smartcards nil))
```

After we cut a version we'll be on melpa{-stable}.

# Usage
Without any configuration, users can simply do `M-x format-find-file`, select the file, then select format `sops-file`. Users can also, when visiting a file literally, `M-x format-decode-buffer` and select `sops-file` as the format.

`sops-file-auto-mode` is a global minor mode that attaches a hook to yaml-mode and installs an entry into auto-mode-alist, so that regular `M-x find-file`s apply the format (whether yaml-mode is installed or not). If creation rules are lax, then sops-file will consider any yaml file as a sops file and will apply the format!

Users are welcome to attach `sops-file-entry-trigger` to any major mode hook they like: if sops-file determines that this file is managed, sops-file will attempt to apply the format encoding. On decyption failure we write to `*sops-file-error*`.

Users can also use this to create sops files for the first time, simply do `M-x format-find-file` on any path. Provided there is a creation_rule for that path, the contents will never hit disk decrypted.

# API
Sops integrates with a diverse array of encryption providers: age, age-on-yubikeys, pgp, external KMS. sops-file.el simply ferries prompts between the decryption process and the user, however some glue code is needed to support a given setup. This is the primary place users should expect to write code: when sops encounters a prompt it cannot handle, it will hang for a bit until it fails and spit out the prompt in a dedicated error buffer.

Look to the existing prompt-handlers, namely `sops-file--prompt-handler-passphrase-identity` -- the important thing is to advance point past the prompt. It is generally correct to not use a save excursion: if the user gets prompted and quits out, then we assume it deliberate (at which point the process is killed and decryption fails).

Users shouldn't need to integrate with sops-file through writing non-prompt-handling code. The public API should be thought of as:
- the sops-file format registration (which happens on load)
- `sops-file-auto-mode`
- `sops-file-entry-trigger` (not a hook itself)
- defcustoms defined in the sops-file group

# Roadmap
Before claiming a stable 1.0, we're going to wait for more users beyond me. There are a couple features I know that need to be implemented for a comprehensive experience: 
- confirmed, tested pgp support: 
  - looking at source pgp follows the same logic as age: lean on gpg-agent to request the passphrase (sops uses gpg-agent as well, if available). If no connection can be made, read it directly from stdin. Gpg has many more moving parts over age, so an isolated test is going to be more complex. But in theory it should work fine, with graphical pinentry or with no daemon (in my tests non-graphical pinentry doesn't work).
- tramp support: as of now, none known. I remember there are some caveats towards remote shell commands and having to mix in stderr/stdout, which could be difficult to handle here.
- support for external KMS providers (testing will be difficult)


## dubious directions
What follows are things that I think would be useful, but would probably mean compromises to the complexity of the code as is or would be difficult to test. Your comments are welcome on these (or anything else related to this project for that matter), as issues.
### sops-file-auto-mode should apply to all buffers on toggle
often while testing this I would find-file into a sops file I'd expect to be decrypted, only to realize that I needed to toggle the mode. I would then toggle the mode and remember I have to revert the buffer for the hook to fire. I would prefer that this minor mode felt alive like all the other minor modes.

There are two major points against this:
- I bet most users toggle on the mode in their init file and don't toggle it interactively (if they do, they probably want to have it be disabled on one specific file in which case they should use find-file-literally).
- Applying the format to buffers not visible is a subpar experience when passphrases are prompted interactively: multiple passphrases, multiple files, we'd need to flit them in or something.

### sops exec-env password prompt
Simple, add the prompts to comint-password-prompt-regexp for similar behavior to allow for read-password during shell command invocations. This is eventually used by comint-output-filter-functions. This can probably be added unconditionally.

## inspirations
epa-file, [sops.el](https://github.com/djgoku/sops), rot13
