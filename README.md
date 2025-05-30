# sops-file.el
A package for viewing + editing [sops](https://github.com/getsops/sops) files. It handles sops pin prompts, I wrote this particularly to handle pin-guarded [age](https://github.com/FiloSottile/age/) keys [stored on yubikeys](https://github.com/str4d/age-plugin-yubikey).

# requirements
- sops minimum version 3.10.2 (latest as of 5/20/25)
  - needed for https://github.com/getsops/sops/pull/1400
- env (for disabling pinentry)
- minimum emacs version unknown

# installation
Using straight.el:
``` emacs-lisp
(use-package sops-file
  :straight
  `(:host github :repo "ajarara/sops-file.el")
  :after yaml-mode  ;; yaml-mode is optional
  :config
  ;; adds an auto-mode-alist entry and yaml-mode hooks if yaml-mode is installed
  (sops-file-auto-mode 1))
```

After we cut a version we'll be on melpa{-stable}.

# usage
Without any configuration, users can simply do `M-x format-find-file`, select the file, then select format `sops-file`. Users can also, when visiting a file literally, `M-x format-decode-buffer` and select `sops-file` as the format.

`sops-file-auto-mode` is a global minor mode that attaches a hook to yaml-mode and installs an entry into auto-mode-alist, so that regular `M-x find-file`s apply the format (whether yaml-mode is installed or not). If creation rules are lax, then sops-file will consider any yaml file as a sops file and will apply the format!

Users are welcome to attach `sops-file-entry-hook` to any major mode they like: if sops-file determines that this file is managed, sops-file will attempt to apply the format encoding. On decyption failure we write to `*sops-file-error*`.

Users can also use this to create sops files for the first time, simply do `M-x format-find-file` on any path. Provided there is a creation_rule for that path, the contents will never hit disk decrypted.

# api
Users shouldn't need to integrate with sops-file through writing code: for now it exposes no hooks and integrates with emacs directly. The public API should be thought of as:
- the sops-file format registration (which happens on load)
- `sops-file-auto-mode`
- `sops-file-entry-hook`
- defcustoms/defvars

# roadmap
Before claiming a stable 1.0, we're going to wait for more users beyond me. There are a couple features I know that need to be implemented for a comprehensive experience: 
- Keygroup handling: sops can ask multiple times for passphrases for a single decryption pass
  - if you use age exclusively, setup is convoluted and might not even work: sops supports only one identity file, and that identity file cannot be encrypted with multiple passphrases. So maybe an armored SOPS_AGE_KEY environment variable?
- confirmed, tested pgp support: 
  - looking at source pgp follows the same logic as age: lean on gpg-agent to request the passphrase (age uses gpg-agent as well, if available). If no connection can be made, read it directly from stdin. Gpg has many more moving parts over age, so an isolated test is going to be more complex. But in theory it should work fine, with graphical pinentry or with no daemon (non-graphical pinentry will not work).

## inspirations
epa-file, [sops.el](https://github.com/djgoku/sops), rot13
