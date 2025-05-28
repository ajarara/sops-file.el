# sops-file.el
A package for viewing + editing [sops](https://github.com/getsops/sops) files. It handles sops pin prompts, I wrote this particularly to handle pin-guarded [age](https://github.com/FiloSottile/age/) keys [stored on yubikeys](https://github.com/str4d/age-plugin-yubikey). It will work out of the box for anything using graphical pinentry, and as long as sops prompts for other passphrases, sops-file will forward through emacs directly.

# requirements
- sops minimum version 3.10.2 (latest as of 5/20/25)
  - needed for https://github.com/getsops/sops/pull/1400

# installation
Using straight.el:
``` emacs-lisp
(use-package sops-file
  :straight
  `(:host github :repo "ajarara/sops-file.el")
  :config
  ;; adds an auto-mode-alist entry and yaml-mode hooks if yaml-mode is installed
  (sops-file-auto-mode 1))
```

After we cut a version we'll be on melpa{-stable}.

# usage
Without any configuration, users can simply do `M-x format-find-file`, select the file, then select format `sops-file`. Users can also, when visiting a file literally, `M-x format-decode-buffer` and select `sops-file` as the format.

`sops-file-auto-mode` is a global minor mode that attaches a hook to yaml-mode and installs an entry into auto-mode-alist, so that regular `M-x find-file`s apply the format (whether yaml-mode is installed or not). If creation rules are lax, then sops-file will consider any yaml file as a sops file and will apply the format!

Users are welcome to attach `sops-file-entry-hook` to any major mode they like: if sops-file determines that this file is managed, sops-file will attempt to apply the format encoding. On decyption failure we write to `*sops-file-error*`.

Users can also use this to create sops files for the first time, simply do `M-x format-find-file` on a new file: provided there is a creation_rule for the path it will never hit disk decrypted.

# api
Users shouldn't need to integrate with sops-file through writing code: for now it exposes no hooks and integrates with emacs directly. It might be useful to think of this package as:
- the sops-file format registration
- `sops-file-entry-hook`

# roadmap
I'm not sure when to define stability, I'd prefer to let this soak with some users before claiming stability. However there are some features I know need to be implemented to be a comprehensive experience. 
- Keygroup handling: sops can ask multiple times for passphrases for a single decryption pass
- GPG support: testing interactively, this works only with graphical pinentry: dropping into a tty and attempting to decrypt causes a "Operation cancelled error". In theory it should be as simple as figuring out what sops prints as the prompt for the pin/passphrase and adding it to the prompts in `sops-file-decode`.

## inspirations
epa-file, [sops.el](https://github.com/djgoku/sops), rot13
