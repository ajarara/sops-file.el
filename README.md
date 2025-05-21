# sops-file.el
A package for viewing + editing [sops](https://github.com/getsops/sops) files. It handles sops pin prompts, I wrote this particularly to handle pin-guarded [age](https://github.com/FiloSottile/age/) keys stored on yubikeys.

# requirements
- sops minimum version 3.10.2 (latest as of 5/20/25)
  - needed for https://github.com/getsops/sops/pull/1400

# proposed implementation
Sops allows users to have encrypted files map to YAML, JSON, or anything. In the case of YAML, JSON, sops also supports [partial decryption](https://github.com/getsops/sops?tab=readme-ov-file#49encrypting-only-parts-of-a-file).

So we want whatever entry/exit mechanism (tramp, auto-mode-alist, format-alist, a minor mode) to be able to:
- change the major mode if applicable
- override the read/save functionality
- prompt the user for a PIN if needed[^1]

We'll do this by using 'sops filestatus' to determine whether to run the yaml file through sops as part of a major mode hook. Then, if it hits, we'll decrypt, prompting if necessary, and I think we'll use `major-mode-suspend` and redecide based off of contents/file-name[^2]. We also overwrite write-region for the buffer to save through piping buffer contents through sops.


[^1]: Sops can use any number of credentials, the ones that are interesting to me are gpg/age keys. Gpg has pinentry, and while age has a pinentry capable implementation (rage), sops links against age as a library instead of a program (so no possibility of letting users work around it). However sops passes through pin requests, so we just have to also pass them through.
[^2]: Because decrypted contents can be yaml, we can't blindly remove .yaml (or .yml or whatever). We could maybe go with "remove .yaml unless it's the only suffix" and let the user override with something more advanced..

## inspirations
epa-file, [sops.el](https://github.com/djgoku/sops)
