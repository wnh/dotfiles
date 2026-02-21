(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
 '(custom-safe-themes
   '("9a977ddae55e0e91c09952e96d614ae0be69727ea78ca145beea1aae01ac78d2" "e5e253a4d31d709f1b7147fe6bb237ed2b9353685eea9a9e18652ac917f48823" "95167736741bef2ad3e0543ed545dada5b95fef309883253387a2b14ab67db8d" "b29ba9bfdb34d71ecf3322951425a73d825fb2c002434282d2e0e8c44fce8185" "2628939b8881388a9251b1bb71bc9dd7c6cffd5252104f9ef236ddfd8dbbf74a" "8746b94181ba961ebd07c7397339d6a7160ee29c75ca1734aa3744274cbe0370" "b5c3c59e2fff6877030996eadaa085a5645cc7597f8876e982eadc923f597aca" "c5e7a36784b1955b28a89a39fef7c65ddc455b8e7fd70c6f5635cb21e4615670" "0af489efe6c0d33b6e9b02c6690eb66ab12998e2649ea85ab7cfedfb39dd4ac9" "f5661fd54b1e60a4ae373850447efc4158c23b1c7c9d65aa1295a606278da0f8" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "cca1d386d4a3f645c2f8c49266e3eb9ee14cf69939141e3deb9dfd50ccaada79" "7887cf8b470098657395502e16809523b629249060d61607c2225d2ef2ad59f5" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default))
 '(ispell-dictionary nil)
 '(notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "PRs" :query "from:notifications@github.com and \"requested changes on this pull request\"")))
 '(package-selected-packages
   '(rc-mode ob-http reason-mode scala-mode vue-mode mermaid-mode notmuch dash zig-mode solarized-theme org-ql nimbus-theme devdocs modus-themes tuareg compat lua-mode dap-mode nix-mode nodejs-repl flycheck add-node-modules-path web-mode typescript-mode kotlin-mode yaml-mode restclient swift-mode sql-indent dumb-jump treemacs markdown-mode rust-mode cider prettier-js exec-path-from-shell marginalia olivetti olivetti-mode elfeed go-mode inf-clojure clojure-mode eglot popper undo-tree magit rg gkroam orderless vertico spacemacs-theme spacemacs-themes evil-collection evil use-package))
 '(safe-local-variable-values '((c-comment-only-line-offset . 4)))
 '(undo-tree-history-directory-alist '((".*" . "~/.emacs.d/undo-history/")))
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#ffffff" :foreground "#000000" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 98 :width normal :foundry "PfEd" :family "Iosevka Term")))))
