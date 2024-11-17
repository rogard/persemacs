#!/usr/bin/env bash
link_name="${HOME}/.emacs"

if [[ -f "${link_name}" ]]; then
    cp "${link_name}" "${HOME}/.emacs.bak" || {
        echo "Failed to create backup of ${link_name}"; exit 1;
    }
fi
target="${PWD}/.emacs.d/init.el"

if [[ ! -f "${target}" ]]; then
    echo "Target ${target} not found"; exit 1;
fi
ln -sf "${target}" "${link_name}" || {
    echo "Failed to create symlink"; exit 1;
}
emacs --batch --eval "(kill-emacs)" || {
    echo "Failed to launch Emacs with the new symlink"; exit 1;
}
echo "Symlink created and Emacs verified successfully."
