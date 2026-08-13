#!/usr/bin/env bash

meta_vc_el='/home/erwann/github/rogard/persemacs/el/init.el'
meta_startup='/home/erwann/.emacs'
if [[ -f "${meta_startup}" ]]; then      
    cp "${meta_startup}" "${meta_startup}.bak" || {
        echo "Failed to create backup of ${meta_startup}"; exit 1;
    }
fi
if [[ ! -f "${meta_vc_el}" ]]; then
    echo "Init file ${meta_vc_el} not found"; exit 1;
fi
ln -sf "${meta_vc_el}" "${meta_startup}" || {
    echo "Failed to create link"; exit 1;
}
emacs --batch --eval "(kill-emacs)" || {
    echo "Failed to launch Emacs with the new link"; exit 1;
}
echo "Link created and Emacs verified successfully."
