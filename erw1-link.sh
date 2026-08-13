#!/usr/bin/env bash

vc_path='/home/erwann/github/rogard/persemacs/erw1-init.el'
su_path='/home/erwann/.emacs'
if [[ -f "${su_path}" ]]; then      
    cp "${su_path}" "${su_path}.bak" || {
        echo "Failed to create backup of ${su_path}"; exit 1;
    }
fi
if [[ ! -f "${vc_path}" ]]; then
    echo "Init file ${vc_path} not found"; exit 1;
fi
ln -sf "${vc_path}" "${su_path}" || {
    echo "Failed to create link"; exit 1;
}
emacs --batch --eval "(kill-emacs)" || {
    echo "Failed to launch Emacs with the new link"; exit 1;
}
echo "Link created and Emacs verified successfully."
