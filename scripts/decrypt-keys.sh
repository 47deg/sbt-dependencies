#!/bin/sh

openssl aes-256-cbc -K $encrypted_5cd1f76c4e65_key -iv $encrypted_5cd1f76c4e65_iv -in keys.tar.enc -out keys.tar -d;
tar -xvf keys.tar;
rm keys.tar;
chmod 600 travis-deploy-key;
cp travis-deploy-key ~/.ssh/id_rsa;