#!/usr/bin/bash
sudo rm -rf /run/user/1000
sudo mkdir /run/user/1000 && sudo chmod 700 /run/user/1000 && sudo chown $(whoami): /run/user/1000
