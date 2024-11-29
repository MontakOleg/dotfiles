#!/bin/bash

# the tail command will skip the first line because it's an informational message
#interfaces="$(networksetup -listallnetworkservices | tail +2)"
interfaces="Wi-Fi"

IFS=$'\n' # split on newlines in the for loops

for interface in $interfaces; do
  echo "Setting proxy on $interface"
  networksetup -setwebproxy "$interface" localhost 8080
  networksetup -setwebproxystate "$interface" on
  networksetup -setsecurewebproxy "$interface" localhost 8080
  networksetup -setsecurewebproxystate "$interface" on
done

if [[ $1 == "--web" ]] ; then
    mitmweb
else
    mitmproxy
fi

for interface in $interfaces; do
  echo "Disabling proxy on $interface"
  networksetup -setwebproxystate "$interface" off
  networksetup -setsecurewebproxystate "$interface" off
done
