#!/usr/bin/env bash

# Key repeat
# defaults write -g KeyRepeat -int 2
# defaults write -g InitialKeyRepeat -int 25
# defaults write -g ApplePressAndHoldEnabled -bool false
defaults write com.apple.HIToolbox AppleFnUsageType -int 0

# Trackpad: touch to click
defaults write com.apple.AppleMultitouchTrackpad Clicking -bool true
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
defaults write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
# defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1

# Dock
defaults write com.apple.dock orientation -string left
defaults write com.apple.dock autohide -bool true
defaults write com.apple.dock show-recents -bool false
defaults write com.apple.dock persistent-apps -array

# Dock active corners
defaults write com.apple.dock wvous-tl-corner -int 1
defaults write com.apple.dock wvous-tr-corner -int 1
defaults write com.apple.dock wvous-bl-corner -int 1
defaults write com.apple.dock wvous-br-corner -int 1
defaults write com.apple.dock wvous-tl-modifier -int 0
defaults write com.apple.dock wvous-tr-modifier -int 0
defaults write com.apple.dock wvous-bl-modifier -int 0
defaults write com.apple.dock wvous-br-modifier -int 0

# Apply dock changes
killall Dock
