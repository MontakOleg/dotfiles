dotfiles_dir=~/dotfiles

ln -sh $dotfiles_dir/vim ~/
mv ~/vim ~/.vim
ln -sh $dotfiles_dir/vim/rc ~/.vimrc
ln -sh $dotfiles_dir/zsh/rc ~/.zshrc
ln -sh $dotfiles_dir/tig/rc ~/.tigrc
ln -sh $dotfiles_dir/ruby/gemrc ~/.gemrc
