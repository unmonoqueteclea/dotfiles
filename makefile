help:
	@echo "⚙️ unmonoqueteclea's dotfiles"
	@echo "--------------------------------------------------"
	@echo " - stow: automatically add soft links for all dotfiles"
stow:
	stow --verbose --target=$$HOME --restow shell/
	stow --verbose --target=$$HOME --restow emacs/
	stow --verbose --target=$$HOME --restow ssh/
	stow --verbose --target=$$HOME --restow git/
	stow --verbose --target=$$HOME --restow xserver/
	stow --verbose --target=$$HOME --restow xmonad/

