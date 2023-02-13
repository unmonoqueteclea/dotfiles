help:
	@echo "⚙ unmonoqueteclea's dotfiles"
	@echo "--------------------------------------------------"
	@echo " - stow: automatically add soft links for all dotfiles"
stow:
	stow --verbose --target=$$HOME --restow shell/
	stow --verbose --target=$$HOME --restow emacs/
	stow --verbose --target=$$HOME --restow ssh/
	stow --verbose --target=$$HOME --restow git/
	stow --verbose --target=$$HOME --restow xserver/
	stow --verbose --target=$$HOME --restow xmonad/
	stow --verbose --target=$$HOME --restow python/
	stow --verbose --target=$$HOME --restow mail/
	mkdir -p $$HOME/.config/rclone
	stow --verbose --target=$$HOME/.config/rclone --restow rclone/
