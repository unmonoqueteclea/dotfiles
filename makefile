help:
	@echo "⚙ unmonoqueteclea's dotfiles"
	@echo "--------------------------------------------------"
	@echo " - stow: automatically add soft links for all dotfiles"
stow:
	@echo "Restowing dotfiles..."
	@crontab "${DIR_VC}/dotfiles/crontab/crontab"
	@stow --target=$$HOME --restow shell/
	@stow --target=$$HOME --restow emacs/
	@stow --target=$$HOME --restow ssh/
	@stow --target=$$HOME --restow git/
	@stow --target=$$HOME --restow xserver/
	@stow --target=$$HOME --restow xmonad/
	@stow --target=$$HOME --restow python/
	@stow --target=$$HOME --restow mail/
	@mkdir -p $$HOME/.config/rclone
	@stow --target=$$HOME/.config/rclone --restow rclone/
	@echo "✅ All dotfiles restowed correctly!"
