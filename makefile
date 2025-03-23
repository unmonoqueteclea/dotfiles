help:
	@echo "⚙ unmonoqueteclea's dotfiles"
	@echo "--------------------------------------------------"
	@echo " - stow: automatically add soft links for all dotfiles"
stow:
	@echo "Restowing dotfiles..."
	@stow --target=$$HOME --restow shell/
	@stow --target=$$HOME --restow emacs/
	@stow --target=$$HOME --restow ssh/
	@stow --target=$$HOME --restow git/
	@stow --target=$$HOME --restow python/
	@crontab "${DIR_VC}/dotfiles/crontab/crontab"
	@echo "✅ All dotfiles restowed correctly!"
