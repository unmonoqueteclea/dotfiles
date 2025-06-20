help:
	@echo "⚙ unmonoqueteclea's dotfiles"
	@echo "--------------------------------------------------"
	@echo " - stow: automatically add soft links for all dotfiles"
stow:
	echo "Restowing dotfiles..."
	stow --target=$$HOME --restow shell/
	stow --target=$$HOME --restow emacs/
	stow --target=$$HOME --restow ssh/
	stow --target=$$HOME --restow git/
	crontab "${DIR_VC}/dotfiles/crontab/crontab"
	cd ../golem/ && stow --target=$$HOME/.emacs.d/elibs/ --restow elisp/
	@echo "✅ All dotfiles restowed correctly!"
