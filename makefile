help:
	@echo "⚙ unmonoqueteclea's dotfiles"
	@echo "--------------------------------------------------"
	@echo " - stow: automatically add soft links for all dotfiles"
stow:
	echo "Restowing dotfiles..."
	cd ../golem/config && stow --target=$$HOME/.config --restow .config
	cd ../golem/config && stow --target=$$HOME/.gemini --restow .gemini
	cd ../golem/ && stow --target=$$HOME/.emacs.d --restow elisp/
	cd ../golem/ && stow --target=$$HOME/.bash --restow shell/

	stow --target=$$HOME --restow shell/
	stow --target=$$HOME --restow emacs/
	stow --target=$$HOME --restow ssh/
	stow --target=$$HOME --restow git/
	crontab "${DIR_VC}/dotfiles/crontab/crontab"
	@echo "✅ All dotfiles restowed correctly!"
