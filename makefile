help:
	@echo "⚙ unmonoqueteclea's dotfiles"
	@echo "--------------------------------------------------"
	@echo " - stow: automatically add soft links for all dotfiles"
stow:
	echo "Restowing dotfiles..."
# gemini tries to rename linked settings.json file to settings.json.orig
# and create a new settings.json file
	rm -f $$HOME/.gemini/settings.json
	rm -f $$HOME/.gemini/settings.json.orig
	cd ../golem/config && stow --target=$$HOME/.gemini --restow .gemini
	cd ../golem/ && stow --target=$$HOME/.emacs.d --restow elisp/
	cd ../golem/ && stow --target=$$HOME/.bash --restow shell/

	stow --target=$$HOME --restow shell/
	stow --target=$$HOME --restow emacs/
	stow --target=$$HOME --restow ssh/
	stow --target=$$HOME --restow git/
	crontab "${DIR_VC}/dotfiles/crontab/crontab"
	@echo "✅ All dotfiles restowed correctly!"
