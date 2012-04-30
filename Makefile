# rsync
RSYNC_FLAGS=-av
RSYNC_EXCLUDE=--exclude ".git/" --exclude "Makefile" --exclude "README.md"

# pathogen
PATHOGEN_URL=https://raw.github.com/tpope/vim-pathogen/master/autoload/pathogen.vim


all: files submodules pathogen

files:
	echo ">> Synchronizing files"
	rsync $(RSYNC_EXCLUDE) $(RSYNC_FLAGS) . ~

submodules:
	echo ">> Updating git repos"
	git submodule init
	git submodule update

pathogen:
	echo ">> Grabbing last version of pathogen"
	mkdir -p ~/.vim/autoload ~/.vim/bundle
	curl -so ~/.vim/autoload/pathogen.vim $(PATHOGEN_URL)

reload:
	source ~/.zshrc
