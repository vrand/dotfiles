# rsync
RSYNC_FLAGS=-av
RSYNC_EXCLUDE=--exclude ".git/" --exclude "Makefile" --exclude "README.md"

# pathogen
PATHOGEN_URL=https://raw.github.com/tpope/vim-pathogen/master/autoload/pathogen.vim


all: files xmonad submodules pathogen

files:
	@echo ">> Synchronizing files"
	@rsync $(RSYNC_EXCLUDE) $(RSYNC_FLAGS) . ~ > /dev/null

xmonad:
	@echo ">> Recompiling xmonad"
	@xmonad --recompile

submodules:
	@echo ">> Updating git repos"
	@git submodule init > /dev/null
	@git submodule update > /dev/null

pathogen:
	@echo ">> Grabbing last version of pathogen"
	@mkdir -p ~/.vim/autoload ~/.vim/bundle > /dev/null
	@curl -so ~/.vim/autoload/pathogen.vim $(PATHOGEN_URL) > /dev/null
