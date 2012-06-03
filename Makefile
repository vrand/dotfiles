# rsync
RSYNC_FLAGS=-av
RSYNC_EXCLUDE=--exclude ".git/" --exclude "Makefile" --exclude "README.md"

all: files xmonad

files:
	@echo ">> Synchronizing files"
	@rsync $(RSYNC_EXCLUDE) $(RSYNC_FLAGS) . ~ > /dev/null

xmonad:
	@echo ">> Recompiling xmonad"
	@xmonad --recompile
