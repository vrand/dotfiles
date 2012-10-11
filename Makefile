# rsync
RSYNC_FLAGS=-av
RSYNC_EXCLUDE=--exclude ".git/" --exclude "Makefile" --exclude "README.md" --exclude "LICENSE"

all: files

files:
	@echo "» Synchronizing files"
	@rsync $(RSYNC_EXCLUDE) $(RSYNC_FLAGS) . ~ > /dev/null
