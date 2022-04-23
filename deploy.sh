HOST_PATH="miaucloud:/home/zairex/htmls/soitbegins"

rsync -av frontend $HOST_PATH --delete --exclude=".*"
rsync -av backend $HOST_PATH --delete --exclude=".*"
