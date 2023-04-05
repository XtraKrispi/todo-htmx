target: watch_server

watch_server:
	ghcid -c cabal repl -W -T main
