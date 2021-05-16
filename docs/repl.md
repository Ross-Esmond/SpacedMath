# Starting the REPL

* Install vim-fireplace. I used Vundle to do this. This must only be done once.
* Run the CLI command `shadow-cljs watch app`.
* In a separate process, run the CLI command `go run src/go/main.go`.
* Connect to the site in a browser.
* In vim, execute the `:Connect 3333` command, followed by the `:Piggieback :app`
command.
* vim-fireplace commands should work.

As seen [here](https://github.com/tpope/vim-fireplace/issues/322).
