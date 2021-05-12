package main

import (
	"log"
	"net/http"
)

func main() {
	http.Handle("/js/", http.FileServer(http.Dir("../../target/cljsbuild/public/")))
	http.Handle("/", http.FileServer(http.Dir("../../static/")))

	log.Fatal(http.ListenAndServe(":3000", nil))
}
