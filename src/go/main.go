package main

import (
	"log"
	"net/http"
	"fmt"
)

func main() {
	http.Handle("/js/", http.FileServer(http.Dir("resources/public/")))
	http.Handle("/", http.FileServer(http.Dir("static/")))

	fmt.Println("listening on localhost:3000")
	log.Fatal(http.ListenAndServe(":3000", nil))
}
