package main

import (
	"log"
	"net/http"
	"fmt"
	"os"

	"github.com/gorilla/mux"
	"github.com/gorilla/sessions"

	"github.com/markbates/goth"
	"github.com/markbates/goth/gothic"
	"github.com/markbates/goth/providers/google"
)

var store = sessions.NewCookieStore([]byte(os.Getenv("SESSION_SECRET")))


func CallbackHandler(res http.ResponseWriter, req *http.Request) {
	user, err := gothic.CompleteUserAuth(res, req)
	if err != nil {
		fmt.Fprintln(res, err)
		return
	}

	session, _ := store.Get(req, "session-name")
	session.Values["email"] = user.Email
	fmt.Println(session.Values["email"])
	err = session.Save(req, res)

	res.Header().Set("Location", "/")
	res.WriteHeader(http.StatusTemporaryRedirect)
}


func AuthHandler(res http.ResponseWriter, req *http.Request) {
	if user, err := gothic.CompleteUserAuth(res, req); err == nil {
		fmt.Println(user)
	} else {
		gothic.BeginAuthHandler(res, req)
	}
}


func ProfileHandler(res http.ResponseWriter, req *http.Request) {
	session, _ := store.Get(req, "session-name")
	email := session.Values["email"]
	if email == nil {
		res.Write([]byte(""))
	} else {
		res.Write([]byte(email.(string)))
	}
}

func LogoutHandler(res http.ResponseWriter, req *http.Request) {
	session, _ := store.Get(req, "session-name")
	session.Options.MaxAge = -1
	_ = session.Save(req, res)

	res.Write([]byte(""))
}


func main() {
	goth.UseProviders(
		google.New(os.Getenv("GOOGLE_KEY"), os.Getenv("GOOGLE_SECRET"), "http://localhost:3000/api/callback/google"),
	)

	m := make(map[string]string)
	m["google"] = "Google"

	var keys []string
	for k := range m {
		keys = append(keys, k)
	}

	r := mux.NewRouter()
	r.HandleFunc("/api/callback/{provider}", CallbackHandler)
	r.HandleFunc("/api/auth/{provider}", AuthHandler)
	r.HandleFunc("/api/profile", ProfileHandler)
	r.HandleFunc("/api/logout", LogoutHandler)

	http.Handle("/js/", http.FileServer(http.Dir("resources/public/")))
	http.Handle("/", http.FileServer(http.Dir("static/")))
	http.Handle("/api/", r)

	fmt.Println("listening on localhost:3000")
	log.Fatal(http.ListenAndServe(":3000", nil))
}
