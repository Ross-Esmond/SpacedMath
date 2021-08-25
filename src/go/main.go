package main

import (
    "log"
    "net/http"
    "fmt"
    "os"
    "encoding/json"

    "github.com/gorilla/mux"
    "github.com/gorilla/sessions"

    "github.com/markbates/goth"
    "github.com/markbates/goth/gothic"
    "github.com/markbates/goth/providers/google"

    "gorm.io/gorm"
    "gorm.io/driver/sqlite"
)

type Problem struct {
    gorm.Model
    Problem string
    Answer  string
    Author  string
    Add      uint
    Constant uint
    Scaler   uint
    Power    uint
    Exp      uint
    Product  uint
}

var store = sessions.NewCookieStore([]byte(os.Getenv("SESSION_SECRET")))
var db, err = gorm.Open(sqlite.Open("sm.db"), &gorm.Config{})

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

func ProblemHandler(res http.ResponseWriter, req *http.Request) {
    session, _ := store.Get(req, "session-name")
    email := session.Values["email"]
    if email == nil {
        res.WriteHeader(http.StatusUnauthorized)
    } else {
        var problem Problem
        json.NewDecoder(req.Body).Decode(&problem)
        problem.Author = email.(string)
        db.Create(&problem)
    }
}

func ProblemServer(res http.ResponseWriter, req *http.Request) {
    var problems []Problem
    db.Find(&problems)
    res.Header().Set("Content-Type", "application/json")
    problemStrings := make([]string, len(problems))
    for i, pr := range problems {
        problemStrings[i] = pr.Problem
    }
    json.NewEncoder(res).Encode(problemStrings)
}

func ProblemDeleter(res http.ResponseWriter, req *http.Request) {
    var problems []Problem
    session, _ := store.Get(req, "session-name")
    email := session.Values["email"]
    if email == nil {
        res.WriteHeader(http.StatusUnauthorized)
    } else {
        var problem Problem
        json.NewDecoder(req.Body).Decode(&problem)
        db.Where("Problem = ? AND Author = ?", problem.Problem, email).Delete(&problems)
        res.Header().Set("Content-Type", "application/json")
        json.NewEncoder(res).Encode(true)
    }
}

func RedirectHandler(w http.ResponseWriter, req *http.Request) {
    target := "https://" + req.Host + req.URL.Path
    if len(req.URL.RawQuery) > 0 {
        target += "?" + req.URL.RawQuery
    }
    http.Redirect(w, req, target, http.StatusPermanentRedirect)
}

func main() {
    db.AutoMigrate(&Problem{})

    redirect := "http://localhost:3000/api/callback/google"
    if os.Getenv("BUILD") == "PROD" {
        redirect = "https://spacedmath.com/api/callback/google"
    }

    goth.UseProviders(
        google.New(os.Getenv("GOOGLE_KEY"), os.Getenv("GOOGLE_SECRET"), redirect),
    )

    r := mux.NewRouter()
    r.HandleFunc("/api/callback/{provider}", CallbackHandler)
    r.HandleFunc("/api/auth/{provider}", AuthHandler)
    r.HandleFunc("/api/profile", ProfileHandler)
    r.HandleFunc("/api/logout", LogoutHandler)
    r.HandleFunc("/api/problems", ProblemHandler).Methods("POST")
    r.HandleFunc("/api/problems", ProblemServer).Methods("GET")
    r.HandleFunc("/api/problems", ProblemDeleter).Methods("DELETE")

    http.Handle("/js/", http.FileServer(http.Dir("resources/public/")))
    http.Handle("/", http.FileServer(http.Dir("static/")))
    http.Handle("/api/", r)

    if os.Getenv("BUILD") == "PROD" {
        http.ListenAndServe(":80", http.HandlerFunc(RedirectHandler))
        fmt.Println("listening on localhost:443")
        log.Fatal(http.ListenAndServeTLS(":443", "cert.pem", "key.pem", nil))
    } else {
        fmt.Println("listening on localhost:3000")
        log.Fatal(http.ListenAndServe(":3000", nil))
    }
}
