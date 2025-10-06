package main

import (
	"encoding/csv"
	"fmt"
	"log"
	"math/rand"
	"net/http"
	"os"
	"strings"
	"time"
)

type CSVData struct {
	filename string
	headers  []string
	genRow   func() []string
}

var csvTemplates = make(map[string]*CSVData)

func init() {
	rand.Seed(time.Now().UnixNano())

	users := &CSVData{
		filename: "users.csv",
		headers:  []string{"id", "name", "email", "created_at"},
		genRow: func() []string {
			id := fmt.Sprintf("%d", rand.Intn(10000)+1)

			firstNames := []string{"John", "Jane", "Bob", "Alice", "David", "Sarah", "Michael", "Emma", "James", "Olivia"}
			lastNames := []string{"Smith", "Johnson", "Williams", "Jones", "Brown", "Davis", "Miller", "Wilson", "Moore", "Taylor"}
			name := firstNames[rand.Intn(len(firstNames))] + " " + lastNames[rand.Intn(len(lastNames))]

			nameParts := strings.Split(strings.ToLower(name), " ")
			email := nameParts[0] + "." + nameParts[1] + "@example.com"

			daysAgo := rand.Intn(365)
			createdAt := time.Now().AddDate(0, 0, -daysAgo).Format(time.RFC3339)

			return []string{id, name, email, createdAt}
		},
	}

	products := &CSVData{
		filename: "products.csv",
		headers:  []string{"id", "name", "price", "stock"},
		genRow: func() []string {
			id := fmt.Sprintf("%d", rand.Intn(10000)+1)
			productTypes := []string{"Laptop", "Smartphone", "Headphones", "Monitor", "Keyboard", "Mouse", "Tablet", "Camera", "Printer", "Speaker"}
			brands := []string{"TechX", "Globex", "Initech", "Umbrella", "Stark", "Wayne", "Acme", "Cyberdyne", "Oscorp", "Massive"}
			name := brands[rand.Intn(len(brands))] + " " + productTypes[rand.Intn(len(productTypes))]
			price := fmt.Sprintf("%.2f", 9.99+rand.Float64()*1990.0)
			stock := fmt.Sprintf("%d", rand.Intn(501))

			return []string{id, name, price, stock}
		},
	}

	csvTemplates["users"] = users
	csvTemplates["products"] = products
}

func generateRandomCSV(template *CSVData, min, max int) [][]string {
	numRows := rand.Intn(max-min+1) + min

	data := make([][]string, numRows+1)
	data[0] = template.headers

	for i := 1; i <= numRows; i++ {
		data[i] = template.genRow()
	}

	return data
}

func saveCSVToDisk(filename string, data [][]string) error {
	file, err := os.Create(filename)
	if err != nil {
		return err
	}
	defer func(file *os.File) {
		err := file.Close()
		if err != nil {
			fmt.Printf("fail to file close error: %v", err)
		}
	}(file)

	writer := csv.NewWriter(file)
	defer writer.Flush()

	for _, row := range data {
		if err := writer.Write(row); err != nil {
			return err
		}
	}
	return nil
}

func listFilesHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/html")
	w.WriteHeader(http.StatusOK)

	fmt.Fprintln(w, "<html><body><h1>Available CSV Files</h1><ul>")
	for key := range csvTemplates {
		fmt.Fprintf(w, "<li><a href=\"/csv/%s\">%s.csv</a> (returns 10-200 random records)</li>\n", key, key)
	}
	fmt.Fprintln(w, "</ul></body></html>")
}

func csvHandler(w http.ResponseWriter, r *http.Request) {
	pathParts := strings.Split(r.URL.Path, "/")
	if len(pathParts) < 3 {
		http.Error(w, "Invalid path", http.StatusBadRequest)
		return
	}

	fileKey := pathParts[2]
	template, exists := csvTemplates[fileKey]
	if !exists {
		http.Error(w, "CSV template not found", http.StatusNotFound)
		return
	}

	csvData := generateRandomCSV(template, 10, 200)

	log.Printf("Generated %s with %d records", template.filename, len(csvData)-1)

	w.Header().Set("Content-Type", "text/csv")
	w.Header().Set("Content-Disposition", fmt.Sprintf("attachment; filename=\"%s\"", template.filename))

	writer := csv.NewWriter(w)
	defer writer.Flush()

	for _, row := range csvData {
		if err := writer.Write(row); err != nil {
			http.Error(w, "Error writing CSV", http.StatusInternalServerError)
			return
		}
	}
}

func healthHandler(w http.ResponseWriter, r *http.Request) {
	w.WriteHeader(http.StatusOK)
	fmt.Fprintln(w, "Server is running")
}

func main() {
	http.HandleFunc("/", listFilesHandler)
	http.HandleFunc("/health", healthHandler)
	http.HandleFunc("/csv/", csvHandler)

	port := 8080
	log.Printf("Starting random CSV mock server on port %d...", port)
	log.Printf("Visit http://localhost:%d/ to see available files", port)
	log.Printf("Each request will return between 10-200 random records")
	if err := http.ListenAndServe(fmt.Sprintf(":%d", port), nil); err != nil {
		log.Fatalf("Server failed to start: %v", err)
	}
}
