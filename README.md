# Advanced Big Data Analytics

## Assignment: Restaurant Discovery & Analytics System

## Scenario

Develop a **Scala-based Restaurant Discovery & Analytics System** using **MongoDB**.

Use MongoDB's official **sample_restaurants** dataset as the initial data source:

- **Database:** `sample_restaurants`
- **Collection:** `restaurants`

The application should be implemented primarily as a **CLI application**. An optional GUI/web application may also be developed using Scala as the backend.

## Requirements

### Scala

The application should demonstrate:

- Variables, data types, conditions and functions
- Scala collections and operations such as `map`, `filter`, `groupBy`, etc.
- Option and/or pattern matching
- Exception/error handling
- Classes or case classes, objects and methods
- Encapsulation
- At least one of trait, inheritance or composition

### MongoDB CRUD

Implement the following through the Scala application:

- **Create:** Add at least 2 new restaurant records
- **Read:** View/search restaurant records
- **Update:** Modify at least 1 existing restaurant
- **Delete:** Remove at least 1 restaurant

### Search & Filtering

Implement at least **3 different** search/filter operations, such as:

- Restaurant name
- Cuisine
- Borough
- ZIP code
- Score

### Indexing

Create and use:

- At least **2 indexes**
- At least **1 compound index**

Indexes should be relevant to the queries implemented.

### Aggregation

Implement at least **3 meaningful MongoDB aggregation queries**, such as:

- Restaurants by cuisine
- Restaurants by borough
- Average score by cuisine
- Top-rated restaurants
- Grade distribution

### Application Structure

The application should provide a simple menu-driven interface, for example:

```
1. Add Restaurant
2. Search/View Restaurants
3. Update Restaurant
4. Delete Restaurant
5. Restaurant Analytics
6. Index Information
7. Exit
```

The exact design and structure may be decided (without shortening) by you.
