#lang scheme
(define customer_table
  '((Customer Age Location)
    ("John Smith" 35 "New York")
    ("Alice Johnson" 28 "Los Angeles")
    ("Michael Brown" 45 "Miami")
    ("Emily Davis" 32 "Houston")
    ("Robert Wilson" 40 "Miami")
    ("Sophia Martinez" 30 "New York")
    ("William Taylor" 38 "Houston")
    ("Emma White" 25 "Los Angeles")
    ("James Harris" 32 "Houston")
    ("Olivia Clark" 29 "Los Angeles")))

(define item_table
  '((Item Category Price)
    ("Apples" "Fruits" 2.30)
    ("Coffee" "Beverages" 3.50)
    ("Bread" "Bakery" 2.00)
    ("Milk" "Dairy" 3.50)
    ("Bananas" "Fruits" 1.75)
    ("Eggs" "Dairy" 4.75)
    ("Orange Juice" "Beverages" 3.25)
    ("Tea" "Beverages" 2.75)
    ("Fish" "Seafood" 12.50)
    ("Broccoli" "Vegetables" 1.80)
    ("Orange" "Fruits" 1.25)
    ("Chicken" "Meat" 7.00)
    ("Lettuce" "Vegetables" 1.20)
    ("Pasta" "Pantry" 3.75)
    ("Salmon" "Seafood" 9.50)
    ("Yogurt" "Dairy" 2.75)
    ("Bacon" "Meat" 6.25)
    ("Cheese" "Dairy" 5.50)
    ("Beef" "Meat" 8.00)
    ("Potatoes" "Vegetables" 2.50)
    ("Chicken Soup" "Canned Goods" 3.50)
    ("Rice" "Grains" 2.25)
    ("Carrots" "Vegetables" 1.10)
    ("Spinach" "Vegetables" 1.60)
    ("Tomatoes" "Vegetables" 1.50)
    ("Apple Juice" "Beverages" 3.40)
    ("Onions" "Vegetables" 1.20)))

(define mix_table
  '((Customer Items Date)
    ("John Smith" ("Apples" "Coffee" "Bread") "15.03.2024")
    ("John Smith" ("Milk" "Bananas") "22.03.2024")
    ("John Smith" ("Eggs" "Orange Juice") "29.03.2024")
    ("John Smith" ("Tea" "Fish" "Broccoli" "Orange") "5.04.2024")
    ("John Smith" ("Chicken" "Lettuce" "Pasta" "Salmon") "12.04.2024")
    ("Alice Johnson" ("Milk" "Bananas") "20.03.2024")
    ("Michael Brown" ("Orange Juice" "Yogurt") "24.03.2024")
    ("Michael Brown" ("Bacon") "28.03.2024")
    ("Michael Brown" ("Coffee" "Bread" "Apples") "2.04.2024")
    ("Michael Brown" ("Milk" "Bananas" "Eggs") "5.04.2024")
    ("Michael Brown" ("Cheese" "Beef" "Potatoes" "Chicken Soup") "10.04.2024")
    ("Emily Davis" ("Chicken" "Lettuce") "24.03.2024")
    ("Emily Davis" ("Pasta" "Salmon" "Rice" "Potatoes") "28.03.2024")
    ("Emily Davis" ("Carrots" "Spinach") "1.04.2024")
    ("Robert Wilson" ("Salmon" "Rice") "21.03.2024")
    ("Robert Wilson" ("Potatoes" "Chicken" "Lettuce" "Pasta") "25.03.2024")
    ("Robert Wilson" ("Milk" "Bananas" "Eggs" "Orange Juice") "29.03.2024")
    ("Robert Wilson" ("Bacon") "2.04.2024")
    ("Robert Wilson" ("Fish" "Broccoli") "6.04.2024")
    ("Sophia Martinez" ("Carrots" "Spinach") "26.03.2024")
    ("Sophia Martinez" ("Tea" "Fish" "Broccoli" "Orange") "30.03.2024")
    ("William Taylor" ("Beef" "Potatoes") "19.03.2024")
    ("William Taylor" ("Chicken Soup" "Tomatoes" "Apple Juice" "Bread") "23.03.2024")
    ("Emma White" ("Tomatoes" "Chicken Soup") "23.03.2024")
    ("Emma White" ("Milk" "Salmon" "Rice" "Potatoes") "27.03.2024")
    ("Emma White" ("Chicken" "Lettuce" "Pasta" "Salmon") "31.03.2024")
    ("James Harris" ("Onions" "Apple Juice") "25.03.2024")
    ("James Harris" ("Cheese" "Beef" "Potatoes" "Chicken Soup") "29.03.2024")
    ("Olivia Clark" ("Fish" "Broccoli") "25.03.2024" )
    ("Olivia Clark" ("Orange" "Chicken" "Lettuce" "Pasta") "29.03.2024")
    
    ))

;1st function
(define (retrieve-unique-items)
  (map car (cdr item_table)))

;;2nd function
(define (retrieve-unique-categories)
(define (delete-same-elements lst) ; helper function that gives every category once
  (cond ((null? lst)'()) 
        ((member (car lst) (cdr lst)) 
         (delete-same-elements (cdr lst)))
        (else (cons (car lst) (delete-same-elements (cdr lst))))))

  (delete-same-elements(map cadr (cdr item_table))))

;3rd function
(define (retrieve-items-by-category category)
  (define (function3 items result) ; helper function to retrieve items by category
    (if (null? items)
        result
    (let ((current-item (car items)))
    (if (eq? (cadr current-item) category)
        (function3 (cdr items) (cons (car current-item) result))
        (function3 (cdr items) result)))))
  (function3 (cdr item_table) '()))

;4th function
(define (retrieve-customerdata-by-name customer)
  (define (function4 customers result) ; helper function to retrieve customer data
    (if (null? customers)
        result
        (let ((current-customer (car customers)))
          (if (eq? (car current-customer) customer)
              (function4 (cdr customers) (append current-customer result))
              (function4 (cdr customers) result)))))
  (function4 (cdr customer_table) '()))

;5th function

(define (retrieve-itemdata-by-name item)
  (define (function5 allitems result) ; helper function to retrieve item data
    (if (null? allitems)
        result
        (let ((current-item (car allitems)))
          (if (eq? (car current-item) item)
              (function5 (cdr allitems) (append current-item result))
              (function5 (cdr allitems) result)))))
  (function5 (cdr item_table) '()))


;6th function
(define (most-expensive-item)
  (define (function6 allitems max-price max-item) ; helper function to find the most expensive item
    (if (null? allitems)
        max-item
        (let ((current-item (car allitems))
              (current-price (caddr (car allitems)))) 
          (if (>= current-price max-price)
              (function6 (cdr allitems) current-price current-item) 
              (function6 (cdr allitems) max-price max-item)))))
  (function6 (cdr item_table) 0 '()))  

;7th function
(define (most-cheap-item)
  (define (function7 allitems min-price min-item) ;helper function to find the cheapest item
    (if (null? allitems)
        min-item
        (let ((current-item (car allitems))
              (current-price (caddr (car allitems)))) ; getting the price column
          (if (<= current-price min-price)
              (function7 (cdr allitems) current-price current-item) 
              (function7 (cdr allitems) min-price min-item)))))
  (function7 (cdr item_table) 100 '()))  


;8th function

(define (total-cost-of-items customer)
  (define (calculate-total-cost items)
    (if (null? items)
        0
        (+ (item-price (car items))    ;item-price is helper method defined below
           (calculate-total-cost (cdr items)))))

  (let ((customer-items (items-bought-by-customer customer)))
    (calculate-total-cost customer-items)))  


;9th function
(define (items-bought-by-customer customer)
   (define (search-items customer rows items) ; helper function that searches for the items bought by customer with given name
    (if (null? rows)
        items
        (if (eq? (car (car rows)) customer)
            (search-items customer 
                          (cdr rows) 
                          (append items (cadr (car rows))))
            (search-items customer (cdr rows) items))))
  (search-items customer mix_table '()))


;10th function
(define (total-cost-of-transactions)
  (define (calculate-transaction-cost items)
    (if (null? items)
        0
        (+ (item-price (car items)) ; helper function defined below
           (calculate-transaction-cost (cdr items)))))

  (define (process-transactions transactions total-cost) ; calculates total cost of transactions
    (if (null? transactions)
        total-cost
        (process-transactions (cdr transactions)
                              (+ total-cost
                                 (calculate-transaction-cost (cadr (car transactions)))))))
  (process-transactions (cdr mix_table) 0))


; 11th function
(define (items-purchased-on-specific-date date table)
  (define (filter-transactions transactions date result) ; this function filters the transactions with the same dates
    (cond ((null? transactions) result)
          ((eq? (caddr (car transactions)) date)
           (filter-transactions (cdr transactions) date
                                (append result (cadr (car transactions)))))
          (else
           (filter-transactions (cdr transactions) date result))))
  (filter-transactions table date '()))




;12th function
(define (total-revenue-by-category)
  (define (calculate-category-revenue category) ; calculates total cost of each category
    (define (calculate-total-items-price allitems) 
      (if (null? allitems)
          0
          (+ (item-price (car allitems))
             (calculate-total-items-price (cdr allitems)))))
    (define (total-price-of-items items)
      (calculate-total-items-price items))
    (total-price-of-items (retrieve-items-by-category category)))

  (define (print-category-revenue category) ; displays each category with total prices
    (display category)
    (display ": ")
    (display (calculate-category-revenue category))
    (newline))

  (define (process-categories categories)
    (cond ((null? categories) " ")
          (else
           (print-category-revenue (car categories))
           (process-categories (cdr categories)))))

  (process-categories (retrieve-unique-categories)))



;13th function
(define (most-popular-category mix_table)
  (define (count-category category mix_table)
    (length (retrieve-items-by-category category)))
  
  (define (function13 categories max-category max-count) ; helper to find the most popular category
    (if (null? categories)
        max-category
        (let ((current-category (car categories))
              (current-count (count-category (car categories) mix_table)))
          (if (> current-count max-count)
              (function13 (cdr categories) current-category current-count)
              (function13 (cdr categories) max-category max-count)))))
  
  (define unique-categories (retrieve-unique-categories))
  
  (if (null? unique-categories)
      "No categories found"
      (function13 (cdr unique-categories) (car unique-categories) 0)))




;14th function
(define (items-by-age-group min-age max-age)
  (define (customer-in-age-range? customer)
    (let ((age (lookup-age customer)))
      (and age (>= age min-age) (<= age max-age))))
  
  (define (lookup-age customer)
    (define (function14 mix_table) ;helper function to find the age of given customer
      (cond ((null? mix_table) '())
            ((eq? customer (caar mix_table)) (cadar mix_table))
            (else (function14 (cdr mix_table)))))
    (function14 (cdr customer_table)))

  (define (collect-items customers)
    (cond ((null? customers) '())
          (else (append (items-bought-by-customer (car customers))
                        (collect-items (cdr customers))))))

  (let ((customers-in-range (filter customer-in-age-range? (map car (cdr customer_table)))))
    (collect-items customers-in-range))) 


;15th function
(define (average-per-transaction-by-location city)

  (define (count-transactions-for-city city table) ; helper function to get the total number of transactions for cities
    (length (filter (lambda (transaction)
                      (let ((customer (car transaction)))
                        (eq? city (find-customer-city customer))))
                    table)))
  
  (define (find-customer-city customer)  
    (define (function15 customer table) ; helper function to find customer's city
      (cond ((null? table) '())
            ((eq? customer (caar table)) (caddr (car table)))
            (else (function15 customer (cdr table)))))
    (function15 customer customer_table))
  

  (define (filter-customers-by-city customers) ;filters customers with same city
    (filter (lambda (customer)
              (eq? (find-customer-city (car customer)) city))
            customers))
  
  (define (all-items-bought-by-customers customers) 
    (define (collect-items customer-list items)
      (if (null? customer-list)
          items
          (collect-items (cdr customer-list)
                         (append items (items-bought-by-customer (car customer-list))))))
    (collect-items (map car customers) '()))
  
  (define (total-price-of-items items) ; gets the price of items bought by customers with same city
    (define (sum-prices items total)
      (if (null? items)
          total
          (sum-prices (cdr items) (+ total (item-price (car items)))))) ; helper function that is defined below

    (sum-prices items 0))

  (let ((customers (filter-customers-by-city (cdr customer_table))))
    (let ((items (all-items-bought-by-customers customers)))
      (let ((total-price (total-price-of-items items)))
        (let ((num-transactions (count-transactions-for-city city mix_table)))
          (if (= num-transactions 0)
              "No transactions"
              (/ total-price num-transactions)))))))



; Helper function for many functions
(define (item-price item-name)
  (let ((item-data (retrieve-itemdata-by-name item-name)))
    (if (null? item-data)
        0
        (caddr item-data))))


;displaying functions
(display "Function 1:")
(newline)
(display (retrieve-unique-items))
(newline)
(display "Function 2:")
(newline)
(display (retrieve-unique-categories))
(newline)
(display "Function 3:")
(newline)
(display (retrieve-items-by-category "Canned Goods"))
(newline)
(display "Function 4:")
(newline)
(display (retrieve-customerdata-by-name "John Smith"))
(newline)
(display "Function 5:")
(newline)
(display (retrieve-itemdata-by-name "Yogurt"))
(newline)
(display "Function 6:")
(newline)
(display (most-expensive-item))
(newline)
(display "Function 7:")
(newline)
(display (most-cheap-item))
(newline)
(display "Function 8:")
(newline)
(display(total-cost-of-items "James Harris"))
(newline)
(display "Function 9:")
(newline)
(display (items-bought-by-customer "Michael Brown"))
(newline)
(display "Function 10:")
(newline)
(display(total-cost-of-transactions))
(newline)
(display "Function 11:")
(newline)
(display(items-purchased-on-specific-date "23.03.2024" mix_table))
(newline)
(display "Function 12:")
(newline)
(display (total-revenue-by-category))
(newline)
(display "Function 13:")
(newline)
(display "Most popular category: ")
(display (most-popular-category mix_table))
(newline)
(display "Function 14:")
(newline)
(display (items-by-age-group 20 30))
(newline)
(display "Function 15:")
(newline)
(display "Miami: ")
(display (average-per-transaction-by-location "Miami"))
(newline)
(display "Houston: ")
(display (average-per-transaction-by-location "Houston"))
(newline)
(display "New York: ")
(display (average-per-transaction-by-location "New York"))
(newline)
(display "Los Angeles: ")
(display (average-per-transaction-by-location "Los Angeles"))