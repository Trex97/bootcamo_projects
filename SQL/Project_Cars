CREATE TABLE cars (
	id_car INT PRIMARY KEY  , 
	model__names TEXT,
	price NUMERIC
)

INSERT INTO cars VALUES
	(1 , 'Alphard' , 400000) ,
	(2 , 'Altis' ,879000) ,
	(3 , 'Yaris' ,549000) ,
	(4 , 'CH-R' ,1100000) ,
	(5 , 'Camry' ,1450000) ,
	(6 , 'Commuter' ,1300000) ,
	(7 , 'Cross' ,1000000) ,
	(8 , 'Avanza' ,699000) ,
	(9 , 'Vios' ,600000) ,
	(10 , 'Revo' ,1200000) ;
	
SELECT * FROM cars; 


CREATE TABLE saller (
	id_saller INT PRIMARY KEY  , 
	name_saller TEXT , 
	commission NUMERIC
)

INSERT INTO saller VALUES 
	(1 , 'Au'   , 8 ) ,
	(2 , 'Bank' , 7 ) ,
	(3 , 'Tunk' , 5 ) ,
	(4 , 'Ton'  , 5 ) ,
	(5 , 'Tor'  , 2 ) ;
	
SELECT * FROM saller ; 

CREATE TABLE customers (
	id_customers INT PRIMARY KEY  ,
	name_customers TEXT , 
	age NUMERIC ,
	sex TEXT 
)

INSERT INTO customers VALUES 
	(1  , 'Mary'    , 25 , 'Female') ,
	(2  , 'Jane'    , 52 , 'Female') ,
	(3  , 'Tom'    , 46 , 'male') ,
	(4  , 'Hang'    , 32 , 'male') ,
	(5  , 'Robert'  , 23 , 'male') ,
	(6  , 'Downey' , 42 , 'male') ,
	(7  , 'Junior' ,   24, 'Female') ,
	(8  , 'Steve' , 43 , 'male') ,
	(9  , 'Roger' , 35 , 'male') ,
	(10 , 'Thor' , 53 , 'male') ;
	
SELECT * FROM customers;

CREATE TABLE bank (
 id_bank INT PRIMARY KEY  ,
 name_bank TEXT
)

INSERT INTO bank VALUES 
  (1,'SCB') ,
  (2,'KBANK') ,
  (3,'NEXT') ,
  (4,'Nemo') ;
  
SELECT * FROM bank ;

CREATE TABLE sales (
	id_sale INT PRIMARY KEY UNIQUE , 
	date_sale DATE ,
	id_cars INT ,
	id_customers INT ,
	id_saller INT , 
	id_bank INT ,
	payment_method TEXT ,
	number_month NUMERIC ,
	FOREIGN KEY (id_cars) REFERENCES cars (id_car)
	FOREIGN KEY (id_customers) REFERENCES customers (id_customers)
	FOREIGN KEY (id_saller) REFERENCES saller (id_saller)
	FOREIGN KEY (id_bank) REFERENCES bank (id_bank)
)


INSERT INTO sales VALUES 
	(1  , '09-01-2021'  ,  1 , 10 , 1 , 1 , 'cash' , 0 ) ,
	(2  , '02-03-2020'  ,  5 , 9 , 4 , 3 , 'installment' , 60 ) ,
	(3  , '03-07-2019'  ,  5 , 1 , 2 , 2 , 'installment', 72) ,
	(4  , '03-08-2022'  ,  6 , 2 , 5 , 4 , 'installment', 60) ,
	(5  , '06-05-2022'  ,  8 , 8 , 5 , 2 , 'cash' , 0 ) ,
	(6  , '06-05-2022'  ,  9 , 5 , 2 , 3 , 'installment', 84) ,
	(7  , '09-06-2021'  , 10 , 7 ,1 , 4 , 'installment' , 48 ) ,
	(8  , '14-02-2023'  ,  2 , 3 , 5 , 2 , 'installment' , 60 ) ,
	(9  , '09-02-2021'  ,  3 , 4 , 2 , 1 , 'installment' , 48 ) ,
	(10 , '03-09-2021'  ,  4 , 6 , 3 , 2 , 'cash' , 0 ) ;
	
SELECT * FROM sales ; 

-- Q1 
WITH sub1 AS (
SELECT * 
FROM sales JOIN cars 
ON sales.id_cars = cars.id_car 
JOIN saller 
ON saller.id_saller = sales.id_saller 
) ,sub2 AS (
SELECT 
	name_saller ,
	count(*) AS num_sale,
	sum(price) AS total_sale, 
	commission/100.0 AS commission
FROM sub1
GROUP BY name_saller 
)

SELECT 
	name_saller , 
	total_sale , 
	total_sale*commission AS salary_saller
FROM sub2 ; 

-- Q2 
WITH sub1 AS (
SELECT 
	* ,
	substr(date_sale,1,2)  AS a, 
	substr(date_sale,4,2)  AS b,
	substr(date_sale,7,4)  AS c
FROM sales JOIN cars 
ON sales.id_cars = cars.id_car 
) , sub2 AS (
SELECT 
	* ,
	c  || '-' || b || '-' || a AS new_date
FROM sub1 
) , sub3 AS (
SELECT 
	*,
	STRFTIME ('%d',new_date) AS day ,
	STRFTIME ('%m',new_date) AS month ,
	STRFTIME ('%Y',new_date) AS year ,
	count(model__names) AS num_cars ,
	sum(price) AS total_income 
FROM sub2 
GROUP BY year 
) 
SELECT 
	year,
	num_cars,
	total_income 
FROM sub3 
GROUP BY year 
ORDER BY year  ;


-- Q3 
WITH sub1 AS (
SELECT 
	* ,
	CASE
		WHEN number_month == 0 THEN price
		ELSE price
	END 
FROM sales JOIN cars
ON sales.id_cars = cars.id_car
JOIN customers 
ON sales.id_customers = customers.id_customers
) , sub2 AS (
SELECT 
	* ,
	price/number_month AS installment_month 
FROM sub1 
) , sub3 AS (
SELECT 
	* ,
	CASE
		WHEN installment_month IS NULL THEN '0'
		ELSE installment_month
	END AS installment_month2 
FROM sub2 
) 
SELECT 
	id_sale ,
	name_customers,
	sex,
	age,
	date_sale,
	model__names,
	price,
	installment_month2 
FROM sub3 ;
