-- MBIS623-20S1 - Assignment 2 - Data Integration and Cleansing
-- UC Student: Phuong Tang -- ID: 23186540

-- In this assignment, we are required to create a suitable data model for the integrated dataset, and write the extracttransform-load (ETL) SQL statements for Stronger Christchurch Infrastructure Rebuild Team (SCIRT) repair jobs
-- We divided our assignment into 4 parts:

--  * Part 0: Overview about 2 original datasets
--  * Part 1: Clean data. It includes checking duplicates, removing duplicates, checking Null values, removing Null values (if having), checking empty strings
--  * Part 2: Finding the discrepancies by comparing 2 original tables. This lead to the requirement of table decomposition in part 4
--  * Part 3: Table decomposition. Here, we try to deploy First normal form (1NF), second normal form (2NF), third normal form (3NF) technique to normalize the data. Some steps at this stage: building ER diagram, using forward engineering to create new tables, load the data from 2 original datasets

-- Important note 1: New schema named "assign2". Please choose this name when using Reverse Engineering.
-- Important note 2: I already tested my script by File--> Open SQL Script --> Execute All Queries. Then run Database --> Reverse Engineer and it worked. 
-- Important note 3: The back and forth checking steps are used in my work to ensure the data quality preserved during each transformation step.

############################################ Part 0: Overview about dataset ######################################
select * from `scirt_jobs_bound`.chch_street_address;
select * from `scirt_jobs_bound`.scirt_job;
CALL `scirt_jobs_bound`.split_column();   # Be careful, if we call this stored procedure many time, we will have a temp_table with many duplicated rows
select * from `scirt_jobs_bound`.temp_table order by job_id;

################################################ Part 1: Clean data ######################################
# (1) Check duplication in chch_street_address  
SELECT unit_value, address_number, address_number_suffix, suburb_locality, town_city, road_name , COUNT(*) as NumDuplicates
FROM `scirt_jobs_bound`.chch_street_address
GROUP BY unit_value, address_number, address_number_suffix, suburb_locality, town_city, road_name
HAVING NumDuplicates > 1;
-- the result showed that we have 1 duplicate at address 156 Lower Styx Road, Bottle Lake, Christchurh, roa_section 302674

# (2) Remove the duplicate data in chch_street_address using intermediate table

SET sql_mode=(SELECT REPLACE(@@sql_mode,'ONLY_FULL_GROUP_BY',''));
-- step 1
CREATE TABLE `scirt_jobs_bound`.chch_street_address_temp 
LIKE `scirt_jobs_bound`.chch_street_address;

-- step 2
INSERT INTO `scirt_jobs_bound`.chch_street_address_temp
SELECT * 
FROM `scirt_jobs_bound`.chch_street_address 
GROUP BY unit_value, address_number, address_number_suffix, suburb_locality, road_name, road_section_id;

-- step 3
DROP TABLE `scirt_jobs_bound`.chch_street_address;

ALTER TABLE `scirt_jobs_bound`.chch_street_address_temp 
RENAME TO `scirt_jobs_bound`.chch_street_address;

# Finally, check the number of rows in chch_street_address table again
select count(*) from `scirt_jobs_bound`.chch_street_address; -- result: 161428 , 1 duplicated row has been cleaned up

# (3) Checking duplicates in scirt_job table
# check the number of rows in scirt_job table
select count(*) from `scirt_jobs_bound`.scirt_job;  # 341 rows

# checking duplicates for the whole scirt_job table
SELECT * , COUNT(*)
FROM `scirt_jobs_bound`.scirt_job
GROUP BY description, routes, locality, delivery_team, start_date, end_date
HAVING COUNT(*) > 1;
-- result: no duplicate

# (4) Checking duplicates in temp_table
SELECT * , COUNT(*)
FROM `scirt_jobs_bound`.temp_table
GROUP BY job_id, route
HAVING COUNT(*) > 1;
-- result: many duplicates

# Get 1 instance from above query to check why it happened. Is it true?
SELECT * FROM `scirt_jobs_bound`.temp_table where job_id = 10914 and route = "Marshland Road";
Select * from `scirt_jobs_bound`.scirt_job where job_id = 10914; -- result: reveal the reason: because of duplicates in typing in the field "routes" of scirt_job

# row counting before delete duplicate
select count(*) from `scirt_jobs_bound`.temp_table;  # 6058 rows

# Delete duplicate rows for temp_table
-- step 1
CREATE TABLE `scirt_jobs_bound`.temp_table1 
LIKE `scirt_jobs_bound`.temp_table;

-- step 2
INSERT INTO `scirt_jobs_bound`.temp_table1
SELECT * 
FROM `scirt_jobs_bound`.temp_table 
GROUP BY job_id, route;

-- step 3
DROP TABLE `scirt_jobs_bound`.temp_table;

ALTER TABLE `scirt_jobs_bound`.temp_table1 
RENAME TO `scirt_jobs_bound`.temp_table;

# row counting after delete duplicate
select count(*) from `scirt_jobs_bound`.temp_table;  #  5931 rows - which means 6058-5931=127 duplicates have been removed

# (5) Checking for null values
SELECT * FROM `scirt_jobs_bound`.chch_street_address
WHERE address_number IS NULL OR road_name IS NULL OR suburb_locality IS NULL; 
-- result: no Null data

SELECT * FROM `scirt_jobs_bound`.scirt_job
WHERE routes IS NULL OR locality IS NULL OR delivery_team IS NULL OR start_date IS NULL OR end_date IS NULL;
-- result: no Null data

# (6) Checking empty string values
SELECT * FROM `scirt_jobs_bound`.chch_street_address
WHERE address_number = "" OR road_name = "" OR suburb_locality = ""; 
-- Result: There are 46 rows in chch_street_address that have no suburb_locality which is related to Taylors Mistake Bay


###################################### Part 2: finding the discrepancy #####################################
# Compare two tables chch_street_address and scirt_job

# First, use the UNION statement to combine rows in both tables
SELECT chch_street_address.road_name, chch_street_address.suburb_locality 
FROM `scirt_jobs_bound`.chch_street_address
UNION ALL
SELECT route, locality FROM
(select temp_table.job_id, temp_table.route, scirt_job.locality
from `scirt_jobs_bound`.temp_table join `scirt_jobs_bound`.scirt_job on temp_table.job_id = scirt_job.job_id) temp;

#Second, group the records based on the primary key and columns that need to compare. If the values in the columns that need to compare are identical, the COUNT(*) returns 2, otherwise the COUNT(*) returns 1
SELECT road_name, suburb_locality
FROM
 (
SELECT chch_street_address.road_name, chch_street_address.suburb_locality FROM `scirt_jobs_bound`.chch_street_address
UNION ALL
SELECT route, locality FROM
							(select temp_table.job_id, temp_table.route, scirt_job.locality
							 from `scirt_jobs_bound`.temp_table join `scirt_jobs_bound`.scirt_job on temp_table.job_id = scirt_job.job_id) temp
)  temp2
GROUP BY road_name, suburb_locality
HAVING COUNT(*) = 1
ORDER BY road_name;
-- result: there are 49 discrepancies between 2 tables

# Get 1 instance from above discrepancy record to check the reason of these discrepancies before finding the way to fix it.
select * from `scirt_jobs_bound`.chch_street_address where road_name = "Aidanfield Drive";
select * from `scirt_jobs_bound`.scirt_job where routes like "%Aidanfield Drive%";

-- The result shows that Aidanfield Drive belong to Aidanfield and Wigram locality in "chch_street_address" table
-- while in the "scirt_job" table, Aidanfield Drive is assigned to Halswell locality
-- To fix it, we need to use associative table to decompose address table to eliminate these discrepancy.

############################## Part 3: Table decomposition ##################################

-- MySQL Workbench Forward Engineering

SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='STRICT_TRANS_TABLES,ERROR_FOR_DIVISION_BY_ZERO,NO_ENGINE_SUBSTITUTION';

-- -----------------------------------------------------
-- Schema Assign2
-- -----------------------------------------------------

-- -----------------------------------------------------
-- Schema Assign2
-- -----------------------------------------------------
CREATE SCHEMA IF NOT EXISTS `Assign2` DEFAULT CHARACTER SET utf8 ;
USE `Assign2` ;

-- -----------------------------------------------------
-- Table `Assign2`.`Delivery_team`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `Assign2`.`Delivery_team` (
  `delivery_team_id` INT NOT NULL AUTO_INCREMENT,
  `delivery_team` VARCHAR(100) NOT NULL,
  PRIMARY KEY (`delivery_team_id`))
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Assign2`.`job`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `Assign2`.`job` (
  `job_id` INT NOT NULL,
  `description` VARCHAR(400) NOT NULL,
  `start_date` DATE NOT NULL,
  `end_date` DATE NOT NULL,
  `delivery_team_id` INT NOT NULL,
  PRIMARY KEY (`job_id`),
  INDEX `delivery_team_fk_idx` (`delivery_team_id` ASC),
  CONSTRAINT `delivery_team_fk`
    FOREIGN KEY (`delivery_team_id`)
    REFERENCES `Assign2`.`Delivery_team` (`delivery_team_id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Assign2`.`Locality`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `Assign2`.`Locality` (
  `locality_id` INT NOT NULL AUTO_INCREMENT,
  `locality` VARCHAR(40) NOT NULL,
  PRIMARY KEY (`locality_id`))
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Assign2`.`Route`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `Assign2`.`Route` (
  `route_id` INT NOT NULL AUTO_INCREMENT,
  `route` VARCHAR(40) NOT NULL,
  `locality_id` INT NOT NULL,
  PRIMARY KEY (`route_id`),
  INDEX `locality_fk_idx` (`locality_id` ASC),
  CONSTRAINT `locality_fk`
    FOREIGN KEY (`locality_id`)
    REFERENCES `Assign2`.`Locality` (`locality_id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Assign2`.`Single_route_job`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `Assign2`.`Single_route_job` (
  `Single_route_job_id` INT NOT NULL AUTO_INCREMENT,
  `job_id` INT NOT NULL,
  `route` VARCHAR(40) NOT NULL,
  `route_id` INT NOT NULL,
  PRIMARY KEY (`Single_route_job_id`),
  INDEX `route_id_fk_idx` (`route_id` ASC),
  CONSTRAINT `job_id_fk`
    FOREIGN KEY (`job_id`)
    REFERENCES `Assign2`.`job` (`job_id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `route_id_fk`
    FOREIGN KEY (`route_id`)
    REFERENCES `Assign2`.`Route` (`route_id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Assign2`.`Address`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `Assign2`.`Address` (
  `address_id` INT NOT NULL,
  `unit_value` VARCHAR(20) NOT NULL,
  `address_number` INT NOT NULL,
  `address_number_suffix` VARCHAR(5) NOT NULL,
  `address_number_high` VARCHAR(5) NOT NULL,
  `route_id` INT NOT NULL,
  PRIMARY KEY (`address_id`),
  INDEX `route_id_fk_idx` (`route_id` ASC),
  CONSTRAINT `routeid_fk`
    FOREIGN KEY (`route_id`)
    REFERENCES `Assign2`.`Route` (`route_id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;

--
-- Dumping data for table `Assign2`.`Locality`
--
INSERT INTO `Assign2`.`Locality` (`locality`) 
SELECT distinct `suburb_locality` FROM `scirt_jobs_bound`.`chch_street_address`;

# check data quality after importing
select * from `Assign2`.`Locality`;
select count(*) from `Assign2`.`Locality`; # 88 localities 

--
-- Dumping data for table `Assign2`.`Delivery_team`
--
INSERT INTO `Assign2`.`Delivery_team`(`delivery_team`) 
SELECT distinct delivery_team FROM `scirt_jobs_bound`.`scirt_job`;

# check data quality after importing
select * from `Assign2`.`Delivery_team`;
select count(*) from `Assign2`.`Delivery_team`; # 7 distinct delivery teams
--
-- Dumping data for table `Assign2`.`Route`
--
INSERT INTO `Assign2`.`Route` (`route`, `locality_id`) 
SELECT distinct `road_name`, `locality_id` 
FROM `scirt_jobs_bound`.`chch_street_address` JOIN `Assign2`.`Locality` 
ON chch_street_address.suburb_locality = `Assign2`.`Locality`.locality;

# check data quality after importing
select * from `Assign2`.`Route`;
select count(*) from `Assign2`.`Route`; -- result: 4389 rows. 1 road can be in more than 1 locality

# Is above result mapped with data from chch_street_address?
SELECT count(distinct road_name, suburb_locality)   FROM `scirt_jobs_bound`.`chch_street_address`;
-- result: 4389. This means the "Route" table retrieved correct data

--
-- Dumping data for table `Assign2`.`job`
--
INSERT INTO `Assign2`.`job`(`job_id`, `description`, `start_date`, `end_date`, `delivery_team_id`) 
SELECT s.`job_id`, `description`, s.`start_date`, s.`end_date`, `delivery_team_id`
FROM `scirt_jobs_bound`.`scirt_job` s  JOIN `Assign2`.`Delivery_team` d On s.`delivery_team` = d.`delivery_team`;

# check data quality after importing
select * from `Assign2`.`job` order by job_id;
select count(*) from `Assign2`.`job`;  -- result: 341 jobs

--
-- Dumping data for table `Assign2`.`Single_route_job`
--
INSERT INTO `Assign2`.`Single_route_job` (`job_id`, `route`, `route_id`) 
SELECT job_id, t.route, route_id
FROM `scirt_jobs_bound`.`temp_table` t JOIN `Assign2`.`Route` r ON t.route = r.route ;

select * from `Assign2`.`Single_route_job` order by job_id;
select count(*) from `scirt_jobs_bound`.`temp_table`; -- result: 5931
select count(*) from `Assign2`.`Single_route_job`; -- result: 7441 single_route_job

#verify why the row numbers of single_route_job is bigger than original temp_table? Is it make sense?
select * from `scirt_jobs_bound`.`temp_table` where job_id = 10310; -- this job id only have 2 routes Milton Street and Frankleigh Street
select * from `Assign2`.`Route` where route = "Milton Street" OR route = 'Frankleigh Street'; 
-- result:  Milton Street belong to 3 localities and Frankleigh belong to 1 locality. 
-- Therefore, 1 job - 2 routes- 4 localities --> the row numbers of single_route_job is bigger than original temp_table is make sense as some route hve more than 1 locality

--
-- Dumping data for table `Assign2`.`Address`
--
INSERT INTO `Assign2`.`Address` (`address_id`, `unit_value`, `address_number`, `address_number_suffix`, `address_number_high`, `route_id`) 
SELECT address_id, unit_value, address_number, address_number_suffix, address_number_high, route_id
FROM `scirt_jobs_bound`.`chch_street_address` c JOIN `Assign2`.`Locality` l 
ON c.suburb_locality = l.locality JOIN `Assign2`.`Route` r
ON l.locality_id = r.locality_id AND c.road_name = r.route;

select count(*) from `Assign2`.`Address`; -- result: 161428, correct mapping data



SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;
