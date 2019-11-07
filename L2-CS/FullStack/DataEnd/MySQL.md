---
id: MySQL
title: MySQL a relation database system
sidebar_label: MySQL
---

## Overview

### Application

- OLAP

  - ACID
    - Atomicity
    - Consistancy
    - Isolation
    - Durablility

- OLTP

- CRUD

### Features

- ISO/IEC 9075 SQL Standard
- ACID Compliance
- Functionalities
  - Stored Procedures
  - Triggles
  - Cursors
  - Updated Views
  - Query Cache
  - Subselect

### RDB Theory

- [Normalized Design](https://en.wikipedia.org/wiki/Database_normalization)
  - Purpose:
    - minimize redundancy
    - minimize dependency
  - 1NF:
    - single value for each Field(Column, Attribute)
      - vilation of 1NF: put mobile phone and home phone into phone field
  - 2NF:
    - 1NF compliance
    - `every non-prime attribute` of the relation is dependent on `the whole of every candidate key`
  - 3NF:
    - 2NF compliance
    - no non-prime attribute depends on other non-prime attributes. All the non-prime attributes must depend on the primary key only
- SQL
  - DDL
    - schema
    - primary key: uniqu column/columns
      - surrogate key: generated primary key, no business value
    - foreign key
      - o2o
      - o2m
      - m2m
    - `CREATE, ALTER, RENAME, DROP, TRUNCATE`
  - DML
    - crud
    - `UPDATE, DELETE, INSERT`
  - DQL
    - `SELECT, SHOW, HELP`
  - DCL
    - Role based permission
    - `GRANT, REVOKE`
  - DTL/TCL
    - transaction
    - `START TRANSACTION, SAVEPOINT, COMMIT, ROLLBACK`
- RDBMS
  - Data Definition
  - Data Manipulation
  - Data Retrival
  - System Administration
    - User, Role, Previlege
    - Data Store
    - System Mon
    - Backup
    - Audit

---

---

## Architecture

### Data Model

- Types

  - Numeric Types

    ```sql
    SMALLINT;
    TINYINT;
    MEDIUMINT;
    INT;
    BIGINT;
    DECIMAL(<width>,<prec>);
    FLOAT;
    DOUBLE;
    BIT;
    NUMERIC;
    ```

  - Date Types

    ```sql
    DATE;
    TIME;
    YEAR;
    DATETIME;
    TIMESTAMP;
    ```

  - String Types

    ```sql
    CHAR(<0,255>);
    VARCHAR(<0,255>);
    TINYTEXT;
    MEDIUMTEXT;
    TEXT;
    LONGTEXT;

    BINARY;
    VARBINARY;
    TINYBLOB;
    MEDIUMBLOB
    BLOB;
    LONGBLOB

    ENUM;
    temperature ENUM('COLD','HOT') DEFUALT 'COLD'

    SET
    ```

  - Spatial

  - JSON

### Building Blocks

- Database/Schema

  ```sql
  SHOW DATABASES;

  USE mydb;
  SELECT DATABASE();
  ```

  - system dbs
    - `mysql; sys; information_schema; performance_schema;`

- Table

  ```sql
  SHOW TABLES;
  SHOW CREATE TABLE mytable;
  SHOW TABLES FROM mydb;
  DESC mytable;

  PRIMARY KEY
  UNIQUE KEY
  FOREIGN KEY
  ```

- View

  ```sql
  SHOW DATABASES;
  ```

- Stored Procedure

  ```sql
  SHOW DATABASES;
  ```

- Function and Operators

  ```sql
  COUNT( [DISTINCT] *);

  SELECT SUM(amount) as 'sum', AVG(amount) as 'avg' from mytable;

  SELECT DATABASE(), VERSION(), USER();

  SELECT CURRENT_DATE(), CURDATE(), CURRENT_TIME(), CURTIME(), NOW(), SYSDATE(),
  CURRENT_TIMESTAMP(), UTC_TIMESTAMP(), DATE_FORMAT(date, formatter),
  SELECT DATADIFF('1923-09-01', '2019-02-10'), DATE_ADD(), DATE_SUB(),
  SELECT FROM_DAYS(2323);
  SELECT SLEEP(1)
  YEAR(), DAY(), MONTH(),

  -- arithmetic
  +, -, *, /, DIV, %
  POW(), SQRT(), PI(),
  ABS(), CEILING(), FLOOR(),
  LOG10(), LOG2(), EXP(),

  -- string
  UPPER(), LOWER(),
  REVERSE(), REPEAT(), REPLACE(),
  CONCAT(), CONCAT_WS(), SUBSTRING(),
  LEFT(str, len), RIGHT(str, len),
  LENGTH(), CHAR_LENGTH()

  --boolean
  >, >=, <, <=, !=, <>, =,
  &&, ||, !,
  AND, OR, NOT, XOR,
  IS NULL, IS NOT NULL,
  IN (a, b, c,...)
  LIKE '%', LIKE '_', NOT LIKE,
  BETWEEN a AND b,
  NOT BETWEEN a AND b,

  CAST(). IFNULL(),

  -- aggregation
  COUNT(),MIN(), MAX(), AVG(), SUM(),
  ```

- Row/Record

  ```sql
  SELECT
  ```

- Column/Field/Attribute

  ```sql
  SHOW COLUMNS FROM mytable;

  SHOW COLUMNS FROM mydb.mytable;

  DESC mytable;
  ```

---

### System Model

- Constrains

  ```sql
  NOT NULL
  UNIQUE
  CHECK
  DEFAULT
  INDEX
  ```

- Storage Procedure

- Trigger

### CRUD

#### Create

```sql
CREATE DATABASE IF NOT EXISTS mydatabase DEFAULT CHARACTER SET utf8mb4;

CREATE TABLE people (
  first_name VARCHAR(50) NOT NULL DEFAULT '',
  last_name VARCHAR(50) NOT NULL DEFAULT '',
  mobile_no VARCHAR(20) NOT NULL DEFAULT '',
  birthday DATE DEFAULT NULL,
  PRIMARY KEY (first_name, last_name),
  UNIQUE KEY mobile_no (mobile_no)
  )
  ENGINE=InnoDB DEFAULT CHARACTER SET utf8mb4;

CREATE TABLE IF NOT EXISTS people (
  id INT NOT NULL AUTO_INCREMENT,
  first_name VARCHAR(50) NOT NULL DEFAULT '',
  last_name VARCHAR(50) NOT NULL DEFAULT '',
  mobile_no VARCHAR(20) NOT NULL DEFAULT '',
  birthday DATE DEFAULT NULL,
  PRIMARY KEY (id),
  UNIQUE KEY mobile_no (mobile_no)
  )
  ENGINE=InnoDB DEFAULT CHARACTER SET utf8mb4;

SHOW CREATE TABLE people;

 create table user (
   id int not null auto_increment,
   last_name VARCHAR(50) not null default "",
   first_name VARCHAR(50) not null default "",
   middle_name VARCHAR(50) default "",
   age int not null,
   current_status VARCHAR(20) not null default 'employed',
   primary key (id)
   )
   engine = innodb default character set utf8mb4;
```

#### Read

```sql
SELECT * FROM mytable;

SELECT col1, col2, ... FROM mytable;

SELECT [DISTINCT] mytab.mycol as newcol, ...
  FROM mytab,...
  WHERE filter_expr
```

#### Update

```sql
ALTER TABLE people ADD COLUMN id INT UNSIGNED DEFAULT NULL;

INSERT INTO  people (first_name, last_name, birthday, mobile_no)
  VALUES ('Bob', 'Thompson', '1993-01-12', '2323-2321-23'),
          ('Tim', 'Bell', '1954-01-12', '2323-2311-23');

INSERT INTO  people
  SET first_name='Jack', last_name='Paul',
  birthday = '1942-02-21', mobile_no='2323-4533-22';

INSERT INTO  people (first_name, last_name, birthday, mobile_no)
  VALUES ('Bob', 'Thompson', '1993-01-12', '2323-2321-23'),
          ('Tim', 'Bell', '1954-01-12', '2323-2311-23')
  ON DUPLICATE KEY UPDATE mobile_no=VALUES(mobile_no);

INSERT IGNORE INTO  people (first_name, last_name, birthday, mobile_no)
  VALUES ('Bob', 'Thompson', '1993-01-12', '2323-2321-23'),
          ('TimR', 'BellR', '1954-01-12', '2323-2311-23-1');

REPLACE INTO  people (first_name, last_name, birthday, mobile_no)
  VALUES ('Bob', 'Thompson', '1993-01-12', '2323-2321-23'),
          ('TimR', 'BellR', '1954-01-12', '2323-2311-23-1');

UPDATE people
  SET birthday='1980-02-42', ...
  WHERE first_name='Jack' AND last_name='Paul';
```

#### Delete

```sql
DROP DATABASE IF EXISTS mydb;

DROP TABLE IF EXISTS people;

TRUNCATE TABLE people;

DELETE FROM people;

DELETE FROM people
  WHERE first_name='Bob';

```

### Query

#### [SELECT](https://dev.mysql.com/doc/refman/8.0/en/select.html)

- ```SQL
  SELECT      col_express, col2_express...
    FROM        table_name
    WHERE       condition_express
    GROUP BY    {col_name | expr | position}
    HAVING      having_express
    ORDER BY    col_order_express, col2_order_express
    LIMIT       lines_num;
  ```

##### col_express

- wildcard \* for all column.
- AS
  - `SELECT col_express AS alias ...`
- `DISTINCT( col1 )` for count unique matched rows.
- aggregation
  - COUNT(), AVG(), MAX(), MIN()
  - `COUNT(DISTINCT col1)`

##### condition_express

- =, !=, >, >=, <, <=
- AND, OR, NOT
- BETWEEN, NOT BETWEEN
  - `value BETWEEN low_bound AND high_bound`
- IN, NOT IN
  - `value IN ( VALUE_SET )`
- LIKE, NOT LIKE
  - `%`: any sequence of chars
  - `_`: any one char
  - `str_val like "%PATTERN%"`
  - `%\_%`
  - case sensitive
  - `LOWER(<key>) LIKE LOWER('%<searchpattern>%')`
- (SELECT_SUBQUERY)
  - CASE-WHEN-THEN-ELSE
  - CASE-WHEN-THEN-WHEN-THEN...-ELSE

##### having_express

- see [link](#conditionexpress)

##### col_order_expresss

- `col ASC/DESC`
  - ASC is default

#### [Join](https://dev.mysql.com/doc/refman/8.0/en/join.html)

- ```SQL
  SELECT t1.name, t2.salary
    FROM employee AS t1
    INNER JOIN info AS t2 ON t1.name = t2.name;

  SELECT *
    FROM customer
    LEFT OUTER JOIN order
    ON customer.id = order.customer_id
  ```

- INNER JOIN, FULL OUTER JOIN, LEFT OUTER JOIN, RIGHT OUTER JOIN
  - Diagram of JOINS: ![Alt](/img/MySQL-Join-01.jpg "JOIN diagram")
- Self-Join

#### [UNION](https://dev.mysql.com/doc/refman/8.0/en/union.html)

- ```SQL
  SELECT ...
  UNION [ALL | DISTINCT] SELECT ...
  [UNION [ALL | DISTINCT] SELECT ...]
  ```

### Indexing

- Unique Index

- Cluster Index

- Non Cluster Index

### Replication

### Clustering

### Business Continuity

#### Backup and Restore

- Restore Schema

### Mornitoring

---

---

## Best Practice

### Install and Config

- Install

  - [Windows](https://dev.mysql.com/doc/refman/8.0/en/windows-installation.html)
  - [Ubuntu Linux](https://dev.mysql.com/doc/mysql-apt-repo-quick-guide/en/)

- Config

  - Initializing the Data Directory

    - default user `root@localhost`
    - customize user and directories
      - `mysqld --initialize --user=mysql --basedir=/opt/mysql/mysql --datadir=/opt/mysql/mysql/data`
    - alter users

      ```sql
      ALTER USER 'root'@'localhost' IDENTIFIED BY 'ROOTPASSWORD';
      CREATE USER 'root'@'127.0.0.1' IDENTIFIED BY 'ROOTPASSWORD';
      CREATE USER 'root'@'::1' IDENTIFIED BY 'ROOTPASSWORD';
      ```

  - Start, Check and Stage Service

    - Ubuntu Linux

      ```bash
      sudo systemctl start mysql
      sudo systemctl status mysql
      sudo systemctl enable mysql

      sudo mysqladmin version
      sudo mysqladmin variables

      sudo mysqlshow
      sudo mysqlshow MYDATABASE
      ```

  - Secure Init Account

    ```sql
    mysql -u root --skip-password

    ALTER USER 'root'@'localhost' IDENTIFIED BY 'NEWROOTPASSWORD';
    CREATE USER 'root'@'127.0.0.1' IDENTIFIED BY 'ROOTPASSWORD';
    CREATE USER 'root'@'::1' IDENTIFIED BY 'ROOTPASSWORD';
    ```

- Upgrade

  - [Documentation](https://dev.mysql.com/doc/refman/8.0/en/upgrading.html)

### MySQL Shell

- login

  - `\c USER@HOST`
  - `\source msyql.sql`
  - `\py`
  - `\js`
  - `\sql`

  ```bash
  mysql -h HOST -u USER -p
  ```

- ssl

  ```sql
  SHOW GLOBAL VARIABLES LIKE 'tls_version';
  SHOW GLOBAL VARIABLES LIKE '%ssl%'

  SHOW SESSION STATUS LIKE 'Ssl_version';
  SHOW SESSION STATUS LIKE 'Ssl_cipher';
  SHOW SESSION VARIABLES LIKE '%ssl%'

  SET [GLOBAL|SESSION] var_name = expr
  ```

- dba

  ```sql
  SHOW WARNINGS;

  SHOW CHARACTER SET;

  SHOW COLLATION LIKE '%utf%'

  SELECT @@hostname;
  SELECT @@version;
  ```

### Drivers

#### MySQL Connector/Python

- install

  - `pip install mysql-connector-python`

- documentation

  - [MySQL Connector/Python](https://dev.mysql.com/doc/connector-python/en/connector-python-introduction.html)

- python driver template

  ```python
  import mysql.connector
  from mysql.connector import Error

  config = {
    'user': 'root',
    'password': 'root@MYSQL',
    'host': '127.0.0.1',
    'database': 'world',
    'raise_on_warnings': True
  }

  try:
      connection = mysql.connector.connect(**config)

      if connection.is_connected():
          db_Info = connection.get_server_info()
          print("Connected to MySQL database... MySQL Server version on ", db_Info)

          cursor = connection.cursor()
          cursor.execute("select database();")
          record = cursor.fetchone()
          print("Your connected to - ", record)

  except Error as e:
      print("Error while connecting to MySQL: ", e)

  finally:
      # closing database connection.
      if connection.is_connected():
          cursor.close()
          connection.close()
          print("MySQL connection is closed")
  ```

---

---

## Misc

### Online Resources

- [MySQL Documentation](https://dev.mysql.com/doc/refman/8.0/en/)
