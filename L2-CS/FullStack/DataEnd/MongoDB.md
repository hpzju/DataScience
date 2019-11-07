---
id: MongoDB
title: MongoDB a document database with the scalability and flexibility
sidebar_label: MongoDB
---

## Overview

### Learning Resources

- [MongoDB Website](https://www.mongodb.com)
- [MongoDB Documentation](https://docs.mongodb.com/)
- [MongoDB in Action](https://www.google.com)
- [MongoDB the definitive guide](https://www.google.com)

---

## Application

### Features

- Document data model
  - Denomalization
  - Aggregation
- Schema-less design
- Ad hoc Query
  - Key-Value database lack of this
- [Indexing](#indexing)
  - B-Tree based algorithm
  - Primary Key is system built-in
  - Secondary keys can support business cases query
- [Sharding](#sharding)
  - scale-out
- [Replication](#replication)
  - replica sets accross mulitple servers
  - 1 Primary + 1 or more Secondary
- Speed and Durability
  - user configurable writeconcern
  - fire-and-forget mode
  - atomic mode
- Scalling
  - scale-out using sharding
- JSON-friendly database
- file stroage with GridFS

### Applicable Scenarios

- Web Application
- Agile DevOps
- Analytics and Logging
- Caching with Query
- Variable Schema

---

## Architecture

### Data Model

- Feilds
- Values
- Documents
- Collections
- Databases
  - namespace and physical group of collections and their indexes

### System Model

- Core Server
  - mongod
    - standalone
    - replica set
  - mongos
    - sharding/replica router
- Java Shell
  - mongo
- [Database Drivers](#drivers)
  - Python, Javascript, Java, C, C++...
- System Tools
  - mongodump/mongorestore
  - bsondump
  - mongoexport/mongoimport
  - mongostat/mongotop
  - mongofiles

### Structures

### CRUD

- Create
  - [db.createCollection(name, options)](https://docs.mongodb.com/manual/reference/method/db.createCollection/index.html)
    - capped collection
    - TTL collection
  - [db.collection.insert()](https://docs.mongodb.com/manual/reference/method/db.collection.insert/)
  - `db.users.insert([{"userame": "jone2"},{userame: "jane"}])`
  - [db.collection.save()](https://docs.mongodb.com/manual/reference/method/db.collection.save/)
- Read
  - [db.collection.findOne()](https://docs.mongodb.com/manual/reference/method/db.collection.findOne/)
  - [db.collection.find()](https://docs.mongodb.com/manual/reference/method/db.collection.find/)
- Update

  - [db.collection.renameCollections()](https://docs.mongodb.com/manual/reference/method/db.collection.renameCollection/)
  - [db.collection.update()](https://docs.mongodb.com/manual/reference/method/db.collection.update/)
    - update operator
      - [Link](https://docs.mongodb.com/manual/reference/operator/update/#id1)
    - upsert
      - insert new document if not exist during update operate

- Delete
  - `use DATABASE`
  - `db.dropDatabase()`
  - `db.COLLECTION.drop()`
  - [db.collection.remove()](https://docs.mongodb.com/manual/reference/method/db.collection.remove/)
    - `db.COLLECTION.remove({})`
  - [db.collecction.deleteOne()](https://docs.mongodb.com/manual/reference/method/db.collection.deleteOne/)
  - [db.collecction.deleteMany()](https://docs.mongodb.com/manual/reference/method/db.collection.deleteMany/)

### Query

- [db.collection.findOne()](https://docs.mongodb.com/manual/reference/method/db.collection.findOne/index.html)
- [db.collection.find()](https://docs.mongodb.com/manual/reference/method/db.collection.find/)
  - `db.postalCodes.find({}, {_id:0, city:1, state:1, pincode:1}).skip(90). limit(10)`
  - `db.postalCodes.find({state:'Gujarat'},{_id:0, city:1, state:1, pincode:1}).sort({city:1}).limit(10)`
- projections
  - inclusive or exclusive
    - '\_id' can be mixed
  - \$slice
    - `db.products.find({}, {'reviews': {$slice: -5}})`
- sort, skip, limit
- dot(.) for subdocument/array field
  - `db.products.find({'details.manufacturer.id': 42848323})`
  - `db.products.find({'tags.0': "soil"})`
- logical operator
  - $and, $or, $xor, $not
    - `{ $and: [ { <expression1> }, { <expression2> } , ... , { <expressionN> } ] }`
    - `{ field: { $not: { <operator-expression> } } }`
- comparison operator
  - $lt, $gt, $lte, $gte, \$ne, $in, $nin
    - `{field: {$gt: value} }`
    - `{ field: { $in: [<value1>, <value2>, ... <valueN> ] } }`
- element operator
  - \$exist,
    - `{ field: { $exists: <boolean> } }`
  - \$type
    - `{ field: { $type: <BSON type> } }`
    - [BSON Types](https://docs.mongodb.com/manual/reference/bson-types/)
- evaluation operator
  - \$regex
    - `{ <field>: { $regex: /pattern/, $options: '<options>' } }`
    - `{ <field>: { $regex: 'pattern', $options: '<options>' } }`
    - `{ <field>: { $regex: /pattern/<options> } }`
  - \$expr
  - \$jsonSchema
  - \$mod
  - \$text
  - \$where
- array operator
  - \$all
  - \$size
  - \$elemMatch
- bitwise operator
  - bitsAllClear, bitsAllSet, bitsAnyClear, bitsAnySet
- geospatial

### Aggregation pipeline

- [db.collection.aggregate()](https://docs.mongodb.com/manual/reference/method/db.collection.aggregate/#db.collection.aggregate)
  - `db.collection.aggregate(pipeline, options)`
- pipeline stages -$addField, $bucket, $bucketAuto, $collStats, $count, $facet, $geoNear, $graphLookup, $group, $indexStats, $limit, $listSessions, $lookup, $match, $out, $project, $redact, $ replaceRoot, $sample, $skip, $sort, $sourtByCount, \$unwind
  - [Link](https://docs.mongodb.com/manual/reference/operator/aggregation-pipeline/)
- pipeline operators
  - [Link](https://docs.mongodb.com/manual/reference/operator/aggregation/)

### Indexing

- [Indexing Concepts](https://docs.mongodb.com/manual/indexes/index.html)

  - single-key indexes
  - compund-key indexes
  - unique indexes
  - sparse indexes
  - multikey indexes
  - hashed indexes
  - geospacial indexes

- Indexing principle and practice

  - single-key indexing
    - exact match
    - sorting
    - range queries
  - compund-key indexes
    - exact matches
    - range matches
    - covering indexes
  - hint()

- [Index functions](https://docs.mongodb.com/manual/reference/method/db.collection.createIndex/)
  - Show Index
    - `db.sparseTest.getIndexes()`
  - Foregroud Indexing
    - `db.indexTest.createIndex({value:1})`
  - Backgroud Indexing
    - `db.indexTest.createIndex({value:1}, {background:true})`
  - Sparse Indexing
    - `db.sparseTest.createIndex({y:1}, {unique:1, sparse:1})`
  - hind() forced full scan
    - `db.sparseTest.find({y:{$ne:2}}, {_id:0}).hint({y:1}).limit(15)`
  - TTL Indexing
    - `db.ttlTest.createIndex({createDate:1}, {expireAfterSeconds:300})`
  - Defragmentation
    - `db.indexTest.reIndex()`
  - Drop Index
    - `db.indexTest.dropIndex("INDEXNAME")`

### Sharding and Cluster

- Architecture
  - ![Alt](/img/MongoDB-Sharding-01.png "mongodb architecture")
  - ![Alt](/img/MongoDB-Sharding-02.png "mongodb granularity of sharding")
- Config [TODO]
  - `use test`
  - `sh.enableSharding('test')`
  - `sh.status()`

### Replication and Cluster [TODO]

- Concepts
  - replica set
- Setup
- Manage

  - rs.status()
  - `rs.stepDown()`

### GridFS [TODO]

### Atomicity, Concurrency, and Transaction

- [TODO](https://docs.mongodb.com/manual/core/write-operations-atomicity/index.html)

### Text Searching [TODO]

### Stroage Engine

- [Link](https://docs.mongodb.com/manual/core/storage-engines/)
- MMAPv1 Engine
- In-Memory Engind
- WiredTiger Plugable Storage [TODO]

### Business Continuity [TODO]

---

## Management

### OS Shell CMD

- login

  - `mongo --host HOST -u admin -p admin --authenticationDatabase admin DATABASE`
  - `use admin; db.auto({user: USER, pwd: PASSWD})`

- logout

  - `db.logout()`

- status

  - `mongostat`
  - `mongotop 10`

- renaming collection

  - `db.sloppyNamedCollection.renameCollection('neatNamedCollection')`
  - `db.sloppyNamedCollection.renameCollection('neatNamedCollection', true)`
  - `db.runCommand({ renameCollection: "test.sloppyNamedCollection ", to: " newDatabase.neatNamedCollection", dropTarget: true })`

### Backup

- bsondump
  - `bsondump user.bson`
  - `bsondump --type=debug user.bson`
- mongodump/mongorestore
  - `mongodump -o DIR`
  - `mongorestore DIR`
  - `mongodump -h HOST -p PORT -u USER -p PASSWD -o DIR`
  - `mongorestore -h HOST -p PORT -u USER -p PASSWD DIR`
- data-file based backup

  - ```bash
    use admin
    db.fsyncLock()
    # copy MongoDB's data files
    db.fsyncUnlock()
    ```

### Monitoring and Diagonose

- db
  - `show dbs`
  - `db.stats(1024)`
  - `db.serverStatus()`
  - `db.currentOp()`
  - `db.runCommand({top:1})`
  - `use DATABASE`
  - `db`
- collection
  - `use DATABASE`
  - `show collections`
  - `db.postalCodes.stats(1024)`
- execution

  - [cursor.explain()](https://docs.mongodb.com/manual/reference/method/cursor.explain/index.html)
    - `db.producnts.find({"name": "cases"}).explain("executionStats")`
  - [db.runCommand()](https://docs.mongodb.com/manual/reference/method/db.runCommand/#db.runCommand)

- logging

  - ```Javascript
    use admin
    db.runCommand({ logrotate: 1 })
    ```

  - `db.getProfilingLevel()`
  - `db.setProfilingLevel(1, 50)`
  - `db.system.profile.find().pretty()`

### [User Admin](https://docs.mongodb.com/manual/reference/method/db.createUser/)

- `use admin`
- `db.createUser({ user:'admin', pwd:'admin', customData:{desc:'The admin user for admin db'}, roles:['readWrite', 'dbAdmin', 'clusterAdmin']})`
- `use test`
- `db.createUser({ user:'read_user', pwd:'read_user', customData:{desc:'The read only user for test database'}, roles:['read']})`
- `db.createUser({ user:'write_user', pwd:'write_user', customData:{desc:'The read write user for test database'}, roles:['readWrite']})`
- `db.auth('read_user', 'read_user')`
- `db.logout()`
- `db.auth({user:'write_user', pwd:'write_user'})`
- `db.dropUser("read_user")`
- `db.dropUser("write_user")`

---

## Best Practice

### Install on Win/Linux

- Please refer to [MongoDB Documentation Site](https://docs.mongodb.com/manual/administration/install-community/)

### [Install with docker](https://hub.docker.com/_/mongo?tab=description)

- `docker image pull mongo`
- `docker container run --name myMongoDB -p 27017:27017 -v mongodb_data:/data/db -e MONGO_INITDB_ROOT_USERNAME=mongoadmin -e MONGO_INITDB_ROOT_PASSWORD=secret -d mongo`
- `docker exec -it myMongoDB bash`

### Install with docker-compose

- `sudo docker-compose up -d`
- `sudo docker-compose down`
- docker-compose.yml

  ```yaml
  # Use root/example as user/password credentials
  version: "3.5"

  services:
    mongo:
      image: mongo
      container_name: myMongoDB
      restart: always
      ports:
        - 27017:27017
      volumes:
        - mongodb_data:/data/db
      environment:
        MONGO_INITDB_ROOT_USERNAME: root
        MONGO_INITDB_ROOT_PASSWORD: example

  volumes: mongodb_data:
  ```

### Drivers

- Pymongo

  ```python
  import pymongo
  import sys

  # establish a connection to the database
  client = pymongo.MongoClient("mongodb://localhost")

  # get a handle to the school database
  db_school = client.school
  col_grades = db_school.grades

  def find():

      print("find, reporting for duty")

      query = {'type': "exam"}
      selector = {'student_id': 1, '_id': 0, 'score': 1}

      try:
          cursor = col_grades.find(query, selector).skip(30).limit(10)
      except:
          print("Unexpected error:", sys.exc_info()[0])

      sanity = 0
      for doc in cursor:
          print(doc)
          sanity += 1
          if (sanity > 10):
              break

  def main():
      find()

  if __name__ == "__main__":
      main()
  ```

- JavaScript Mongo

  ```javascript
  const MongoClient = require("mongodb").MongoClient;
  const assert = require("assert");

  (async function() {
    // Connection URL
    const url = "mongodb://localhost:27017/learn";
    // Database Name
    const dbName = "learn";
    const client = new MongoClient(url, { useNewUrlParser: true });

    try {
      // Use connect method to connect to the Server
      await client.connect();
      console.log("******Connected successfully to server");

      const db = client.db(dbName);
    } catch (err) {
      console.log(err.stack);
    }

    client.close();
    console.log("******Closed successfully to server");
  })();
  ```
