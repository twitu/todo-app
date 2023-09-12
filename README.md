# todo-app
1. Install Stack [Installation Guide](https://docs.haskellstack.org/en/stable/)
2. DB setup queries are present in [this file](https://github.com/adiR28/todo-app/blob/main/todo-app/DB-Queries.sql)
3. environment variables for redis -:
    1)  REDIS_HOST
    2) REDIS_PORT
    3) REDIS_PASSWORD_REQ
    4) REDIS_AUTH
    5) REDIS_CLUSTERED_ENABLED
4. environment variables for DB -:
    1) DB_HOST
    2) DB_PORT
    3) DB_USER
    4) DB_PASSWORD
    5) DB_NAME
5. Application Environmment
   1) PORT
6. stack build
7. stack run

# Dummy Server
1. Install Flask
2. Run `flask --app <dummyCallBack.py location in repo> run`

Note -:  Postgres@12 and redis@6 should be running locally
